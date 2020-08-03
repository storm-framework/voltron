{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Auth where

import           Data.Aeson
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Time             ( MonadTime(..) )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad                  ( replicateM )
import           Control.Lens.Operators         ( (?~)
                                                , (^.)
                                                , (^?)
                                                , (<&>)
                                                )
import           Control.Lens.Combinators
                                         hiding ( assign )
import           Control.Lens.Lens              ( (&) )
import           Control.Lens.Internal.ByteString
                                                ( unpackLazy8 )
import           Frankie.Auth
import           Crypto.JWT
import           Crypto.JOSE.Types              ( Base64Octets(..) )
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy.Encoding       as L
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Base64.URL    as B64Url
import qualified Data.ByteString.Lazy          as L
import           Data.Int                       ( Int64 )
import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           GHC.Generics
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           Frankie.Config

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Templates
import           Binah.Frankie
import           Binah.SMTP

import           Controllers
import           Controllers.User               ( extractUserData, extractUserNG )
import           Controllers.Enroller           ( genRandomText )
import           Model
import           JSON
import           Crypto
import           Types 


--------------------------------------------------------------------------------
-- | SignIn
--------------------------------------------------------------------------------

{-@ ignore signIn @-}
signIn :: Controller ()
signIn = do
   AuthInfo emailAddress password <- decodeBody
   user                           <- authUser emailAddress password
   userId                         <- project userId' user
   token                          <- genJwt userId
   userNG                         <- extractUserNG user
   respondJSON status200 $ LoginResponse (unpackLazy8 token) userNG

{-@ ignore authUser @-}
authUser :: Text -> Text -> Controller (Entity User)
authUser emailAddress password = do
  user <- selectFirstOr (errorResponse status401 (Just "Incorrect credentials"))
                        (userEmailAddress' ==. emailAddress)
  encrypted <- project userPassword' user
  if verifyPass' (Pass (T.encodeUtf8 password)) (EncryptedPass encrypted)
    then return user
    else respondError status401 (Just "Incorrect credentials")



-------------------------------------------------------------------------------
-- | Reset password : generate a random code and send to user's email 
-------------------------------------------------------------------------------
{-@ ignore reset @-}
reset :: Controller ()
reset = do
  ResetInfo {..} <- decodeBody
 --  user           <- selectFirstOr 
 --                      (errorResponse status401 (Just "Unknown email address"))
 --                      (userEmailAddress' ==. resetInfoEmailAddress)
  _    <- updateWhere 
            (resetPasswordEmail' ==. resetEmailAddress) 
            (resetPasswordValid' `assign` False)
  code <- genRandomText
  insert (mkResetPassword resetEmailAddress code True)
  sendResetMail code resetEmailAddress
  respondJSON status200 $ "OK: Please check " <> resetEmailAddress

sendResetMail :: Text -> Text -> Controller ()
sendResetMail code userEmail = do 
  smtpConfig <- configSMTP <$> getConfig
  let subject = "VOLTRON Password Reset Code"
  let to      = mkPublicAddress (T.unpack userEmail)
  let from    = mkPublicAddress (smtpUser smtpConfig) 
  res        <- sendPlainTextMail smtpConfig to from subject (mkBody code)
  case res of 
    Right _ -> return () 
    Left _  -> respondError status401 (Just "Error sending email!")

mkBody :: Text -> LT.Text
mkBody code =
  LT.fromStrict
    $  "Please use the following code to reset your VOLTRON password: "
    <> code

-------------------------------------------------------------------------------
-- | `resetPass` actually resets the password using a previously mailed code
-------------------------------------------------------------------------------
{-@ ignore resetPass @-}
resetPass :: Controller ()
resetPass = do
  ResetPassInfo {..} <- decodeBody
  -- 1. Check if the code is valid
  _ <- selectFirstOr
        (errorResponse status403 (Just "invalid reset code"))
        ((resetPasswordCode'  ==. resetPassCode) &&: 
         (resetPasswordEmail' ==. resetPassEmail) &&: 
         (resetPasswordValid' ==. True))
  -- 2. Invalidate the code 
  _  <- updateWhere 
          (resetPasswordCode' ==. resetPassCode) 
          (resetPasswordValid' `assign` False)
  -- 3. Update the user's information
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 resetPassPassword))
  _  <- updateWhere 
          (userEmailAddress' ==. resetPassEmail)
          (userPassword' `assign` encrypted)
  respondJSON status200 ("OK: Login with new password" :: T.Text) 

-------------------------------------------------------------------------------
-- | Auth method
-------------------------------------------------------------------------------

authMethod :: AuthMethod (Entity User) Controller
authMethod = AuthMethod
  { authMethodTry     = checkIfAuth
  , authMethodRequire = checkIfAuth >>= \case
                          Just user -> pure user
                          Nothing   -> respondError status401 Nothing
  }

{-@ ignore checkIfAuth @-}
checkIfAuth :: Controller (Maybe (Entity User))
checkIfAuth = do
  authHeader <- requestHeader hAuthorization
  let token = authHeader >>= ByteString.stripPrefix "Bearer " <&> L.fromStrict
  claims <- liftTIO $ mapM doVerify token
  case claims of
    Just (Right claims) -> do
      let sub    = claims ^. claimSub ^? _Just . string
      let userId = sub <&> T.unpack >>= readMaybe <&> toSqlKey
      case userId of
        Nothing     -> return Nothing
        Just userId -> selectFirst (userId' ==. userId)
    _ -> return Nothing

-------------------------------------------------------------------------------
-- | JWT
-------------------------------------------------------------------------------

{-@ ignore genJwt @-}
genJwt :: UserId -> Controller L.ByteString
genJwt userId = do
  claims <- liftTIO $ mkClaims userId
  jwt    <- liftTIO $ doJwtSign claims
  case jwt of
    Right jwt                         -> return (encodeCompact jwt)
    Left  (JWSError                e) -> respondError status500 (Just (show e))
    Left  (JWTClaimsSetDecodeError s) -> respondError status400 (Just s)
    Left  JWTExpired                  -> respondError status401 (Just "expired token")
    Left  _                           -> respondError status401 Nothing

mkClaims :: UserId -> TIO ClaimsSet
mkClaims userId = do
  t <- currentTime
  return $ emptyClaimsSet & (claimSub ?~ uid ^. re string) & (claimIat ?~ NumericDate t)
  where uid = T.pack (show (fromSqlKey userId))

doJwtSign :: ClaimsSet -> TIO (Either JWTError SignedJWT)
doJwtSign claims = runExceptT $ do
  alg <- bestJWSAlg key
  signClaims key (newJWSHeader ((), alg)) claims

doVerify :: L.ByteString -> TIO (Either JWTError ClaimsSet)
doVerify s = runExceptT $ do
  let audCheck = const True
  s' <- decodeCompact s
  verifyClaims (defaultJWTValidationSettings audCheck) key s'

-- TODO: Read this from env
key :: JWK
key = fromOctets raw
 where
  raw :: ByteString
  raw = "\xe5L\xb7\xf6\x03|\xb6\n\x10\xd8\xb8\x96\xe2\xc4W@#W\xb4>\th*iiW\x12\x80z\x04i="
