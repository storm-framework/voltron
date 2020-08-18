{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Auth where

import           Data.Aeson
import           Data.Maybe
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
import           Data.Time.Clock                ( secondsToDiffTime )
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
import           Frankie.Cookie
import qualified Frankie.Log                   as Log
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
import           Controllers.Class              ( genRandomText )
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
   respondTagged $ setSessionCookie token (jsonResponse status200 userNG)

{-@ signOut :: TaggedT<{\_ -> False}, {\_ -> True}> _ () @-}
signOut :: Controller ()
signOut = respondTagged $ expireSessionCookie (emptyResponse status201)

{-@ ignore authUser @-}
authUser :: Text -> Text -> Controller (Entity User)
authUser emailAddress password = do
  user <- selectFirstOr (errorResponse status401 (Just "Incorrect credentials"))
                        (userEmailAddress' ==. emailAddress)
  encrypted <- project userPassword' user
  if verifyPass' (Pass (T.encodeUtf8 password)) (EncryptedPass encrypted)
    then return user
    else respondError status401 (Just "Incorrect credentials")


setSessionCookie :: L.ByteString -> Response -> Response
setSessionCookie token = setCookie
  (defaultSetCookie { setCookieName   = "session"
                    , setCookieValue  = L.toStrict token
                    , setCookieMaxAge = Just $ secondsToDiffTime 604800 -- 1 week
                    }
  )

expireSessionCookie :: Response -> Response
expireSessionCookie =
  setCookie (defaultSetCookie { setCookieName = "session", setCookieMaxAge = Just 0 })

-------------------------------------------------------------------------------
-- | Auth method
-------------------------------------------------------------------------------

authMethod :: AuthMethod (Entity User) Controller
authMethod = AuthMethod
  { authMethodTry     = checkIfAuth
  , authMethodRequire = checkIfAuth >>= \case
                          Just user -> pure user
                          Nothing   -> respondTagged $ expireSessionCookie (emptyResponse status401)
  }

{-@ ignore checkIfAuth @-}
checkIfAuth :: Controller (Maybe (Entity User))
checkIfAuth = do
  key    <- configSecretKey <$> getConfig
  token  <- listToMaybe <$> getCookie "session"
  claims <- liftTIO $ mapM (doVerify key . L.fromStrict) token
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
  key    <- configSecretKey `fmap` getConfigT
  claims <- liftTIO $ mkClaims userId
  jwt    <- liftTIO $ doJwtSign key claims
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

doJwtSign :: JWK -> ClaimsSet -> TIO (Either JWTError SignedJWT)
doJwtSign key claims = runExceptT $ do
  alg <- bestJWSAlg key
  signClaims key (newJWSHeader ((), alg)) claims

doVerify :: JWK -> L.ByteString -> TIO (Either JWTError ClaimsSet)
doVerify key s = runExceptT $ do
  let audCheck = const True
  s' <- decodeCompact s
  verifyClaims (defaultJWTValidationSettings audCheck) key s'
