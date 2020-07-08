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

import           Controllers
import           Controllers.User               ( extractUserData, extractUserNG )
import           Controllers.Invitation         ( InvitationCode(..) )
import           Model
import           JSON
import           Crypto
import           Types 

-- Add an instructor from cmd-line ------------------------------------------
{-@ ignore addUser @-}
addUser :: CreateUser -> Task UserId
addUser (CreateUser {..}) = do
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 userPassword))
  insert $ mkUser userEmail encrypted userFirst userLast False

-- Add a class from cmd-line ------------------------------------------------
{-@ ignore addClass @-}
addClass :: CreateClass -> Task ClassId
addClass (CreateClass {..}) = do
  instrId <- lookupUserId classInstructor
  insert $ mkClass classInstitution className instrId

-- Add a group from cmd-line ------------------------------------------------
{-@ ignore addGroup @-}
addGroup :: CreateGroup -> Task GroupId
addGroup (CreateGroup {..}) = do 
  clsId <- lookupClassId groupClass
  insert $ mkGroup groupName groupEditorLink clsId 

-- Add an enroll, i.e. student to a group from cmd-line ----------------------------------------------
{-@ ignore addEnroll @-}
addEnroll :: CreateEnroll -> Task EnrollId
addEnroll (CreateEnroll {..}) = do
  studentId <- lookupUserId  enrollStudent
  groupId   <- lookupGroupId enrollGroup 
  insert     $ mkEnroll studentId groupId

lookupUserId :: Text -> Task UserId
lookupUserId email = do
  r <- selectFirstOrCrash (userEmailAddress' ==. email)
  project userId' r

lookupGroupId :: Text -> Task GroupId
lookupGroupId name = do
  r <- selectFirstOrCrash (groupName' ==. name)
  project groupId' r

lookupClassId :: Text -> Task ClassId
lookupClassId name = do
  r <- selectFirstOrCrash (className' ==. name)
  project classId' r


--------------------------------------------------------------------------------
-- | SignIn
--------------------------------------------------------------------------------

{-@ ignore signIn @-}
signIn :: Controller ()
signIn = do
   SignInReq emailAddress password <- decodeBody
   user                            <- authUser emailAddress password
   -- respondTagged $ errorResponse status401 (Just "Got USER!")
   userId                          <- project userId' user
   token                           <- genJwt userId
   -- userData                        <- extractUserData user
   userNG                          <- extractUserNG user
   -- respondTagged $ errorResponse status401 (Just "Got USER-DATA!")
   respondJSON status200 $ LoginResponse (unpackLazy8 token) userNG
   -- respondTagged $ errorResponse status401 (Just "Got JFC!")

{-@ ignore authUser @-}
authUser :: Text -> Text -> Controller (Entity User)
authUser emailAddress password = do
  user <- selectFirstOr (errorResponse status401 (Just "Incorrect credentials"))
                        (userEmailAddress' ==. emailAddress)
  encrypted <- project userPassword' user
  if verifyPass' (Pass (T.encodeUtf8 password)) (EncryptedPass encrypted)
    then return user
    else respondError status401 (Just "Incorrect credentials")

data SignInReq = SignInReq
  { signInReqEmailAddress :: Text
  , signInReqPassword :: Text
  }
  deriving Generic

instance FromJSON SignInReq where
  parseJSON = genericParseJSON (stripPrefix "signInReq")

-------------------------------------------------------------------------------
-- | SignUp
-------------------------------------------------------------------------------

{-@ ignore signUp @-}
signUp :: Controller ()
signUp = do
  (SignUpReq (InvitationCode id code) CreateUser {..}) <- decodeBody
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 userPassword))
  let user = mkUser userEmail
                    encrypted
                    userFirst
                    userLast
                    False 
  _ <- selectFirstOr
    (errorResponse status403 (Just "invalid invitation"))
    (   (invitationId' ==. id)
    &&: (invitationCode' ==. code)
    &&: (invitationEmailAddress' ==. userEmail)
    &&: (invitationAccepted' ==. False)
    )
  userId   <- insert user
  _        <- updateWhere (invitationId' ==. id) (invitationAccepted' `assign` True)
  user     <- selectFirstOr notFoundJSON (userId' ==. userId)
  token    <- genJwt userId
  userData <- extractUserNG user
  respondJSON status201 $ LoginResponse (unpackLazy8 token) userData

data SignUpReq = SignUpReq
  { signUpReqInvitationCode :: InvitationCode
  , signUpReqUser :: CreateUser
  }
  deriving Generic

instance FromJSON SignUpReq where
  parseJSON = genericParseJSON (stripPrefix "signUpReq")

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
