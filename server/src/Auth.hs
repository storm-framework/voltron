{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Auth where

import           Data.Aeson
import           Data.Maybe
import qualified Crypto.Hash                   as Crypto
import qualified Crypto.MAC.HMAC               as Crypto
import           Control.Monad.Time             ( MonadTime(..) )
import           Frankie.Auth
import           Database.Persist.Sql           ( fromSqlKey )
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy.Encoding       as L
import           Data.Time.Clock                ( UTCTime
                                                , secondsToDiffTime
                                                )
import qualified Data.ByteArray                as BA
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Char8         as Char8
import qualified Data.ByteString.Base64.URL    as B64Url
import qualified Data.ByteString.Lazy          as L
import           Data.Int                       ( Int64 )
import           GHC.Generics
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
import           Binah.Crypto
import           Binah.Time

import           Controllers
import           Controllers.User               ( extractUserData, extractUserNG )
import           Model
import           JSON
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
   token                          <- genToken userId
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


setSessionCookie :: SessionToken -> Response -> Response
setSessionCookie token = setCookie
  (defaultSetCookie { setCookieName   = "session"
                    , setCookieValue  = L.toStrict (encode token)
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
  cookie  <- listToMaybe <$> getCookie "session"
  case cookie >>= decode . L.fromStrict of
    Just t@SessionToken{..} -> do
      valid <- verifyToken t
      if valid then selectFirst (userId' ==. stUserId) else return Nothing
    _ -> return Nothing

-------------------------------------------------------------------------------
-- | Session Tokens
-------------------------------------------------------------------------------

data SessionToken = SessionToken
  { stUserId :: UserId
  , stTime   :: UTCTime
  , stHash   :: Text
  }
  deriving Generic

instance ToJSON SessionToken where
  toEncoding = genericToEncoding (stripPrefix "st")

instance FromJSON SessionToken where
  parseJSON = genericParseJSON (stripPrefix "st")

genToken :: UserId -> Controller SessionToken
genToken userId = do
  key  <- configSecretKey `fmap` getConfigT
  time <- currentTime
  return (SessionToken userId time (doHmac key userId time))

verifyToken :: SessionToken -> Controller Bool
verifyToken SessionToken{..} = do
  key <- configSecretKey `fmap` getConfigT
  return (doHmac key stUserId stTime == stHash)

doHmac :: ByteString -> UserId -> UTCTime -> Text
doHmac key userId time = T.decodeUtf8 . B64Url.encode . ByteString.pack $ h
  where
    t   = show time
    u   = show (fromSqlKey userId)
    msg = Char8.pack (t ++ ":" ++ u)
    h   = BA.unpack (hs256 key msg)

hs256 :: ByteString -> ByteString -> Crypto.HMAC Crypto.SHA256
hs256 = Crypto.hmac
