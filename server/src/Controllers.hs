{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers where

import           Data.Aeson
import           Data.ByteString                ( ByteString )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , runReaderT
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Database.Persist.Sqlite        ( SqlBackend )
import qualified Control.Concurrent.MVar       as MVar
import qualified Text.Mustache.Types           as Mustache
import           Frankie.Auth
import           Frankie.Config

import           Binah.Actions
import           Binah.Frankie
import qualified Frankie.Log
import           Binah.Core
import           Binah.Infrastructure
import           Binah.Filters
import           Binah.Templates
import           Binah.Concurrent
import           Binah.SMTP                    as SMTP
import qualified Network.AWS                   as AWS
import qualified Network.AWS.S3                as S3
import           Network.Socket                 ( PortNumber, HostName )
import           Model

data Config = Config
  { configAuthMethod    :: !(AuthMethod (Entity User) Controller)
  , configTemplateCache :: !(MVar.MVar Mustache.TemplateCache)
  , configSMTP          :: SMTPConfig
  , configSecretKey     :: ByteString
  }

data SMTPConfig = SMTPConfig
  { smtpHost :: HostName
  , smtpUser :: SMTP.UserName
  , smtpPass :: SMTP.Password
  }

--   , configAWS :: AWSConfig
--   , configSecretKey :: JWT.JWK
--   }

-- data SMTPConfig = SMTPConfig
--   { smtpHost :: String
--   , smtpPort :: PortNumber
--   , smtpUser :: String
--   , smtpPass :: String
--   }

-- data AWSConfig = AWSConfig
--   { awsAuth :: AWS.Auth
--   , awsRegion:: AWS.Region
--   , awsBucket :: S3.BucketName
--   }

type TasCon m = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config m))
type Controller = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))


instance Frankie.Auth.HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod

instance HasTemplateCache Config where
  getTemplateCache = configTemplateCache

type Task = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config TIO))

runTask :: Task () -> Controller ()
runTask task = do
  backend <- lift ask
  cfg     <- getConfigT
  flip mapTaggedT task $ \t -> do
    forkTIO $ configure cfg (t `runReaderT` backend)
  return ()

--------------------------------------------------------------------------------
-- | Responses
--------------------------------------------------------------------------------

defaultHeaders :: ResponseHeaders
defaultHeaders = [(hContentType, "application/json")]

{-@ respondJSON :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ _ @-}
respondJSON :: ToJSON a => Status -> a -> Controller b
respondJSON status a = respondTagged (jsonResponse status a)

jsonResponse :: ToJSON a => Status -> a -> Response
jsonResponse status a = Response status defaultHeaders (encode a)

emptyResponse :: Status -> Response
emptyResponse status = Response status defaultHeaders ""


{-@ respondError :: _ -> _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ _ @-}
respondError :: Status -> Maybe String -> Controller a
respondError status error = respondTagged (errorResponse status error)

errorResponse :: Status -> Maybe String -> Response
errorResponse status error = Response status defaultHeaders (encodeError error)
 where
  encodeError Nothing  = encode $ object []
  encodeError (Just e) = encode $ object ["error" .= e]

notFoundJSON :: Response
notFoundJSON = errorResponse status404 Nothing

--------------------------------------------------------------------------------
-- | Misc
--------------------------------------------------------------------------------

hAccessControlAllowOrigin :: HeaderName
hAccessControlAllowOrigin = "Access-Control-Allow-Origin"

{-@ decodeBody :: TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ _ @-}
decodeBody :: FromJSON a => Controller a
decodeBody = do
  req  <- requestT
  body <- liftTIO $ reqBody req
  case eitherDecode body of
    Left  s -> respondError status400 (Just s)
    Right a -> return a

-- {-@ requireInstructor ::
--   u: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser}> _ {v: () | IsInstructor u}
-- @-}
-- requireInstructor :: Entity User -> Controller ()
-- requireInstructor user = do
--   level <- project userLevel' user
--   if level == "instructor" then return () else respondError status403 Nothing