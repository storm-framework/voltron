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
import           Network.Socket                 ( HostName )
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

type TasCon m = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config m))
type Controller = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))

instance Frankie.Auth.HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod

type Task = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config TIO))

runTask :: Task () -> Controller ()
runTask task = do
  backend <- lift ask
  cfg     <- getConfigT
  flip mapTaggedT task $ \t -> do
    forkTIO $ configure cfg (t `runReaderT` backend)
  return ()
