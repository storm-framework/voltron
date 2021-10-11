{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{-@ LIQUID "--compile-spec" @-}

module Server
    ( runServer
    , runTask'
    , initDB
    , ServerOpts(..)
    , Stage(..)
    )
where

import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend
                                                , runSqlite
                                                , runMigration
                                                , createSqlitePool
                                                )
import           System.FilePath               as P
import           System.Directory
import           System.Environment
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Network.Mime
import           Frankie.Config
import           Frankie.Auth
import qualified Frankie.Log                   as Log
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Data.Pool                      ( Pool )
import qualified Data.Pool                     as Pool
import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , MonadTransControl(..)
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Logger          ( runNoLoggingT )
import qualified Control.Concurrent.MVar       as MVar
import qualified Text.Mustache.Types           as Mustache
import           Text.Read                      ( readMaybe )
import           Data.Typeable
import           Data.Data
import           Storm.Core
import           Storm.Frankie
import           Storm.Infrastructure
import           Storm.Insert
import           Storm.Actions
import           Storm.Filters
import           Storm.JSON

import           Storm.SMTP             -- TODO: DUMMY RECURSIVE IMPORTS for LH 
import           Storm.Updates          -- TODO: DUMMY RECURSIVE IMPORTS for LH 

import           Controllers
import           Controllers.User
import           Controllers.Class
import           Controllers.Reset

import           Model
import           Auth

data Stage = Prod | Dev deriving (Data, Typeable, Show)

data ServerOpts = ServerOpts
  { optsPort   :: Port
  , optsHost   :: HostPreference
  , optsStatic :: Maybe String
  , optsPool   :: Int
  , optsDBPath :: T.Text
  }

runServer :: ServerOpts -> IO ()
runServer ServerOpts {..} = runNoLoggingT $ do
    liftIO $ initDB optsDBPath
    cfg  <- liftIO readConfig
    pool <- createSqlitePool optsDBPath optsPool
    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host optsHost
            port optsPort
            initWithT $ initFromPool cfg pool
        dispatch $ do
            post "/api/signin"         signIn
            post "/api/signout"        signOut
            post "/api/reset"          reset
            post "/api/resetpass"      resetPass
            get  "/api/user/me"        userGetMe
            post "/api/enroll"         addRoster
            post "/api/setlanguage"    setLanguage
            get  "/api/roster/:class"  getRoster
            post "/api/setgroup"       setGroup
            get  "/api/ping"           ping

            case optsStatic of
                Just path -> fallback (sendFromDirectory path "index.html")
                Nothing   -> fallback $ do
                    req <- request
                    let path = joinPath (map T.unpack (reqPathInfo req))
                    respondError status404 (Just ("Route not found: " ++ path))
        onError $ \err -> do
          logT Log.ERROR ("Yikes! voltron-server failed with " <> displayException err)
          respond $ serverError "bad bad nab" 

runTask' :: T.Text -> Task a -> IO a
runTask' dbpath task = runSqlite dbpath $ do
    cfg     <- liftIO readConfig
    backend <- ask
    liftIO . runTIO $ configure cfg (runReaderT (unTag task) backend)

readConfig :: IO Config
readConfig = Config authMethod
                <$> MVar.newMVar mempty
                <*> readSMTPConfig
                <*> readSecretKey

readSMTPConfig :: IO SMTPConfig
readSMTPConfig = do
    host <- fromMaybe "localhost" <$> lookupEnv "VOLTRON_SMTP_HOST"
    user <- fromMaybe ""          <$> lookupEnv "VOLTRON_SMTP_USER"
    pass <- fromMaybe ""          <$> lookupEnv "VOLTRON_SMTP_PASS"
    return $ SMTPConfig host user pass

readSecretKey :: IO BS.ByteString
readSecretKey = do
    secret <- fromMaybe "sb8NHmF@_-nsf*ymt!wJ3.KXmTDPsNoy" <$> lookupEnv "VOLTRON_SECRET_KEY"
    return $ T.encodeUtf8 . T.pack $ secret

initDB :: T.Text -> IO ()
initDB dbpath = runSqlite dbpath $ do
    runMigration migrateAll

-- Static files

sendFromDirectory :: FilePath -> FilePath -> Controller ()
sendFromDirectory dir fallback = do
    req <- request
    let path = dir </> joinPath (map T.unpack (reqPathInfo req))
    exists <- liftTIO . TIO $ doesFileExist path
    if exists then sendFile path else sendFile (dir </> fallback)

sendFile :: FilePath -> Controller ()
sendFile path = do
    let mime = defaultMimeLookup (T.pack path)
    content <- liftTIO . TIO . LBS.readFile $ path
    respondTagged $ Response status200 [(hContentType, mime)] content

-- TODO find a way to provide this without exposing the instance of MonadBaseControl

initFromPool :: Config
             -> Pool SqlBackend
             -> Controller ()
             -> TaggedT (Entity User) (ControllerT TIO) ()
initFromPool cfg pool = mapTaggedT run
    where run act = Pool.withResource pool $ configure cfg . runReaderT act

instance MonadBase IO TIO where
    liftBase = TIO

instance MonadBaseControl IO TIO where
    type StM TIO a = a
    liftBaseWith f = TIO (f runTIO)
    restoreM = return

instance MonadBase IO (ControllerT TIO) where
    liftBase = lift . liftBase

instance MonadBaseControl IO (ControllerT TIO) where
    type StM (ControllerT TIO) a = ControllerStatus a
    liftBaseWith f = ControllerT $ \r -> TIO $ fmap Working (f (runTIO . flip runController r))
    restoreM st = ControllerT $ \_ -> return st



