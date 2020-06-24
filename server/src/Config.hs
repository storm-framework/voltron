{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}


module Config where

import           System.Console.CmdArgs
import           Frankie                        ( HostPreference
                                                , Port
                                                )
import qualified Data.Text                     as T
import           Data.Maybe
import           GHC.Exts                       ( fromString )
import           System.Console.CmdArgs
import           System.Environment
import           Server
import qualified Auth                      

voltronModes :: Voltron
voltronModes = modes 
  [ modeServer        &= auto
  , modeAddInstructor &= explicit &= name "add-instructor"
  ]

data Voltron
  = Server 
    { port   :: Port
    , host   :: String
    , static :: Maybe String
    , pool   :: Int
    , db     :: T.Text
    }
  -- for bootstrapping / debugging
  | AddInstructor 
    { email    :: T.Text
    , password :: T.Text
    , db       :: T.Text
    }
  | AddGroup 
    { grpName    :: T.Text
    , editorLink :: T.Text  
    , db         :: T.Text
    }
  | AddStudent 
    { email    :: T.Text
    , password :: T.Text 
    , group    :: T.Text 
    , db       :: T.Text 
    }
  deriving (Data, Typeable, Show)

modeServer :: Voltron
modeServer = Server
  { port   = 3000 &= typ "PORT" 
                  &= help "The port to bind to (default 3000)"
  , host   = "127.0.0.1" 
                  &= typ "HOST" 
                  &= help "The interface to bind to (default 127.0.0.1)"
  , pool   = 1    &= typ "SIZE" 
                  &= help "Sql Backend pool size (default 1)"
  , static = def  &= typ "PATH" 
                  &= help "If specified serve any unknown route from this directory"
  , db     = "db.sqlite" 
                  &= typ "PATH" 
                  &= help "Database path (default db.sqlite)"
  }

modeAddInstructor :: Voltron
modeAddInstructor = AddInstructor
  { email    = "" &= typ "EMAIL"
  , password = "" &= typ "PASSWORD"
  , db       = "db.sqlite" 
                  &= typ "PATH" 
                  &= help "Database path (default db.sqlite)"
  }

modeAddGroup :: Voltron
modeAddGroup = AddGroup
  { grpName   = "0" &= typ  "STRING" 
                 &= help "The (unique) string identifier for a group"
  , editorLink = "-123" 
                 &= typ "STRING"
                 &= help "The hash identifier for the firepad editor buffer"
  , db       = "db.sqlite" 
                 &= typ "PATH" 
                 &= help "Database path (default db.sqlite)"
  }

modeAddStudent = AddStudent
  { email    = "" &= typ "EMAIL"
  , password = "" &= typ "PASSWORD"
  , group    = "" &= typ "STRING"
  , db       = "db.sqlite" 
                  &= typ "PATH" 
                  &= help "Database path (default db.sqlite)"
  }
