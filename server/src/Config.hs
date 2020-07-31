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
  [ modeServer    &= auto
  , modeAddClass  &= explicit &= name "add-class"
  , modeAddUser   &= explicit &= name "add-user"
  -- , modeAddGroup  &= explicit &= name "add-group"
  -- , modeAddEnroll &= explicit &= name "add-enroll"
  ]

data Voltron
  = Server 
    { port   :: Port
    , host   :: String
    , static :: Maybe String
    , pool   :: Int
    , db     :: T.Text
    }
  | AddClass
    { institution :: T.Text
    , className   :: T.Text
    , instructor  :: T.Text -- email
    , db          :: T.Text
    }
  | AddUser
    { email     :: T.Text
    , password  :: T.Text
    , firstName :: T.Text
    , lastName  :: T.Text
    , db        :: T.Text
    }
    deriving (Data, Typeable, Show)

  -- for bootstrapping / debugging

  --  AddGroup 
  --   { className  :: T.Text
  --   , groupName  :: T.Text
  --   , editorLink :: T.Text  -- firepad hash
  --   , db         :: T.Text
  --   }
  --  AddEnroll
  --   { student    :: T.Text  -- email
  --   , className  :: T.Text 
  --   , groupName  :: T.Text 
  --   , db         :: T.Text 
  --   }

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

modeAddClass :: Voltron
modeAddClass = AddClass
  { institution = ""  
                   &= typ "STRING"   
                   &= help "The string identifier for the host institution"
  , className   = "" 
                   &= typ  "STRING" 
                   &= help "The (unique) string identifier for a group"
  , instructor  = "" 
                   &= typ "EMAIL"
                   &= help "The email identifier for class' instructor"
  , db       = "db.sqlite" 
                   &= typ "PATH" 
                   &= help "Database path (default db.sqlite)"
  }

modeAddUser :: Voltron
modeAddUser = AddUser
  { email     = "" &= typ "EMAIL"
  , password  = "" &= typ "PASSWORD"
  , firstName = "" &= typ "STRING"
  , lastName  = "" &= typ "STRING"
  , db        = "db.sqlite" 
                  &= typ "PATH" 
                  &= help "Database path (default db.sqlite)"
  }

-- modeAddGroup :: Voltron
-- modeAddGroup = AddGroup
--   { className  = ""  
--                  &= typ "STRING"   
--                  &= help "The (unique) string identifier for a group"
--   , groupName  = "0" 
--                  &= typ  "STRING" 
--                  &= help "The (unique) string identifier for a group"
--   , editorLink = "-123" 
--                  &= typ "STRING"
--                  &= help "The hash identifier for the firepad editor buffer"
--   , db       = "db.sqlite" 
--                  &= typ "PATH" 
--                  &= help "Database path (default db.sqlite)"
--   }




-- modeAddEnroll :: Voltron
-- modeAddEnroll = AddEnroll
--   { student    = "" &= typ "EMAIL"
--   , className  = "" &= typ "STRING"
--   , groupName  = "" &= typ "STRING"
--   , db         = "db.sqlite" 
--                   &= typ "PATH" 
--                   &= help "Database path (default db.sqlite)"
--   }
