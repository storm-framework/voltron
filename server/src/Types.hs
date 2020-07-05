{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Text                      ( Text(..) )
import           GHC.Generics
import           JSON

data Buffer = Buffer
  { bufferId   :: Text
  , bufferHash :: Text
  , bufferText :: Text
  , bufferDiv  :: Text
  }
  deriving (Show, Generic)

instance ToJSON Buffer where
  toEncoding = genericToEncoding (stripPrefix "buffer")

data UserNG = UserNG
  { userFirstName :: Text
  , userLastName  :: Text
  }
  deriving Generic

instance ToJSON UserNG where
  toEncoding = genericToEncoding (stripPrefix "user")

data ClassData
  = Student
     { classClass     :: Text
     , classGrpBuffer :: Buffer
     }
  | Instructor
     { classClass      :: Text
     , classAllBuffers :: [Buffer]
     }
  deriving Generic

instance ToJSON ClassData where
  toEncoding = genericToEncoding (stripPrefix "class")

data UserData = UserData
  { userUser    :: UserNG
  , userClasses :: [ClassData]
  }
  deriving Generic

instance ToJSON UserData where
  toEncoding = genericToEncoding (stripPrefix "user")

data LoginResponse = LoginResponse
  { respAccessToken :: String
  , respUser        :: UserData
  }
  deriving Generic

instance ToJSON LoginResponse where
  toEncoding = genericToEncoding (stripPrefix "resp")

data CreateUser = CreateUser
  { userEmail    :: Text
  , userPassword :: Text
  , userFirst    :: Text
  , userLast     :: Text
  }
  deriving Generic

instance FromJSON CreateUser where
  parseJSON = genericParseJSON defaultOptions

data CreateClass = CreateClass
  { classInstitution :: Text
  , className        :: Text
  , classInstructor  :: Text
  }
  deriving Generic

instance FromJSON CreateClass where
  parseJSON = genericParseJSON defaultOptions

data CreateGroup = CreateGroup
  { groupClass      :: Text
  , groupName       :: Text
  , groupEditorLink :: Text
  }
  deriving Generic

instance FromJSON CreateGroup where
  parseJSON = genericParseJSON defaultOptions

data CreateEnroll = CreateEnroll
  { enrollStudent :: Text
  , enrollClass   :: Text
  , enrollGroup   :: Text
  }
  deriving Generic

instance FromJSON CreateEnroll where
  parseJSON = genericParseJSON defaultOptions



