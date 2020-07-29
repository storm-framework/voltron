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

instance FromJSON Buffer where
  parseJSON = genericParseJSON (stripPrefix "buffer")

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
  , respUser        :: UserNG
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
  deriving (Show, Generic)

instance FromJSON CreateUser where
  parseJSON = genericParseJSON defaultOptions

data CreateClass = CreateClass
  { classInstitution :: Text
  , className        :: Text
  , classInstructor  :: Text
  }
  deriving (Show, Generic)

instance FromJSON CreateClass where
  parseJSON = genericParseJSON defaultOptions

data CreateGroup = CreateGroup
  { groupClass      :: Text
  , groupName       :: Text
  , groupEditorLink :: Text
  }
  deriving (Show, Generic)

instance FromJSON CreateGroup where
  parseJSON = genericParseJSON defaultOptions

data CreateEnroll = CreateEnroll
  { enrollStudent :: Text       -- ^ email of the student
  , enrollClass   :: Text       -- ^ name  of the class 
  , enrollGroup   :: Text       -- ^ name  of the group
  }
  deriving (Show, Generic)

instance FromJSON CreateEnroll where
  parseJSON = genericParseJSON defaultOptions

data EnrollStudent = EnrollStudent 
  { esFirstName :: Text
  , esLastName  :: Text
  , esEmail     :: Text
  , esGroup     :: Text
  }
  deriving Generic

instance FromJSON EnrollStudent where
  parseJSON = genericParseJSON (stripPrefix "es")

-- | An `Roster` datatype that mirrors the client side version -----------------------

data Roster = Roster 
  { rosterClass    :: Text 
  , rosterBuffers  :: [Buffer] 
  , rosterStudents :: [EnrollStudent] 
  }
  deriving Generic

instance FromJSON Roster where
  parseJSON = genericParseJSON (stripPrefix "roster")


