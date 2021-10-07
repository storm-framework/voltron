{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Int                       ( Int64 )
import           Data.Aeson
import           Data.Text                      ( Text(..), strip)
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
  , userEmail     :: Text
  }
  deriving Generic

instance ToJSON UserNG where
  toEncoding = genericToEncoding (stripPrefix "user")

data ClassData
  = Student
     { classClass     :: Text
     , classLanguage  :: Text
     , classGrpBuffer :: Buffer
     , classAllGroups :: [Text]
     }
  | Instructor
     { classClass      :: Text
     , classLanguage   :: Text
     , classAllBuffers :: [Buffer]
     }
  deriving Generic

instance ToJSON ClassData where
  toEncoding = genericToEncoding (stripPrefix "class")

data UserData = UserData
  { userUser     :: UserNG
  , userTheme    :: Text
  , userKeyBinds :: Text
  , userClasses  :: [ClassData]
  }
  deriving Generic

instance ToJSON UserData where
  toEncoding = genericToEncoding (stripPrefix "user")

data CreateUser = CreateUser
  { crUserEmail    :: Text
  , crUserPassword :: Text
  , crUserFirst    :: Text
  , crUserLast     :: Text
  , crUserTheme    :: Text
  , crUserKeyBinds :: Text
  }
  deriving (Show, Generic)

mkCreateUser :: Text -> Text -> Text -> Text -> Text -> Text -> CreateUser
mkCreateUser email pass first last theme keyBinds =
  CreateUser (strip email) pass (strip first) (strip last) (strip theme) (strip keyBinds)

instance FromJSON CreateUser where
  parseJSON = genericParseJSON defaultOptions

data CreateClass = CreateClass
  { crClassInstitution :: Text
  , crClassName        :: Text
  , crClassInstructor  :: Text
  , crClassLanguage    :: Text
  }
  deriving (Show, Generic)


mkCreateClass :: Text -> Text -> Text -> Text -> CreateClass
mkCreateClass inst name instr lang =
  CreateClass (strip inst) (strip name) (strip instr) (strip lang)

instance FromJSON CreateClass where
  parseJSON = genericParseJSON defaultOptions

data CreateGroup = CreateGroup
  { groupClass      :: Text
  , groupName       :: Text
  , groupEditorLink :: Text
  }
  deriving (Show, Generic)

mkCreateGroup :: Text -> Text -> Text -> CreateGroup
mkCreateGroup klass name link =
  CreateGroup (strip klass) (strip name) link

instance FromJSON CreateGroup where
  parseJSON = genericParseJSON defaultOptions

data CreateEnroll = CreateEnroll
  { enrollStudent :: Text       -- ^ email of the student
  , enrollClass   :: Text       -- ^ name  of the class
  , enrollGroup   :: Text       -- ^ name  of the group
  }
  deriving (Show, Generic)

mkCreateEnroll :: Text -> Text -> Text -> CreateEnroll
mkCreateEnroll email klass group = CreateEnroll (strip email) (strip klass) (strip group)

instance FromJSON CreateEnroll where
  --parseJSON = genericParseJSON defaultOptions
  parseJSON = genericParseJSON (stripPrefix "enroll")

data EnrollStudent = EnrollStudent
  { esFirstName :: Text
  , esLastName  :: Text
  , esEmail     :: Text
  , esGroup     :: Text
  }
  deriving Generic

instance FromJSON EnrollStudent where
  parseJSON = genericParseJSON (stripPrefix "es")

instance ToJSON EnrollStudent where
  toEncoding = genericToEncoding (stripPrefix "es")

-- | An Set-Class-Language datatype that mirrors the client side version -----------------------

data ClassLangInfo = ClassLangInfo
 { cliClass    :: Text
 , cliLanguage :: Text
 }
 deriving Generic

instance FromJSON ClassLangInfo where
  parseJSON = genericParseJSON (stripPrefix "cli")


-- | An `Roster` datatype that mirrors the client side version -----------------------

data Roster = Roster
  { rosterClass    :: Text
  , rosterBuffers  :: [Buffer]
  , rosterStudents :: [EnrollStudent]
  }
  deriving Generic

instance FromJSON Roster where
  parseJSON = genericParseJSON (stripPrefix "roster")

-- | Payload for a login/sign-in request ---------------------------------------------

data AuthInfo = AuthInfo
  { authInfoEmailAddress :: Text
  , authInfoPassword :: Text
  }
  deriving Generic

instance FromJSON AuthInfo where
  parseJSON = genericParseJSON (stripPrefix "authInfo")

-- | Payload for a reset-password request ---------------------------------------------

data ResetInfo = ResetInfo
  { resetEmailAddress :: Text }
  deriving Generic

instance FromJSON ResetInfo where
  parseJSON = genericParseJSON (stripPrefix "reset")

data ResetPassInfo = ResetPassInfo
  { resetPassEmail    :: Text
  , resetPassPassword :: Text
  , resetPassCode     :: Text
  }
  deriving Generic

instance FromJSON ResetPassInfo where
  parseJSON = genericParseJSON (stripPrefix "resetPass")
