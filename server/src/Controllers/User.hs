{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

import           Data.Text                      ( Text, pack )
import           GHC.Generics

import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Frankie
import           Binah.SMTP
import           Binah.JSON

import           Controllers
import           Model
import           JSON
import           Types

import qualified Debug.Trace

----------------------------------------------------------------------------------------------------
-- | User List
----------------------------------------------------------------------------------------------------

{-@ extractUserNG :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
extractUserNG :: Entity User -> Controller UserNG
extractUserNG u = do
  firstName <- project userFirstName' u
  lastName  <- project userLastName' u
  return     $ UserNG firstName lastName

{-@ extractUserData :: u:_ -> TaggedT<{\v -> u == v}, {\_ -> False}> _ _ _ @-}
extractUserData :: Entity User -> Controller UserData
extractUserData u = do
  emailAddress <- project userEmailAddress' u
  uNG          <- extractUserNG u
  theme        <- project userTheme' u
  keyBinds     <- project userKeyBinds' u
  classes      <- extractUserClasses u
  return $ UserData uNG theme keyBinds classes

{-@ extractUserClasses :: u:_ -> TaggedT<{\v -> u == v}, {\_ -> False}> _ _ _ @-}
extractUserClasses :: Entity User -> Controller [ClassData]
extractUserClasses u = do
  instrClasses   <- extractInstrClasses u
  studentClasses <- extractStudentClasses u
  return (instrClasses ++ studentClasses)

{-@ extractInstrClasses :: u:_ -> TaggedT<{\v -> u == v}, {\_ -> False}> _ _ _ @-}
extractInstrClasses :: Entity User -> Controller [ClassData]
extractInstrClasses u = do
  uId        <- project userId' u
  classes    <- selectList (classInstructor' ==. uId)
  mapT extractInstrData classes

{-@ extractInstrData :: c: _ -> TaggedT<{\v -> IsInstructorC c v}, {\_ -> False}> _ _ _ @-}
extractInstrData :: Entity Class -> Controller ClassData
extractInstrData cls = do
  clsId     <- project classId' cls
  clsName   <- project className' cls
  clsLang   <- project classEditorLang' cls
  allGroups <- selectList (groupClass' ==. clsId)
  allBufs   <- mapT (extractBuffer clsName False) allGroups
  return (Instructor clsName clsLang allBufs)

{-@ extractStudentClasses :: u:_ -> TaggedT<{\v -> u == v}, {\_ -> False}> _ _ _ @-}
extractStudentClasses :: Entity User -> Controller [ClassData]
extractStudentClasses u = do
  uId       <- project userId' u
  uEnrolls  <- selectList (enrollStudent' ==. uId)
  mapMaybeT enrollClassData uEnrolls

{-@ enrollClassData :: e:_ -> TaggedT<{\v -> IsInGroupE e v}, {\_ -> False}> _ _ _ @-}
enrollClassData :: Entity Enroll -> Controller (Maybe ClassData)
enrollClassData enroll = do
  grpId   <- project enrollGroup' enroll
  clsId   <- project enrollClass' enroll
  grp     <- selectFirst (groupId' ==. grpId &&: groupClass' ==. clsId)
  cls     <- selectFirst (classId' ==. clsId)
  case (grp, cls) of
    (Just grp, Just cls) -> do
      clsName <- project className' cls
      lang    <- project classEditorLang' cls
      grpBuf  <- extractBuffer clsName True grp
      return $ Just (Student clsName lang grpBuf)
    _ -> return Nothing

{-@ extractBuffer :: Text -> Bool -> g: (Entity Group) -> 
                     TaggedT<{\v -> IsInstructorG g v || IsInGroupG g v}, {\_ -> False}> _ _ _ 
  @-}
extractBuffer :: Text -> Bool -> Entity Group -> Controller Buffer
extractBuffer clsName isStudent group = do
  bName <- project groupName' group
  bHash <- project groupEditorLink' group
  let bText = "-- Code for group: " <> bName
  let bDiv  = "editor-" <> clsName <> "-" <> bName
  let bDiv' = if isStudent then bDiv <> "-" <> "student" else bDiv
  return $ Buffer bName bHash bText bDiv'


traceShow :: (Show a) => String -> a -> a
traceShow msg x = Debug.Trace.trace (msg <> ": " <> (show x)) x

----------------------------------------------------------------------------------------------------
-- | User Get
----------------------------------------------------------------------------------------------------

{-@ userGetMe :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
userGetMe :: Controller ()
userGetMe = do
  user     <- requireAuthUser
  -- user     <- selectFirstOr notFoundJSON (userId' ==. userId)
  userData <- extractUserData user
  respondJSON status200 userData

----------------------------------------------------------------------------------------------------
-- | User Update
----------------------------------------------------------------------------------------------------

{-@ userUpdateMe :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
userUpdateMe :: Controller ()
userUpdateMe = do
  user            <- requireAuthUser
  userId          <- project userId' user
  UserUpdate {..} <- decodeBody
  let up =          (userFirstName' `assign` userUpdateFirstName)
          `combine` (userLastName' `assign` userUpdateLastName)
  _        <- updateWhere (userId' ==. userId) up
  user     <- selectFirstOr notFoundJSON (userId' ==. userId)
  userData <- extractUserData user
  respondJSON status200 userData


data UserUpdate = UserUpdate
  { userUpdateFirstName :: Text
  , userUpdateLastName :: Text
  }
  deriving Generic

instance FromJSON UserUpdate where
  parseJSON = genericParseJSON (stripPrefix "userUpdate")
