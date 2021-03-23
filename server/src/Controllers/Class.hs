{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Class
  ( addUser
  , addGroup
  , addClass
  , addEnroll
  , addRoster
  , getRoster
  , setLanguage
  , genRandomText
  , sendMail
  )
where

import Control.Monad.Random ( MonadRandom(getRandoms) )
import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64.URL    as B64Url
import qualified Data.Maybe                    as Mb

import           Frankie.Config ( MonadConfig(getConfig) )
import           Storm.Core
import           Storm.Actions
import           Storm.Updates
import           Storm.Insert
import           Storm.Filters
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Frankie
import           Storm.SMTP ( publicAddress, sendMailWithLoginSTARTTLS, simpleMail' )
import           Storm.Random ()
import           Storm.Crypto
import           Storm.JSON

import qualified Frankie.Log                   as Log
import           Controllers
import           Model
import           Types
import qualified Data.Text.Lazy as LT

-------------------------------------------------------------------------------
-- | Update the language-mode used for a given class --------------------------
-------------------------------------------------------------------------------

{-@ setLanguage :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
setLanguage :: Controller ()
setLanguage = do
  instr   <- requireAuthUser
  instrId <- project userId' instr
  ClassLangInfo {..} <- decodeBody
  cls   <- selectFirstOr (errorResponse status403 Nothing)
                         (className' ==. cliClass &&: classInstructor' ==. instrId)
  clsId <- project classId' cls
  _     <- updateWhere (classId' ==. clsId)
                       (classEditorLang' `assign` cliLanguage)
  respondJSON status200 ("OK: updated language for " <> cliClass <> " to " <> cliLanguage)

-------------------------------------------------------------------------------
-- | Respond with Current [EnrollStudent] for className -----------------------
-------------------------------------------------------------------------------

{-@ getRoster :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
getRoster :: T.Text -> Controller ()
getRoster className = do
  instr   <- requireAuthUser
  instrId <- project userId' instr
  cls     <- selectFirstOr (errorResponse status403 Nothing)
                           (className' ==. className &&: classInstructor' ==. instrId)
  clsId   <- project classId' cls
  enrolls <- selectList (enrollClass' ==. clsId)
  roster  <- mapT enrollEnrollStudent enrolls
  respondJSON status200 roster

{-@ enrollEnrollStudent :: {e:(Entity Enroll) | IsInstructorE e (currentUser 0)} ->
      TaggedT<{\v -> v == currentUser 0}, {\v -> v == currentUser 0}> _ _ _ @-}
enrollEnrollStudent :: Entity Enroll -> Controller EnrollStudent
enrollEnrollStudent enroll = do
  userId  <- project enrollStudent' enroll
  groupId <- project enrollGroup' enroll
  clsId   <- project enrollClass' enroll
  user    <- selectFirstOr notFoundJSON (userId' ==. userId)
  group   <- selectFirstOr notFoundJSON (groupId' ==. groupId &&: groupClass' ==. clsId)
  EnrollStudent
    `fmap` project userFirstName'    user
    <*>    project userLastName'     user
    <*>    project userEmailAddress' user
    <*>    project groupName'        group

-------------------------------------------------------------------------------
-- | Add a full roster of students to a class using an Roster -----------------
-------------------------------------------------------------------------------

{-@ addRoster :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
addRoster :: Controller ()
addRoster = do
  instr         <- requireAuthUser
  instrId       <- project userId' instr
  r@Roster {..} <- decodeBody
  crUsers       <- mapT createUser rosterStudents

  cls   <- selectFirstOr (errorResponse status403 Nothing)
                         (className' ==. rosterClass &&: classInstructor' ==. instrId)
  clsId <- project classId' cls
  mapT addUser   crUsers
  mapT (addGroup  clsId) (rosterGroups r)
  mapT (addEnroll clsId) (rosterEnrolls r)
  getRoster rosterClass
  -- respondJSON status200 ("OK:addRoster" :: T.Text)

{-@ addGroup :: {c: ClassId | isInstructor c (entityKey (currentUser 0))} -> CreateGroup -> 
      TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
addGroup :: ClassId -> CreateGroup -> Controller (Maybe GroupId)
addGroup clsId r@(CreateGroup {..}) = do
  id <- insertMaybe (mkGroup groupName groupEditorLink clsId)
  whenT (Mb.isNothing id) (logT Log.WARNING ("addGroup: skipping duplicate group " ++ show r))
  return id

{-@ createUser :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
createUser :: EnrollStudent -> Controller CreateUser
createUser (EnrollStudent {..}) = do
  password <- genRandomText
  let crUser = mkCreateUser esEmail password esFirstName esLastName "" ""
  return crUser

{-@ addEnroll :: {c: ClassId | isInstructor c (entityKey (currentUser 0))} -> CreateEnroll -> 
                 TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
addEnroll :: ClassId -> CreateEnroll -> Controller EnrollId
addEnroll clsId r@(CreateEnroll {..}) = do
  logT Log.INFO ("addEnroll: " ++ show r)
  student   <- selectFirstOr notFoundJSON (userEmailAddress' ==. enrollStudent)
  studentId <- project userId' student
  group     <- selectFirstOr notFoundJSON (groupName' ==. enrollGroup)
  groupId   <- project groupId' group
  insert (mkEnroll studentId clsId groupId)

{-@ genRandomText :: TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
genRandomText :: Controller T.Text
genRandomText = do
  bytes <- liftTIO getRandoms
  return $ T.decodeUtf8 $ B64Url.encode $ BS.pack (take 24 bytes)

rosterGroups :: Roster -> [CreateGroup]
rosterGroups (Roster {..}) = [ g | (_, g) <- M.toList groupM ]
  where
    groupM = M.fromList [ (bufferId , group)
                          | Buffer {..} <- rosterBuffers
                          , let group    = mkCreateGroup rosterClass bufferId bufferHash
                        ]

rosterEnrolls :: Roster -> [CreateEnroll]
rosterEnrolls (Roster {..}) =
  [ mkCreateEnroll esEmail rosterClass esGroup | EnrollStudent {..} <- rosterStudents ]

-------------------------------------------------------------------------------
-- | Add a user ---------------------------------------------------------------
-------------------------------------------------------------------------------

{-@ addUser :: _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
addUser :: (MonadTIO m) => CreateUser -> TasCon m (Maybe UserId)
addUser r@(CreateUser {..}) = do
  logT Log.INFO ("addUser: " ++ show r)
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 crUserPassword))
  maybeId <- insertMaybe (mkUser crUserEmail encrypted crUserFirst crUserLast "" "" False)
  if Mb.isNothing maybeId
    then logT Log.WARNING ("addUser: skipping duplicate user " ++ show r)
    else sendWelcomeMail r
  return maybeId

sendWelcomeMail :: (MonadTIO m) => CreateUser -> TasCon m ()
sendWelcomeMail r@(CreateUser {..}) = do
  res <- sendMail welcomeSubject (welcomeBody r) crUserEmail 
  case res of
    Left err -> logT Log.ERROR ("sendWelcomeMail: " <> T.unpack err)
    Right _  -> return ()


welcomeSubject :: T.Text
welcomeSubject = "Welcome to VOLTRON!"

welcomeBody :: CreateUser -> T.Text
welcomeBody r = T.unlines 
  [ "Dear " <> crUserFirst r <> " " <> crUserLast r <> "," 
  , ""
  , "Welcome to the collaborative code-editing app Voltron!"
  , ""
  , "To begin using, please visit:" <> voltronURL
  , "Click the link to reset your password, using the email address: " <> crUserEmail r
  , "Then log in using your new password."
  , ""
  , "We hope you enjoy using Voltron!"
  , ""
  , "- voltron.sys"
  ]

voltronURL :: T.Text
voltronURL = "https://voltron.programming.systems"

-------------------------------------------------------------------------------
-- | Generic function for sending email
-------------------------------------------------------------------------------
{-@ sendMail :: _ -> _ -> _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
sendMail :: (MonadTIO m) => T.Text -> T.Text -> T.Text -> TasCon m (Either T.Text ())
sendMail subject body userEmail = do
  SMTPConfig{..} <- configSMTP <$> getConfig
  let to      = publicAddress userEmail
  let from    = publicAddress (T.pack smtpUser)
  let mail    = simpleMail' to from subject (LT.fromStrict body) 
  res        <- sendMailWithLoginSTARTTLS smtpHost smtpUser smtpPass mail
  logT Log.INFO ("send email: " ++ show res)
  case res of
    Right _ -> return (Right ())
    Left _  -> return (Left "Error sending email!")

-------------------------------------------------------------------------------
-- | Add a class --------------------------------------------------------------
-------------------------------------------------------------------------------

{-@ addClass :: {c:_ | IsAdmin (currentUser 0)} -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
addClass :: CreateClass -> Task (Maybe ClassId)
addClass r@(CreateClass {..}) = do
  logT Log.INFO ("addClass: " ++ show r)
  maybeInstr <- selectFirst (userEmailAddress' ==. crClassInstructor)
  case maybeInstr of
    Just instr -> do
      instrId <- project userId' instr
      id      <- insertMaybe (mkClass crClassInstitution crClassName instrId crClassLanguage)
      whenT (Mb.isNothing id) (logT Log.WARNING ("addClass: skipping duplicate class " ++ show r))
      return id
    Nothing -> do
      logT Log.ERROR ("addClass: cannot find user " ++ show crClassInstructor)
      return Nothing
