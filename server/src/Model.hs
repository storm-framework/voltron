{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--compile-spec" @-}

module Model
  ( migrateAll
  , mkUser
  , mkClass
  , mkGroup
  , mkEnroll
  , mkResetPassword
  , User
  , Class
  , Group
  , Enroll
  , ResetPassword
  , userId'
  , userEmailAddress'
  , userPassword'
  , userFirstName'
  , userLastName'
  , userTheme'
  , userKeyBinds'
  , userAdmin'
  , classId'
  , classInstitution'
  , className'
  , classInstructor'
  , classEditorLang'
  , groupId'
  , groupName'
  , groupEditorLink'
  , groupClass'
  , enrollId'
  , enrollStudent'
  , enrollClass'
  , enrollGroup'
  , resetPasswordId'
  , resetPasswordEmail'
  , resetPasswordCode'
  , resetPasswordValid'
  , UserId
  , ClassId
  , GroupId
  , EnrollId
  , ResetPasswordId
  )
where


import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import qualified Database.Persist              as Persist

import           Binah.Core

import Data.ByteString (ByteString)
import Data.Text       (Text)

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  emailAddress Text
  password ByteString
  firstName Text
  lastName Text
  theme Text
  keyBinds Text
  admin Bool
  UniqueEmailAddress emailAddress

Class
  institution Text
  name Text
  instructor UserId
  editorLang Text
  UniqueInstClass institution name

Group
  name Text
  editorLink Text
  class ClassId
  UniqueGroupClass name class

Enroll
  student UserId
  class ClassId
  group GroupId
  UniqueEnroll student class

ResetPassword
  email Text
  code Text
  valid Bool
  UniqueReset code
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isInstructor :: ClassId -> UserId -> Bool @-}

{-@ measure isStudent :: ClassId -> UserId -> GroupId -> Bool @-}

{-@ measure isAdmin :: UserId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsClassInstructor R VIEWER = isInstructor (groupClass (entityVal R)) (entityKey VIEWER) @-}

{-@ predicate IsGroupStudent R VIEWER = isStudent (groupClass (entityVal R)) (entityKey VIEWER) (entityKey R) @-}

{-@ predicate IsSelf USER VIEWER = VIEWER == USER @-}

{-@ predicate IsAdmin _ VIEWER = isAdmin (entityKey VIEWER) @-}

{-@ predicate IsClassInstructorClass R _ VIEWER = isInstructor (entityKey R) (entityKey VIEWER) @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
     x_0: Text
  -> x_1: ByteString
  -> x_2: Text
  -> x_3: Text
  -> x_4: Text
  -> x_5: Text
  -> x_6: Bool
  -> BinahRecord <
       {\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userFirstName (entityVal row) == x_2 && userLastName (entityVal row) == x_3 && userTheme (entityVal row) == x_4 && userKeyBinds (entityVal row) == x_5 && userAdmin (entityVal row) == x_6}
     , {\new viewer -> IsInstructor viewer}
     , {\x_0 x_1 -> (x_1 == x_0)}
     > (Entity User) User
@-}
mkUser :: Text -> ByteString -> Text -> Text -> Text -> Text -> Bool -> BinahRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 x_4 x_5 x_6 = BinahRecord (User x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity User | (userAdmin (entityVal v)) => isAdmin (entityKey v)} @-}

{-@ assume userId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) User UserId
@-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userEmailAddress (entityVal row)}
  , {\field row -> field == userEmailAddress (entityVal row)}
  , {\old -> userEmailAddressCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userEmailAddressCap x_0)}
  > (Entity User) User Text
@-}
userEmailAddress' :: EntityFieldWrapper (Entity User) User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userPassword :: User -> ByteString @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' :: EntityFieldWrapper <
    {\x_0 x_1 -> (x_1 == x_0)}
  , {\row field -> field == userPassword (entityVal row)}
  , {\field row -> field == userPassword (entityVal row)}
  , {\old -> userPasswordCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userPasswordCap x_0)}
  > (Entity User) User ByteString
@-}
userPassword' :: EntityFieldWrapper (Entity User) User ByteString
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userFirstName :: User -> Text @-}

{-@ measure userFirstNameCap :: Entity User -> Bool @-}

{-@ assume userFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userFirstName (entityVal row)}
  , {\field row -> field == userFirstName (entityVal row)}
  , {\old -> userFirstNameCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userFirstNameCap x_0)}
  > (Entity User) User Text
@-}
userFirstName' :: EntityFieldWrapper (Entity User) User Text
userFirstName' = EntityFieldWrapper UserFirstName

{-@ measure userLastName :: User -> Text @-}

{-@ measure userLastNameCap :: Entity User -> Bool @-}

{-@ assume userLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userLastName (entityVal row)}
  , {\field row -> field == userLastName (entityVal row)}
  , {\old -> userLastNameCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userLastNameCap x_0)}
  > (Entity User) User Text
@-}
userLastName' :: EntityFieldWrapper (Entity User) User Text
userLastName' = EntityFieldWrapper UserLastName

{-@ measure userTheme :: User -> Text @-}

{-@ measure userThemeCap :: Entity User -> Bool @-}

{-@ assume userTheme' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userTheme (entityVal row)}
  , {\field row -> field == userTheme (entityVal row)}
  , {\old -> userThemeCap old}
  , {\old _ _ -> userThemeCap old}
  > (Entity User) User Text
@-}
userTheme' :: EntityFieldWrapper (Entity User) User Text
userTheme' = EntityFieldWrapper UserTheme

{-@ measure userKeyBinds :: User -> Text @-}

{-@ measure userKeyBindsCap :: Entity User -> Bool @-}

{-@ assume userKeyBinds' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userKeyBinds (entityVal row)}
  , {\field row -> field == userKeyBinds (entityVal row)}
  , {\old -> userKeyBindsCap old}
  , {\old _ _ -> userKeyBindsCap old}
  > (Entity User) User Text
@-}
userKeyBinds' :: EntityFieldWrapper (Entity User) User Text
userKeyBinds' = EntityFieldWrapper UserKeyBinds

{-@ measure userAdmin :: User -> Bool @-}

{-@ measure userAdminCap :: Entity User -> Bool @-}

{-@ assume userAdmin' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userAdmin (entityVal row)}
  , {\field row -> field == userAdmin (entityVal row)}
  , {\old -> userAdminCap old}
  , {\old _ _ -> userAdminCap old}
  > (Entity User) User Bool
@-}
userAdmin' :: EntityFieldWrapper (Entity User) User Bool
userAdmin' = EntityFieldWrapper UserAdmin

-- * Class
{-@ mkClass ::
     x_0: Text
  -> x_1: Text
  -> x_2: UserId
  -> x_3: Text
  -> BinahRecord <
       {\row -> classInstitution (entityVal row) == x_0 && className (entityVal row) == x_1 && classInstructor (entityVal row) == x_2 && classEditorLang (entityVal row) == x_3}
     , {\_ viewer -> isAdmin (entityKey viewer)}
     , {\x_0 x_1 -> False}
     > (Entity User) Class
@-}
mkClass :: Text -> Text -> UserId -> Text -> BinahRecord (Entity User) Class
mkClass x_0 x_1 x_2 x_3 = BinahRecord (Class x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Class | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Class | isInstructor (entityKey v) (classInstructor (entityVal v))} @-}

{-@ assume classId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) Class ClassId
@-}
classId' :: EntityFieldWrapper (Entity User) Class ClassId
classId' = EntityFieldWrapper ClassId

{-@ measure classInstitution :: Class -> Text @-}

{-@ measure classInstitutionCap :: Entity Class -> Bool @-}

{-@ assume classInstitution' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == classInstitution (entityVal row)}
  , {\field row -> field == classInstitution (entityVal row)}
  , {\old -> classInstitutionCap old}
  , {\old _ _ -> classInstitutionCap old}
  > (Entity User) Class Text
@-}
classInstitution' :: EntityFieldWrapper (Entity User) Class Text
classInstitution' = EntityFieldWrapper ClassInstitution

{-@ measure className :: Class -> Text @-}

{-@ measure classNameCap :: Entity Class -> Bool @-}

{-@ assume className' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == className (entityVal row)}
  , {\field row -> field == className (entityVal row)}
  , {\old -> classNameCap old}
  , {\old _ _ -> classNameCap old}
  > (Entity User) Class Text
@-}
className' :: EntityFieldWrapper (Entity User) Class Text
className' = EntityFieldWrapper ClassName

{-@ measure classInstructor :: Class -> UserId @-}

{-@ measure classInstructorCap :: Entity Class -> Bool @-}

{-@ assume classInstructor' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == classInstructor (entityVal row)}
  , {\field row -> field == classInstructor (entityVal row)}
  , {\old -> classInstructorCap old}
  , {\old _ _ -> classInstructorCap old}
  > (Entity User) Class UserId
@-}
classInstructor' :: EntityFieldWrapper (Entity User) Class UserId
classInstructor' = EntityFieldWrapper ClassInstructor

{-@ measure classEditorLang :: Class -> Text @-}

{-@ measure classEditorLangCap :: Entity Class -> Bool @-}

{-@ assume classEditorLang' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == classEditorLang (entityVal row)}
  , {\field row -> field == classEditorLang (entityVal row)}
  , {\old -> classEditorLangCap old}
  , {\x_0 x_1 x_2 -> ((isInstructor (entityKey x_0) (entityKey x_2))) => (classEditorLangCap x_0)}
  > (Entity User) Class Text
@-}
classEditorLang' :: EntityFieldWrapper (Entity User) Class Text
classEditorLang' = EntityFieldWrapper ClassEditorLang

-- * Group
{-@ mkGroup ::
     x_0: Text
  -> x_1: Text
  -> x_2: ClassId
  -> BinahRecord <
       {\row -> groupName (entityVal row) == x_0 && groupEditorLink (entityVal row) == x_1 && groupClass (entityVal row) == x_2}
     , {\r viewer -> isInstructor (groupClass (entityVal r)) (entityKey viewer)}
     , {\x_0 x_1 -> (IsClassInstructor x_0 x_1 || IsGroupStudent x_0 x_1)}
     > (Entity User) Group
@-}
mkGroup :: Text -> Text -> ClassId -> BinahRecord (Entity User) Group
mkGroup x_0 x_1 x_2 = BinahRecord (Group x_0 x_1 x_2)

{-@ invariant {v: Entity Group | v == getJust (entityKey v)} @-}



{-@ assume groupId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) Group GroupId
@-}
groupId' :: EntityFieldWrapper (Entity User) Group GroupId
groupId' = EntityFieldWrapper GroupId

{-@ measure groupName :: Group -> Text @-}

{-@ measure groupNameCap :: Entity Group -> Bool @-}

{-@ assume groupName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == groupName (entityVal row)}
  , {\field row -> field == groupName (entityVal row)}
  , {\old -> groupNameCap old}
  , {\x_0 x_1 x_2 -> ((IsClassInstructor x_0 x_2)) => (groupNameCap x_0)}
  > (Entity User) Group Text
@-}
groupName' :: EntityFieldWrapper (Entity User) Group Text
groupName' = EntityFieldWrapper GroupName

{-@ measure groupEditorLink :: Group -> Text @-}

{-@ measure groupEditorLinkCap :: Entity Group -> Bool @-}

{-@ assume groupEditorLink' :: EntityFieldWrapper <
    {\x_0 x_1 -> (IsClassInstructor x_0 x_1 || IsGroupStudent x_0 x_1)}
  , {\row field -> field == groupEditorLink (entityVal row)}
  , {\field row -> field == groupEditorLink (entityVal row)}
  , {\old -> groupEditorLinkCap old}
  , {\x_0 x_1 x_2 -> ((IsClassInstructor x_0 x_2)) => (groupEditorLinkCap x_0)}
  > (Entity User) Group Text
@-}
groupEditorLink' :: EntityFieldWrapper (Entity User) Group Text
groupEditorLink' = EntityFieldWrapper GroupEditorLink

{-@ measure groupClass :: Group -> ClassId @-}

{-@ measure groupClassCap :: Entity Group -> Bool @-}

{-@ assume groupClass' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == groupClass (entityVal row)}
  , {\field row -> field == groupClass (entityVal row)}
  , {\old -> groupClassCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (groupClassCap x_0)}
  > (Entity User) Group ClassId
@-}
groupClass' :: EntityFieldWrapper (Entity User) Group ClassId
groupClass' = EntityFieldWrapper GroupClass

-- * Enroll
{-@ mkEnroll ::
     x_0: UserId
  -> x_1: ClassId
  -> x_2: GroupId
  -> BinahRecord <
       {\row -> enrollStudent (entityVal row) == x_0 && enrollClass (entityVal row) == x_1 && enrollGroup (entityVal row) == x_2}
     , {\r viewer -> isInstructor (enrollClass (entityVal r)) (entityKey viewer)}
     , {\x_0 x_1 -> (isClassInstructor (enrollClass (entityVal x_0)) (entityKey x_1) || isStudent (enrollClass (entityVal x_0)) (entityKey x_1) (enrollGroup (entityVal x_0)))}
     > (Entity User) Enroll
@-}
mkEnroll :: UserId -> ClassId -> GroupId -> BinahRecord (Entity User) Enroll
mkEnroll x_0 x_1 x_2 = BinahRecord (Enroll x_0 x_1 x_2)

{-@ invariant {v: Entity Enroll | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Enroll | isStudent (enrollClass (entityVal v)) (enrollStudent (entityVal v)) (enrollGroup (entityVal v))} @-}

{-@ assume enrollId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) Enroll EnrollId
@-}
enrollId' :: EntityFieldWrapper (Entity User) Enroll EnrollId
enrollId' = EntityFieldWrapper EnrollId

{-@ measure enrollStudent :: Enroll -> UserId @-}

{-@ measure enrollStudentCap :: Entity Enroll -> Bool @-}

{-@ assume enrollStudent' :: EntityFieldWrapper <
    {\x_0 x_1 -> (isClassInstructor (enrollClass (entityVal x_0)) (entityKey x_1) || isStudent (enrollClass (entityVal x_0)) (entityKey x_1) (enrollGroup (entityVal x_0)))}
  , {\row field -> field == enrollStudent (entityVal row)}
  , {\field row -> field == enrollStudent (entityVal row)}
  , {\old -> enrollStudentCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (enrollStudentCap x_0)}
  > (Entity User) Enroll UserId
@-}
enrollStudent' :: EntityFieldWrapper (Entity User) Enroll UserId
enrollStudent' = EntityFieldWrapper EnrollStudent

{-@ measure enrollClass :: Enroll -> ClassId @-}

{-@ measure enrollClassCap :: Entity Enroll -> Bool @-}

{-@ assume enrollClass' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == enrollClass (entityVal row)}
  , {\field row -> field == enrollClass (entityVal row)}
  , {\old -> enrollClassCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (enrollClassCap x_0)}
  > (Entity User) Enroll ClassId
@-}
enrollClass' :: EntityFieldWrapper (Entity User) Enroll ClassId
enrollClass' = EntityFieldWrapper EnrollClass

{-@ measure enrollGroup :: Enroll -> GroupId @-}

{-@ measure enrollGroupCap :: Entity Enroll -> Bool @-}

{-@ assume enrollGroup' :: EntityFieldWrapper <
    {\x_0 x_1 -> (isClassInstructor (enrollClass (entityVal x_0)) (entityKey x_1) || isStudent (enrollClass (entityVal x_0)) (entityKey x_1) (enrollGroup (entityVal x_0)))}
  , {\row field -> field == enrollGroup (entityVal row)}
  , {\field row -> field == enrollGroup (entityVal row)}
  , {\old -> enrollGroupCap old}
  , {\old _ _ -> enrollGroupCap old}
  > (Entity User) Enroll GroupId
@-}
enrollGroup' :: EntityFieldWrapper (Entity User) Enroll GroupId
enrollGroup' = EntityFieldWrapper EnrollGroup

-- * ResetPassword
{-@ mkResetPassword ::
     x_0: Text
  -> x_1: Text
  -> x_2: Bool
  -> BinahRecord <
       {\row -> resetPasswordEmail (entityVal row) == x_0 && resetPasswordCode (entityVal row) == x_1 && resetPasswordValid (entityVal row) == x_2}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > (Entity User) ResetPassword
@-}
mkResetPassword :: Text -> Text -> Bool -> BinahRecord (Entity User) ResetPassword
mkResetPassword x_0 x_1 x_2 = BinahRecord (ResetPassword x_0 x_1 x_2)

{-@ invariant {v: Entity ResetPassword | v == getJust (entityKey v)} @-}



{-@ assume resetPasswordId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) ResetPassword ResetPasswordId
@-}
resetPasswordId' :: EntityFieldWrapper (Entity User) ResetPassword ResetPasswordId
resetPasswordId' = EntityFieldWrapper ResetPasswordId

{-@ measure resetPasswordEmail :: ResetPassword -> Text @-}

{-@ measure resetPasswordEmailCap :: Entity ResetPassword -> Bool @-}

{-@ assume resetPasswordEmail' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == resetPasswordEmail (entityVal row)}
  , {\field row -> field == resetPasswordEmail (entityVal row)}
  , {\old -> resetPasswordEmailCap old}
  , {\old _ _ -> resetPasswordEmailCap old}
  > (Entity User) ResetPassword Text
@-}
resetPasswordEmail' :: EntityFieldWrapper (Entity User) ResetPassword Text
resetPasswordEmail' = EntityFieldWrapper ResetPasswordEmail

{-@ measure resetPasswordCode :: ResetPassword -> Text @-}

{-@ measure resetPasswordCodeCap :: Entity ResetPassword -> Bool @-}

{-@ assume resetPasswordCode' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == resetPasswordCode (entityVal row)}
  , {\field row -> field == resetPasswordCode (entityVal row)}
  , {\old -> resetPasswordCodeCap old}
  , {\old _ _ -> resetPasswordCodeCap old}
  > (Entity User) ResetPassword Text
@-}
resetPasswordCode' :: EntityFieldWrapper (Entity User) ResetPassword Text
resetPasswordCode' = EntityFieldWrapper ResetPasswordCode

{-@ measure resetPasswordValid :: ResetPassword -> Bool @-}

{-@ measure resetPasswordValidCap :: Entity ResetPassword -> Bool @-}

{-@ assume resetPasswordValid' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == resetPasswordValid (entityVal row)}
  , {\field row -> field == resetPasswordValid (entityVal row)}
  , {\old -> resetPasswordValidCap old}
  , {\old _ _ -> resetPasswordValidCap old}
  > (Entity User) ResetPassword Bool
@-}
resetPasswordValid' :: EntityFieldWrapper (Entity User) ResetPassword Bool
resetPasswordValid' = EntityFieldWrapper ResetPasswordValid
