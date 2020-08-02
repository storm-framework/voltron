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
  ( EntityFieldWrapper(..)
  , migrateAll
  , BinahRecord
  , persistentRecord
  , mkInvitation
  , mkUser
  , mkClass
  , mkGroup
  , mkEnroll
  , mkResetPassword
  , Invitation
  , User
  , Class
  , Group
  , Enroll
  , ResetPassword
  , invitationId'
  , invitationCode'
  , invitationEmailAddress'
  , invitationFirstName'
  , invitationLastName'
  , invitationAccepted'
  , invitationEmailStatus'
  , invitationEmailError'
  , userId'
  , userEmailAddress'
  , userPassword'
  , userFirstName'
  , userLastName'
  , userAdmin'
  , classId'
  , classInstitution'
  , className'
  , classInstructor'
  , groupId'
  , groupName'
  , groupEditorLink'
  , groupClass'
  , enrollId'
  , enrollStudent'
  , enrollClass'
  , enrollGroup'
  , resetPasswordId'
  , resetPasswordCode'
  , resetPasswordEmail'
  , resetPasswordValid'
  , InvitationId
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
import           Data.Text                      ( Text )
import qualified Database.Persist              as Persist

import           Binah.Core

import Data.ByteString (ByteString)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Invitation
  code Text
  emailAddress Text
  firstName Text
  lastName Text
  accepted Bool
  emailStatus String
  emailError String Maybe
  

User
  emailAddress Text
  password ByteString
  firstName Text
  lastName Text
  admin Bool
  UniqueEmailAddress emailAddress

Class
  institution Text
  name Text
  instructor UserId
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
  code Text
  email Text
  valid Bool
  UniqueReset code
|]

{-@
data EntityFieldWrapper record typ < querypolicy :: Entity record -> Entity User -> Bool
                                   , selector :: Entity record -> typ -> Bool
                                   , flippedselector :: typ -> Entity record -> Bool
                                   , capability :: Entity record -> Bool
                                   , updatepolicy :: Entity record -> Entity record -> Entity User -> Bool
                                   > = EntityFieldWrapper _
@-}

data EntityFieldWrapper record typ = EntityFieldWrapper (Persist.EntityField record typ)
{-@ data variance EntityFieldWrapper covariant covariant invariant invariant invariant invariant invariant @-}

{-@ measure currentUser :: Entity User @-}

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsSelf USER VIEWER = VIEWER == USER @-}

{-@ predicate IsInstructor USER = userLevel USER == "instructor" @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ data BinahRecord record <
    p :: Entity record -> Bool
  , insertpolicy :: Entity record -> Entity User -> Bool
  , querypolicy  :: Entity record -> Entity User -> Bool
  >
  = BinahRecord _
@-}
data BinahRecord record = BinahRecord record
{-@ data variance BinahRecord invariant covariant invariant invariant @-}

{-@ persistentRecord :: BinahRecord record -> record @-}
persistentRecord :: BinahRecord record -> record
persistentRecord (BinahRecord record) = record

{-@ measure getJust :: Key record -> Entity record @-}

-- * Invitation
{-@ mkInvitation ::
     x_0: Text
  -> x_1: Text
  -> x_2: Text
  -> x_3: Text
  -> x_4: Bool
  -> x_5: String
  -> x_6: (Maybe String)
  -> BinahRecord <
       {\row -> invitationCode (entityVal row) == x_0 && invitationEmailAddress (entityVal row) == x_1 && invitationFirstName (entityVal row) == x_2 && invitationLastName (entityVal row) == x_3 && invitationAccepted (entityVal row) == x_4 && invitationEmailStatus (entityVal row) == x_5 && invitationEmailError (entityVal row) == x_6}
     , {\invitation viewer -> not (invitationAccepted (entityVal invitation)) && IsInstructor viewer && emailStatus invitation == "not_sent"}
     , {\x_0 x_1 -> False}
     > Invitation
@-}
mkInvitation x_0 x_1 x_2 x_3 x_4 x_5 x_6 = BinahRecord (Invitation x_0 x_1 x_2 x_3 x_4 x_5 x_6)

{-@ invariant {v: Entity Invitation | v == getJust (entityKey v)} @-}



{-@ assume invitationId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > Invitation InvitationId
@-}
invitationId' :: EntityFieldWrapper Invitation InvitationId
invitationId' = EntityFieldWrapper InvitationId

{-@ measure invitationCode :: Invitation -> Text @-}

{-@ measure invitationCodeCap :: Entity Invitation -> Bool @-}

{-@ assume invitationCode' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationCode (entityVal row)}
  , {\field row -> field == invitationCode (entityVal row)}
  , {\old -> invitationCodeCap old}
  , {\old _ _ -> invitationCodeCap old}
  > Invitation Text
@-}
invitationCode' :: EntityFieldWrapper Invitation Text
invitationCode' = EntityFieldWrapper InvitationCode

{-@ measure invitationEmailAddress :: Invitation -> Text @-}

{-@ measure invitationEmailAddressCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationEmailAddress (entityVal row)}
  , {\field row -> field == invitationEmailAddress (entityVal row)}
  , {\old -> invitationEmailAddressCap old}
  , {\old _ _ -> invitationEmailAddressCap old}
  > Invitation Text
@-}
invitationEmailAddress' :: EntityFieldWrapper Invitation Text
invitationEmailAddress' = EntityFieldWrapper InvitationEmailAddress

{-@ measure invitationFirstName :: Invitation -> Text @-}

{-@ measure invitationFirstNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationFirstName (entityVal row)}
  , {\field row -> field == invitationFirstName (entityVal row)}
  , {\old -> invitationFirstNameCap old}
  , {\old _ _ -> invitationFirstNameCap old}
  > Invitation Text
@-}
invitationFirstName' :: EntityFieldWrapper Invitation Text
invitationFirstName' = EntityFieldWrapper InvitationFirstName

{-@ measure invitationLastName :: Invitation -> Text @-}

{-@ measure invitationLastNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationLastName (entityVal row)}
  , {\field row -> field == invitationLastName (entityVal row)}
  , {\old -> invitationLastNameCap old}
  , {\old _ _ -> invitationLastNameCap old}
  > Invitation Text
@-}
invitationLastName' :: EntityFieldWrapper Invitation Text
invitationLastName' = EntityFieldWrapper InvitationLastName

{-@ measure invitationAccepted :: Invitation -> Bool @-}

{-@ measure invitationAcceptedCap :: Entity Invitation -> Bool @-}

{-@ assume invitationAccepted' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationAccepted (entityVal row)}
  , {\field row -> field == invitationAccepted (entityVal row)}
  , {\old -> invitationAcceptedCap old}
  , {\x_0 x_1 x_2 -> ((not (invitationAccepted (entityVal x_0)) && invitationAccepted (entityVal x_1))) => (invitationAcceptedCap x_0)}
  > Invitation Bool
@-}
invitationAccepted' :: EntityFieldWrapper Invitation Bool
invitationAccepted' = EntityFieldWrapper InvitationAccepted

{-@ measure invitationEmailStatus :: Invitation -> String @-}

{-@ measure invitationEmailStatusCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailStatus' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationEmailStatus (entityVal row)}
  , {\field row -> field == invitationEmailStatus (entityVal row)}
  , {\old -> invitationEmailStatusCap old}
  , {\x_0 x_1 x_2 -> ((IsInstructor viewer && (emailStatus x_1 == "sent" || emailStatus x_1 == "error"))) => (invitationEmailStatusCap x_0)}
  > Invitation String
@-}
invitationEmailStatus' :: EntityFieldWrapper Invitation String
invitationEmailStatus' = EntityFieldWrapper InvitationEmailStatus

{-@ measure invitationEmailError :: Invitation -> (Maybe String) @-}

{-@ measure invitationEmailErrorCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailError' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationEmailError (entityVal row)}
  , {\field row -> field == invitationEmailError (entityVal row)}
  , {\old -> invitationEmailErrorCap old}
  , {\old _ _ -> invitationEmailErrorCap old}
  > Invitation (Maybe String)
@-}
invitationEmailError' :: EntityFieldWrapper Invitation (Maybe String)
invitationEmailError' = EntityFieldWrapper InvitationEmailError

-- * User
{-@ mkUser ::
     x_0: Text
  -> x_1: ByteString
  -> x_2: Text
  -> x_3: Text
  -> x_4: Bool
  -> BinahRecord <
       {\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userFirstName (entityVal row) == x_2 && userLastName (entityVal row) == x_3 && userAdmin (entityVal row) == x_4}
     , {\new viewer -> IsInstructor viewer}
     , {\x_0 x_1 -> (x_1 == x_0)}
     > User
@-}
mkUser x_0 x_1 x_2 x_3 x_4 = BinahRecord (User x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > User UserId
@-}
userId' :: EntityFieldWrapper User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userEmailAddress (entityVal row)}
  , {\field row -> field == userEmailAddress (entityVal row)}
  , {\old -> userEmailAddressCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userEmailAddressCap x_0)}
  > User Text
@-}
userEmailAddress' :: EntityFieldWrapper User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userPassword :: User -> ByteString @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' :: EntityFieldWrapper <
    {\x_0 x_1 -> (x_1 == x_0)}
  , {\row field -> field == userPassword (entityVal row)}
  , {\field row -> field == userPassword (entityVal row)}
  , {\old -> userPasswordCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userPasswordCap x_0)}
  > User ByteString
@-}
userPassword' :: EntityFieldWrapper User ByteString
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userFirstName :: User -> Text @-}

{-@ measure userFirstNameCap :: Entity User -> Bool @-}

{-@ assume userFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userFirstName (entityVal row)}
  , {\field row -> field == userFirstName (entityVal row)}
  , {\old -> userFirstNameCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userFirstNameCap x_0)}
  > User Text
@-}
userFirstName' :: EntityFieldWrapper User Text
userFirstName' = EntityFieldWrapper UserFirstName

{-@ measure userLastName :: User -> Text @-}

{-@ measure userLastNameCap :: Entity User -> Bool @-}

{-@ assume userLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userLastName (entityVal row)}
  , {\field row -> field == userLastName (entityVal row)}
  , {\old -> userLastNameCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userLastNameCap x_0)}
  > User Text
@-}
userLastName' :: EntityFieldWrapper User Text
userLastName' = EntityFieldWrapper UserLastName

{-@ measure userAdmin :: User -> Bool @-}

{-@ measure userAdminCap :: Entity User -> Bool @-}

{-@ assume userAdmin' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userAdmin (entityVal row)}
  , {\field row -> field == userAdmin (entityVal row)}
  , {\old -> userAdminCap old}
  , {\old _ _ -> userAdminCap old}
  > User Bool
@-}
userAdmin' :: EntityFieldWrapper User Bool
userAdmin' = EntityFieldWrapper UserAdmin

-- * Class
{-@ mkClass ::
     x_0: Text
  -> x_1: Text
  -> x_2: UserId
  -> BinahRecord <
       {\row -> classInstitution (entityVal row) == x_0 && className (entityVal row) == x_1 && classInstructor (entityVal row) == x_2}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > Class
@-}
mkClass x_0 x_1 x_2 = BinahRecord (Class x_0 x_1 x_2)

{-@ invariant {v: Entity Class | v == getJust (entityKey v)} @-}



{-@ assume classId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > Class ClassId
@-}
classId' :: EntityFieldWrapper Class ClassId
classId' = EntityFieldWrapper ClassId

{-@ measure classInstitution :: Class -> Text @-}

{-@ measure classInstitutionCap :: Entity Class -> Bool @-}

{-@ assume classInstitution' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == classInstitution (entityVal row)}
  , {\field row -> field == classInstitution (entityVal row)}
  , {\old -> classInstitutionCap old}
  , {\old _ _ -> classInstitutionCap old}
  > Class Text
@-}
classInstitution' :: EntityFieldWrapper Class Text
classInstitution' = EntityFieldWrapper ClassInstitution

{-@ measure className :: Class -> Text @-}

{-@ measure classNameCap :: Entity Class -> Bool @-}

{-@ assume className' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == className (entityVal row)}
  , {\field row -> field == className (entityVal row)}
  , {\old -> classNameCap old}
  , {\old _ _ -> classNameCap old}
  > Class Text
@-}
className' :: EntityFieldWrapper Class Text
className' = EntityFieldWrapper ClassName

{-@ measure classInstructor :: Class -> UserId @-}

{-@ measure classInstructorCap :: Entity Class -> Bool @-}

{-@ assume classInstructor' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == classInstructor (entityVal row)}
  , {\field row -> field == classInstructor (entityVal row)}
  , {\old -> classInstructorCap old}
  , {\old _ _ -> classInstructorCap old}
  > Class UserId
@-}
classInstructor' :: EntityFieldWrapper Class UserId
classInstructor' = EntityFieldWrapper ClassInstructor

-- * Group
{-@ mkGroup ::
     x_0: Text
  -> x_1: Text
  -> x_2: ClassId
  -> BinahRecord <
       {\row -> groupName (entityVal row) == x_0 && groupEditorLink (entityVal row) == x_1 && groupClass (entityVal row) == x_2}
     , {\_ viewer -> IsInstructor viewer}
     , {\x_0 x_1 -> False}
     > Group
@-}
mkGroup x_0 x_1 x_2 = BinahRecord (Group x_0 x_1 x_2)

{-@ invariant {v: Entity Group | v == getJust (entityKey v)} @-}



{-@ assume groupId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > Group GroupId
@-}
groupId' :: EntityFieldWrapper Group GroupId
groupId' = EntityFieldWrapper GroupId

{-@ measure groupName :: Group -> Text @-}

{-@ measure groupNameCap :: Entity Group -> Bool @-}

{-@ assume groupName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == groupName (entityVal row)}
  , {\field row -> field == groupName (entityVal row)}
  , {\old -> groupNameCap old}
  , {\x_0 x_1 x_2 -> ((IsInstructor x_2)) => (groupNameCap x_0)}
  > Group Text
@-}
groupName' :: EntityFieldWrapper Group Text
groupName' = EntityFieldWrapper GroupName

{-@ measure groupEditorLink :: Group -> Text @-}

{-@ measure groupEditorLinkCap :: Entity Group -> Bool @-}

{-@ assume groupEditorLink' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == groupEditorLink (entityVal row)}
  , {\field row -> field == groupEditorLink (entityVal row)}
  , {\old -> groupEditorLinkCap old}
  , {\x_0 x_1 x_2 -> ((IsInstructor x_2)) => (groupEditorLinkCap x_0)}
  > Group Text
@-}
groupEditorLink' :: EntityFieldWrapper Group Text
groupEditorLink' = EntityFieldWrapper GroupEditorLink

{-@ measure groupClass :: Group -> ClassId @-}

{-@ measure groupClassCap :: Entity Group -> Bool @-}

{-@ assume groupClass' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == groupClass (entityVal row)}
  , {\field row -> field == groupClass (entityVal row)}
  , {\old -> groupClassCap old}
  , {\old _ _ -> groupClassCap old}
  > Group ClassId
@-}
groupClass' :: EntityFieldWrapper Group ClassId
groupClass' = EntityFieldWrapper GroupClass

-- * Enroll
{-@ mkEnroll ::
     x_0: UserId
  -> x_1: ClassId
  -> x_2: GroupId
  -> BinahRecord <
       {\row -> enrollStudent (entityVal row) == x_0 && enrollClass (entityVal row) == x_1 && enrollGroup (entityVal row) == x_2}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > Enroll
@-}
mkEnroll x_0 x_1 x_2 = BinahRecord (Enroll x_0 x_1 x_2)

{-@ invariant {v: Entity Enroll | v == getJust (entityKey v)} @-}



{-@ assume enrollId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > Enroll EnrollId
@-}
enrollId' :: EntityFieldWrapper Enroll EnrollId
enrollId' = EntityFieldWrapper EnrollId

{-@ measure enrollStudent :: Enroll -> UserId @-}

{-@ measure enrollStudentCap :: Entity Enroll -> Bool @-}

{-@ assume enrollStudent' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == enrollStudent (entityVal row)}
  , {\field row -> field == enrollStudent (entityVal row)}
  , {\old -> enrollStudentCap old}
  , {\old _ _ -> enrollStudentCap old}
  > Enroll UserId
@-}
enrollStudent' :: EntityFieldWrapper Enroll UserId
enrollStudent' = EntityFieldWrapper EnrollStudent

{-@ measure enrollClass :: Enroll -> ClassId @-}

{-@ measure enrollClassCap :: Entity Enroll -> Bool @-}

{-@ assume enrollClass' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == enrollClass (entityVal row)}
  , {\field row -> field == enrollClass (entityVal row)}
  , {\old -> enrollClassCap old}
  , {\old _ _ -> enrollClassCap old}
  > Enroll ClassId
@-}
enrollClass' :: EntityFieldWrapper Enroll ClassId
enrollClass' = EntityFieldWrapper EnrollClass

{-@ measure enrollGroup :: Enroll -> GroupId @-}

{-@ measure enrollGroupCap :: Entity Enroll -> Bool @-}

{-@ assume enrollGroup' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == enrollGroup (entityVal row)}
  , {\field row -> field == enrollGroup (entityVal row)}
  , {\old -> enrollGroupCap old}
  , {\old _ _ -> enrollGroupCap old}
  > Enroll GroupId
@-}
enrollGroup' :: EntityFieldWrapper Enroll GroupId
enrollGroup' = EntityFieldWrapper EnrollGroup

-- * ResetPassword
{-@ mkResetPassword ::
     x_0: Text
  -> x_1: Text
  -> x_2: Bool
  -> BinahRecord <
       {\row -> resetPasswordCode (entityVal row) == x_0 && resetPasswordEmail (entityVal row) == x_1 && resetPasswordValid (entityVal row) == x_2}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > ResetPassword
@-}
mkResetPassword x_0 x_1 x_2 = BinahRecord (ResetPassword x_0 x_1 x_2)

{-@ invariant {v: Entity ResetPassword | v == getJust (entityKey v)} @-}



{-@ assume resetPasswordId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > ResetPassword ResetPasswordId
@-}
resetPasswordId' :: EntityFieldWrapper ResetPassword ResetPasswordId
resetPasswordId' = EntityFieldWrapper ResetPasswordId

{-@ measure resetPasswordCode :: ResetPassword -> Text @-}

{-@ measure resetPasswordCodeCap :: Entity ResetPassword -> Bool @-}

{-@ assume resetPasswordCode' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == resetPasswordCode (entityVal row)}
  , {\field row -> field == resetPasswordCode (entityVal row)}
  , {\old -> resetPasswordCodeCap old}
  , {\old _ _ -> resetPasswordCodeCap old}
  > ResetPassword Text
@-}
resetPasswordCode' :: EntityFieldWrapper ResetPassword Text
resetPasswordCode' = EntityFieldWrapper ResetPasswordCode

{-@ measure resetPasswordEmail :: ResetPassword -> Text @-}

{-@ measure resetPasswordEmailCap :: Entity ResetPassword -> Bool @-}

{-@ assume resetPasswordEmail' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == resetPasswordEmail (entityVal row)}
  , {\field row -> field == resetPasswordEmail (entityVal row)}
  , {\old -> resetPasswordEmailCap old}
  , {\old _ _ -> resetPasswordEmailCap old}
  > ResetPassword Text
@-}
resetPasswordEmail' :: EntityFieldWrapper ResetPassword Text
resetPasswordEmail' = EntityFieldWrapper ResetPasswordEmail

{-@ measure resetPasswordValid :: ResetPassword -> Bool @-}

{-@ measure resetPasswordValidCap :: Entity ResetPassword -> Bool @-}

{-@ assume resetPasswordValid' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == resetPasswordValid (entityVal row)}
  , {\field row -> field == resetPasswordValid (entityVal row)}
  , {\old -> resetPasswordValidCap old}
  , {\old _ _ -> resetPasswordValidCap old}
  > ResetPassword Bool
@-}
resetPasswordValid' :: EntityFieldWrapper ResetPassword Bool
resetPasswordValid' = EntityFieldWrapper ResetPasswordValid

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------


