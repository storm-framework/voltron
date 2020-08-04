{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Reset (reset, resetPass) where

import           Data.Aeson
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Time             ( MonadTime(..) )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad                  ( replicateM )
import           Control.Lens.Operators         ( (?~)
                                                , (^.)
                                                , (^?)
                                                , (<&>)
                                                )
import           Control.Lens.Combinators
                                         hiding ( assign )
import           Control.Lens.Lens              ( (&) )
import           Control.Lens.Internal.ByteString
                                                ( unpackLazy8 )
import           Frankie.Auth
import           Crypto.JWT
import           Crypto.JOSE.Types              ( Base64Octets(..) )
import           Data.Text                      ( Text(..) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy.Encoding       as L
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Base64.URL    as B64Url
import qualified Data.ByteString.Lazy          as L
import           Data.Int                       ( Int64 )
import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           GHC.Generics
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           Frankie.Config
import qualified Frankie.Log                   as Log
import           Binah.Core
import           Binah.Actions
import           Binah.Updates
import           Binah.Insert
import           Binah.Filters
import           Binah.Helpers
import           Binah.Infrastructure
import           Binah.Templates
import           Binah.Frankie
import           Binah.SMTP

import           Controllers
import           Controllers.User               ( extractUserData, extractUserNG )
import           Controllers.Class              ( genRandomText )
import           Model
import           JSON
import           Crypto
import           Types 

-------------------------------------------------------------------------------
-- | Reset password : generate a random code and send to user's email 
-------------------------------------------------------------------------------
{-@ ignore reset @-}
reset :: Controller ()
reset = do
  ResetInfo {..} <- decodeBody
  user           <- selectFirstOr 
                      (errorResponse status401 (Just "Unknown email address"))
                      (userEmailAddress' ==. resetEmailAddress)
  _    <- updateWhere 
            (resetPasswordEmail' ==. resetEmailAddress) 
            (resetPasswordValid' `assign` False)
  code <- genRandomText
  insert (mkResetPassword resetEmailAddress code True)
  sendResetMail code resetEmailAddress
  respondJSON status200 $ "OK: Please check " <> resetEmailAddress

sendResetMail :: Text -> Text -> Controller ()
sendResetMail code userEmail = do 
  smtpConfig <- configSMTP <$> getConfig
  let subject = "VOLTRON Password Reset Code"
  let to      = mkPublicAddress (T.unpack userEmail)
  let from    = mkPublicAddress (smtpUser smtpConfig) 
  res        <- sendPlainTextMail smtpConfig to from subject (mkBody code)
  Log.log Log.INFO ("reset email: " ++ show res)
  case res of 
    Right _ -> return () 
    Left _  -> respondError status401 (Just "Error sending email!")

mkBody :: Text -> LT.Text
mkBody code =
  LT.fromStrict
    $  "Please use the following code to reset your VOLTRON password: "
    <> code

-------------------------------------------------------------------------------
-- | `resetPass` actually resets the password using a previously mailed code
-------------------------------------------------------------------------------
{-@ ignore resetPass @-}
resetPass :: Controller ()
resetPass = do
  ResetPassInfo {..} <- decodeBody
  -- 1. Check if the code is valid
  _ <- selectFirstOr
        (errorResponse status403 (Just "invalid reset code"))
        ((resetPasswordCode'  ==. resetPassCode) &&: 
         (resetPasswordEmail' ==. resetPassEmail) &&: 
         (resetPasswordValid' ==. True))
  -- 2. Invalidate the code 
  _  <- updateWhere 
          (resetPasswordCode' ==. resetPassCode) 
          (resetPasswordValid' `assign` False)
  -- 3. Update the user's information
  EncryptedPass encrypted <- encryptPassTIO' (Pass (T.encodeUtf8 resetPassPassword))
  _  <- updateWhere 
          (userEmailAddress' ==. resetPassEmail)
          (userPassword' `assign` encrypted)
  respondJSON status200 ("OK: Login with new password" :: T.Text) 
