{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Frankie                        ( HostPreference
                                                , Port
                                                )
import qualified Data.Text                     as T
import           Data.Maybe
import           GHC.Exts                       ( fromString )
import           System.Console.CmdArgs
import           System.Environment
import           Server
import           Config
import qualified Auth                           
import qualified Controllers.Enroller as Enroller
import           Types

main :: IO ()
main = do
  args <- cmdArgs voltronModes 
  case args of

    Server {..} -> do
      runServer $ ServerOpts port (fromString host) static pool db

    AddClass {..} -> do
      let thing = mkCreateClass institution className instructor 
      rId <- runTask' db $ Enroller.addClass thing 
      putStrLn ("Add Class: " ++ show rId)

    AddUser {..} -> do
      let thing = mkCreateUser email password firstName lastName
      rId <- runTask' db $ Enroller.addUser thing 
      putStrLn ("Add User: " ++ show rId)

    -- AddGroup {..} -> do
    --   let thing = CreateGroup className groupName editorLink
    --   rId <- runTask' db $ Enroller.addGroup thing 
    --   putStrLn ("Add Group: " ++ show rId)

    -- AddEnroll {..} -> do
    --   let thing = CreateEnroll student className groupName 
    --   rId <- runTask' db $ Enroller.addEnroll thing 
    --   putStrLn ("Add Group: " ++ show rId)