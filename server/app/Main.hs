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
import           Types

main :: IO ()
main = do
  args <- cmdArgs voltronModes 
  case args of

    Server {..} -> do
      runServer $ ServerOpts port (fromString host) static pool db

    AddUser {..} -> do
      let thing = CreateUser email password "" ""
      rId <- runTask' db $ Auth.addUser thing 
      putStrLn ("Add User: " ++ show rId)

    AddClass {..} -> do
      let thing = CreateClass institution className instructor 
      rId <- runTask' db $ Auth.addClass thing 
      putStrLn ("Add Class: " ++ show rId)

    AddGroup {..} -> do
      let thing = CreateGroup className groupName editorLink
      rId <- runTask' db $ Auth.addGroup thing 
      putStrLn ("Add Group: " ++ show rId)

    AddEnroll {..} -> do
      let thing = CreateEnroll student className groupName 
      rId <- runTask' db $ Auth.addEnroll thing 
      putStrLn ("Add Group: " ++ show rId)