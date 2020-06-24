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

main :: IO ()
main = do
  args <- cmdArgs voltronModes 
  case args of
    Server {..} -> do
      runServer $ ServerOpts port (fromString host) static pool db
    AddInstructor {..} -> do
      let user = Auth.UserCreate email password "" ""
      instrId <- runTask' db $ Auth.addInstructor user
      putStrLn ("Add Instructor: " ++ show instrId)
      return ()
    AddGroup {..} -> do
      grpId <- runTask' db $ Auth.addGroup grpName editorLink
      putStrLn ("Add Group: " ++ show grpId)
      return ()
    AddStudent {..} -> do  
      let user = Auth.UserCreate email password "" ""
      studentId <- runTask' db $ Auth.addStudent user grpName
      putStrLn ("Add Student: " ++ show studentId)
      return ()