{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Effectful
import Effectful.Log.Static
import RIO (stderr)

main :: IO ()
main = do
  let isVerbose = True
  logOptions <- logOptionsHandle stderr isVerbose
  runEff $ runLog logOptions $ do
    logDebug "Debug message"
    logInfo "Info message"
    logWarn "Warn message"
    logError "Error message"
