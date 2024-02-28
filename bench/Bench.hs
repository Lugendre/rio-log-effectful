{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq (rnf)
import Criterion.Main
import Effectful
import Effectful.Log.Static qualified as V1
import Effectful.Log.StaticV2 qualified as V2
import RIO qualified

main :: IO ()
main = do
  !v1Options <- V1.setLogUseTime True <$> V1.logOptionsHandle RIO.stderr True
  !v2Options <- RIO.setLogUseTime True <$> RIO.logOptionsHandle RIO.stderr True

  defaultMain
    [ bgroup
        "rio effectful log"
        [ bench "v1" $ whnfIO $ v1Log v1Options
        , bench "v2" $ whnfIO $ v2Log v2Options
        ]
    ]

v1Log :: V1.LogOptions -> IO ()
v1Log options = do
  runEff $ V1.runLog options $ do
    V1.logInfo "Hello, world!"

v2Log :: RIO.LogOptions -> IO ()
v2Log options = do
  runEff $ V2.runLog options $ do
    V2.logInfo "Hello, world!"
