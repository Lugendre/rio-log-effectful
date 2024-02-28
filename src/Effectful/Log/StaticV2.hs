{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.Log.StaticV2 where

import Effectful
import Effectful.Dispatch.Static
import RIO qualified

data Log :: Effect
type instance DispatchOf Log = Static WithSideEffects
newtype instance StaticRep Log = Log RIO.LogFunc

runLog :: (IOE :> es) => RIO.LogOptions -> Eff (Log : es) a -> Eff es a
runLog options inner =
  RIO.withLogFunc options $ \lf ->
    evalStaticRep (Log lf) inner

logGeneric ::
  (Log :> es, HasCallStack) =>
  RIO.LogSource ->
  RIO.LogLevel ->
  RIO.Utf8Builder ->
  Eff es ()
logGeneric src level str = do
  Log logFunc <- getStaticRep
  unsafeEff_
    . flip RIO.runReaderT logFunc
    $ RIO.logGeneric src level str
{-# INLINE logGeneric #-}

logInfo :: (Log :> es, HasCallStack) => RIO.Utf8Builder -> Eff es ()
logInfo = logGeneric "" RIO.LevelInfo
{-# INLINE logInfo #-}
