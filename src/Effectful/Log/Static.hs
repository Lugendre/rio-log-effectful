{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Effectful.Log.Static (
  -- ** Running with logging
  withLogFunc,
  newLogFunc,
  LogFunc,
  Log,
  StaticRep(Log),
  logOptionsHandle,
  runLogRaw,
  runLog,

  -- *** Log options
  LogOptions,
  setLogMinLevel,
  setLogMinLevelIO,
  setLogVerboseFormat,
  setLogVerboseFormatIO,
  setLogTerminal,
  setLogUseTime,
  setLogUseColor,
  setLogUseLoc,
  setLogFormat,
  setLogLevelColors,
  setLogSecondaryColor,
  setLogAccentColors,

  -- ** Standard logging functions
  logDebug,
  logInfo,
  logWarn,
  logError,
  logOther,

  -- ** Advanced logging functions

  -- *** Sticky logging
  logSticky,
  logStickyDone,

  -- *** With source

  --
  -- $withSource
  logDebugS,
  logInfoS,
  logWarnS,
  logErrorS,
  logOtherS,

  -- *** Generic log function
  logGeneric,

  -- ** Advanced running functions
  mkLogFunc,
  logOptionsMemory,

  -- ** Data types
  LogLevel (..),
  LogSource,
  CallStack,

  -- ** Convenience functions
  displayCallStack,

  -- * Type-generic logger
  GLog,
  runGLogRaw,
  glog,
  GLogFunc,
  gLogFuncClassic,
  mkGLogFunc,
  contramapMaybeGLogFunc,
  contramapGLogFunc,
  HasLogLevel (..),
  HasLogSource (..),
)
where

import Effectful
import Effectful.Dispatch.Static

import Data.Kind (Type)
import RIO hiding (
  glog,
  logDebug,
  logDebugS,
  logError,
  logErrorS,
  logGeneric,
  logInfo,
  logInfoS,
  logOther,
  logOtherS,
  logSticky,
  logStickyDone,
  logWarn,
  logWarnS,
 )
import RIO qualified

-- | Provide the ability to log messages via 'LogFunc' like 'RIO'.
data Log :: Effect

type instance DispatchOf Log = Static WithSideEffects
newtype instance StaticRep Log = Log LogFunc

-- | Run the 'Log' effect.
runLogRaw :: (IOE :> es) => LogFunc -> Eff (Log : es) a -> Eff es a
runLogRaw logFunc = evalStaticRep (Log logFunc)

-- | Run the 'Log' effect with 'withLogFunc'.
runLog :: forall es a. (IOE :> es) => LogOptions -> Eff (Log : es) a -> Eff es a
runLog options inner = withLogFunc options $ \lf ->
  evalStaticRep (Log lf) inner

-- | Lifted 'RIO.logGeneric'.
logGeneric ::
  (Log :> es, HasCallStack) =>
  LogSource ->
  LogLevel ->
  Utf8Builder ->
  Eff es ()
logGeneric src level str = do
  Log logFunc <- getStaticRep
  unsafeEff_
    . flip runReaderT logFunc
    $ RIO.logGeneric src level str
{-# INLINE logGeneric #-}

-- Note: Benchmarking showed no difference in performance between the implementation without 'runReaderT'.

-- | Lifted 'RIO.logDebug'.
logDebug ::
  (Log :> es, HasCallStack) =>
  Utf8Builder ->
  Eff es ()
logDebug = logGeneric "" LevelDebug
{-# INLINE logDebug #-}

-- | Lifted 'RIO.logInfo'.
logInfo ::
  (Log :> es, HasCallStack) =>
  Utf8Builder ->
  Eff es ()
logInfo = logGeneric "" LevelInfo
{-# INLINE logInfo #-}

-- | Lifted 'RIO.logWarn'.
logWarn ::
  (Log :> es, HasCallStack) =>
  Utf8Builder ->
  Eff es ()
logWarn = logGeneric "" LevelWarn
{-# INLINE logWarn #-}

-- | Lifted 'RIO.logError'.
logError ::
  (Log :> es, HasCallStack) =>
  Utf8Builder ->
  Eff es ()
logError = logGeneric "" LevelError
{-# INLINE logError #-}

-- | Lifted 'RIO.logOther'.
logOther ::
  (Log :> es, HasCallStack) =>
  -- | level
  Text ->
  Utf8Builder ->
  Eff es ()
logOther = logGeneric "" . LevelOther
{-# INLINE logOther #-}

-- | Lifted 'RIO.logDebugS'.
logDebugS ::
  (Log :> es, HasCallStack) =>
  LogSource ->
  Utf8Builder ->
  Eff es ()
logDebugS src = logGeneric src LevelDebug
{-# INLINE logDebugS #-}

-- | Lifted 'RIO.logInfoS'.
logInfoS ::
  (Log :> es, HasCallStack) =>
  LogSource ->
  Utf8Builder ->
  Eff es ()
logInfoS src = logGeneric src LevelInfo
{-# INLINE logInfoS #-}

-- | Lifted 'RIO.logWarnS'.
logWarnS ::
  (Log :> es, HasCallStack) =>
  LogSource ->
  Utf8Builder ->
  Eff es ()
logWarnS src = logGeneric src LevelWarn
{-# INLINE logWarnS #-}

-- | Lifted 'RIO.logErrorS'.
logErrorS ::
  (Log :> es, HasCallStack) =>
  LogSource ->
  Utf8Builder ->
  Eff es ()
logErrorS src = logGeneric src LevelError
{-# INLINE logErrorS #-}

-- | Lifted 'RIO.logOtherS'.
logOtherS ::
  (Log :> es, HasCallStack) =>
  -- | level
  Text ->
  LogSource ->
  Utf8Builder ->
  Eff es ()
logOtherS src = logGeneric src . LevelOther
{-# INLINE logOtherS #-}

-- | Lifted 'RIO.logSticky'.
logSticky :: (Log :> es, HasCallStack) => Utf8Builder -> Eff es ()
logSticky = logOther "sticky"

-- | Lifted 'RIO.logStickyDone'.
logStickyDone :: (Log :> es, HasCallStack) => Utf8Builder -> Eff es ()
logStickyDone = logOther "sticky-done"

-- | Provide the ability to log messages via 'GLogFunc' like 'RIO'.
data GLog msg :: Effect

type instance DispatchOf (GLog msg) = Static WithSideEffects
newtype instance StaticRep (GLog (msg :: Type)) = GLog (GLogFunc msg)

-- | Run the 'GLog' effect.
runGLogRaw :: (IOE :> es) => GLogFunc msg -> Eff (GLog msg : es) a -> Eff es a
runGLogRaw glogFunc = evalStaticRep (GLog glogFunc)

-- | Lifted 'RIO.glog'.
glog ::
  (GLog msg :> es) =>
  msg ->
  Eff es ()
glog t = do
  GLog glogFunc <- getStaticRep
  unsafeEff_ . flip runReaderT glogFunc $ RIO.glog t
{-# INLINEABLE glog #-}
