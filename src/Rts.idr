module Rts

import Data.SortedMap
import Data.SortedSet
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
globalStoreSymbols : SortedSet String
globalStoreSymbols = fromList
  [ "getOrSetGHCConcSignalSignalHandlerStore"
  , "getOrSetGHCConcWindowsPendingDelaysStore"
  , "getOrSetGHCConcWindowsIOManagerThreadStore"
  , "getOrSetGHCConcWindowsProddingStore"
  , "getOrSetSystemEventThreadEventManagerStore"
  , "getOrSetSystemEventThreadIOManagerThreadStore"
  , "getOrSetSystemTimerThreadEventManagerStore"
  , "getOrSetSystemTimerThreadIOManagerThreadStore"
  , "getOrSetLibHSghcFastStringTable"
  , "getOrSetLibHSghcPersistentLinkerState"
  , "getOrSetLibHSghcInitLinkerDone"
  , "getOrSetLibHSghcGlobalDynFlags"
  , "getOrSetLibHSghcStaticOptions"
  , "getOrSetLibHSghcStaticOptionsReady"
  ]

-- HINT: needed for FFI value boxing
wiredInCons : List (StgName, StgName, StgName, StgName, DataCon -> Rts -> Rts)
wiredInCons = []
  {-
  -- unit-id,     module,       type con,     data con
  [ ("ghc-prim",  "GHC.Types",  "Char",       "C#",         \s dc -> s {rtsCharCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Int",        "I#",         \s dc -> s {rtsIntCon       = dc})
  , ("ghc-prim",  "GHC.Types",  "Word",       "W#",         \s dc -> s {rtsWordCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Float",      "F#",         \s dc -> s {rtsFloatCon     = dc})
  , ("ghc-prim",  "GHC.Types",  "Double",     "D#",         \s dc -> s {rtsDoubleCon    = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "True",       \s dc -> s {rtsTrueCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "False",      \s dc -> s {rtsFalseCon     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int8",       "I8#",        \s dc -> s {rtsInt8Con      = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int16",      "I16#",       \s dc -> s {rtsInt16Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int32",      "I32#",       \s dc -> s {rtsInt32Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int64",      "I64#",       \s dc -> s {rtsInt64Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word8",      "W8#",        \s dc -> s {rtsWord8Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word16",     "W16#",       \s dc -> s {rtsWord16Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word32",     "W32#",       \s dc -> s {rtsWord32Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word64",     "W64#",       \s dc -> s {rtsWord64Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Ptr",    "Ptr",        "Ptr",        \s dc -> s {rtsPtrCon       = dc})
  , ("ghc-internal",      "GHC.Internal.Ptr",    "FunPtr",     "FunPtr",     \s dc -> s {rtsFunPtrCon    = dc})
  , ("ghc-internal",      "GHC.Internal.Stable", "StablePtr",  "StablePtr",  \s dc -> s {rtsStablePtrCon = dc})

  -- validation for extStgRtsSupportModule
  , ("ghc-prim",  "GHC.Tuple",  "Tuple2",        "(,)",        \s _dc -> s)
  ]
  -}

wiredInClosures : List (StgName, StgName, StgName, Atom -> Rts -> Rts)
wiredInClosures =
  -- unit-id,     module,                   binder,                         closure setter
  [ {-("ghc-internal",      "GHC.Internal.TopHandler",         "runIO",                        \s cl -> s {rtsTopHandlerRunIO            = cl})
  , ("ghc-internal",      "GHC.Internal.TopHandler",         "runNonIO",                     \s cl -> s {rtsTopHandlerRunNonIO         = cl})
  , -}("ghc-internal",      "GHC.Internal.TopHandler",         "flushStdHandles",              \cl => {rtsTopHandlerFlushStdHandles  := cl})
  {-
  , ("ghc-internal",      "GHC.Internal.Pack",               "unpackCString",                \s cl -> s {rtsUnpackCString              = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "divZeroException",             \s cl -> s {rtsDivZeroException           = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "underflowException",           \s cl -> s {rtsUnderflowException         = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "overflowException",            \s cl -> s {rtsOverflowException          = cl})
  , (":ext-stg",  ":ExtStg.RTS.Support",    "applyFun1Arg",                 \s cl -> s {rtsApplyFun1Arg               = cl})
  , (":ext-stg",  ":ExtStg.RTS.Support",    "tuple2Proj0",                  \s cl -> s {rtsTuple2Proj0                = cl})
  , ("ghc-internal",      "GHC.Internal.Control.Exception.Base", "nestedAtomically",             \s cl -> s {rtsNestedAtomically           = cl})
  , ("ghc-internal",      "GHC.Internal.Control.Exception.Base", "nonTermination",               \s cl -> s {rtsNonTermination             = cl})
  , ("ghc-internal",      "GHC.Internal.IO.Exception",       "blockedIndefinitelyOnMVar",    \s cl -> s {rtsBlockedIndefinitelyOnMVar  = cl})
  , ("ghc-internal",      "GHC.Internal.IO.Exception",       "blockedIndefinitelyOnSTM",     \s cl -> s {rtsBlockedIndefinitelyOnSTM   = cl})
  -}
  ]

export
initRtsSupport : String -> List String -> List Module -> M ()
initRtsSupport progName progArgs mods = do
  {-
  -- create empty Rts data con, it is filled gradually
  modify' $ \s@StgState{..} -> s {ssRtsSupport = emptyRts progName progArgs}
  initRtsCDataSymbols
  -}
  -- collect rts related modules
  let rtsModSet = fromList $
                    [(MkUnitId u, MkModuleName m) | (u, m, _, _, _) <- wiredInCons] ++
                    [(MkUnitId u, MkModuleName m) | (u, m, _, _) <- wiredInClosures]
      rtsMods = [m | m@(MkModule _ moduleUnitId moduleName _ _ _ _ _ _ _) <- mods, contains (moduleUnitId, moduleName) rtsModSet]
  {-
  -- lookup wired-in constructors
  let dcMap = Map.fromList
                [ ((moduleUnitId, moduleName, tcName, dcName), dc)
                | m@Module{..} <- rtsMods
                , (tcU, tcMs) <- moduleTyCons
                , tcU == moduleUnitId
                , (tcM, tcs) <- tcMs
                , tcM == moduleName
                , TyCon{..} <- tcs
                , dc@DataCon{..} <- tcDataCons
                ]

  forM_ wiredInCons $ \(u, m, t, d, setter) -> do
    case Map.lookup (UnitId u, ModuleName m, t, d) dcMap of
        Nothing -> error $ "missing wired in data con: " ++ show (u, m, t, d)
        Just dc -> modify' $ \s@StgState{..} -> s {ssRtsSupport = setter ssRtsSupport dc}
  -}
  -- lookup wired-in closures
  let getBindings : TopBinding -> List Binder
      getBindings = \case
        StgTopLifted (StgNonRec i _) => [i]
        StgTopLifted (StgRec l) => map fst l
        _ => []
      closureMap = SortedMap.fromList
                [ ((uId, mName, bName), topBinding)
                | m@(MkModule _ moduleUnitId moduleName _ _ _ _ _ _ _) <- rtsMods
                , topBinding <- concatMap getBindings $ gettops m
                , (uId, mName, bName, _) <- wiredInClosures
                , MkUnitId uId == moduleUnitId
                , MkModuleName mName == moduleName
                , bName == topBinding.binderName
                ]

  for_ wiredInClosures $ \(u, m, n, setter) => do
    case lookup (u, m, n) closureMap of
        Nothing => putStrLn $ "missing wired in closure: " ++ show (u, m, n)-- ++ "\n" ++ unlines (map show $ Map.keys closureMap)
        Just b  => do
          cl <- lookupEnv empty b
          modify {ssRtsSupport $= setter cl}
