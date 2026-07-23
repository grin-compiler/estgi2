module Rts

import Data.SortedMap
import Data.SortedSet
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Stg.Reconstruct
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
wiredInCons : List (StgName, StgName, StgName, StgName)
wiredInCons =
  -- unit-id,     module,       type con,     data con
  [ ("ghc-prim",  "GHC.Types",  "Char",       "C#")-- ,         \s dc -> s {rtsCharCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Int",        "I#")-- ,         \s dc -> s {rtsIntCon       = dc})
  , ("ghc-prim",  "GHC.Types",  "Word",       "W#")-- ,         \s dc -> s {rtsWordCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Float",      "F#")-- ,         \s dc -> s {rtsFloatCon     = dc})
  , ("ghc-prim",  "GHC.Types",  "Double",     "D#")-- ,         \s dc -> s {rtsDoubleCon    = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "True")-- ,       \s dc -> s {rtsTrueCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "False")-- ,      \s dc -> s {rtsFalseCon     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int8",       "I8#")-- ,        \s dc -> s {rtsInt8Con      = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int16",      "I16#")-- ,       \s dc -> s {rtsInt16Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int32",      "I32#")-- ,       \s dc -> s {rtsInt32Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int64",      "I64#")-- ,       \s dc -> s {rtsInt64Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word8",      "W8#")-- ,        \s dc -> s {rtsWord8Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word16",     "W16#")-- ,       \s dc -> s {rtsWord16Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word32",     "W32#")-- ,       \s dc -> s {rtsWord32Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word64",     "W64#")-- ,       \s dc -> s {rtsWord64Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Ptr",    "Ptr",        "Ptr")-- ,        \s dc -> s {rtsPtrCon       = dc})
  , ("ghc-internal",      "GHC.Internal.Ptr",    "FunPtr",     "FunPtr")-- ,     \s dc -> s {rtsFunPtrCon    = dc})
  , ("ghc-internal",      "GHC.Internal.Stable", "StablePtr",  "StablePtr")-- ,  \s dc -> s {rtsStablePtrCon = dc})

  -- validation for extStgRtsSupportModule
  , ("ghc-prim",  "GHC.Tuple",  "Tuple2",        "(,)")-- ,        \s _dc -> s)
  ]

wiredInClosures : List (StgName, StgName, StgName)
wiredInClosures =
  -- unit-id,     module,                   binder,                         closure setter
  [ {-("ghc-internal",      "GHC.Internal.TopHandler",         "runIO",                        \s cl -> s {rtsTopHandlerRunIO            = cl})
  , ("ghc-internal",      "GHC.Internal.TopHandler",         "runNonIO",                     \s cl -> s {rtsTopHandlerRunNonIO         = cl})
  , -}("ghc-internal",      "GHC.Internal.TopHandler",         "flushStdHandles")
  {-
  , ("ghc-internal",      "GHC.Internal.Pack",               "unpackCString",                \s cl -> s {rtsUnpackCString              = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "divZeroException",             \s cl -> s {rtsDivZeroException           = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "underflowException",           \s cl -> s {rtsUnderflowException         = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "overflowException",            \s cl -> s {rtsOverflowException          = cl})
  -}
  , (":ext-stg",  ":ExtStg.RTS.Support",    "applyFun1Arg")
  , (":ext-stg",  ":ExtStg.RTS.Support",    "tuple2Proj0")
  {-
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
                    [(MkUnitId u, MkModuleName m) | (u, m, _, _) <- wiredInCons] ++
                    [(MkUnitId u, MkModuleName m) | (u, m, _) <- wiredInClosures]
      rtsMods = [m | m@(MkModule _ moduleUnitId moduleName _ _ _ _ _ _ _) <- mods, contains (moduleUnitId, moduleName) rtsModSet]

  -- lookup wired-in constructors
  let dcMap = SortedMap.fromList
                [ ((moduleUnitId, moduleName, tc.tcName, dc.dcName), dc)
                | MkModule _ moduleUnitId moduleName _ _ _ _ _ moduleTyCons _ <- rtsMods
                , (tcU, tcMs) <- moduleTyCons
                , tcU == moduleUnitId
                , (tcM, tcs) <- tcMs
                , tcM == moduleName
                , tc <- tcs
                , dc <- tc.tcDataCons
                ]

      lookupDataCon : StgName -> StgName -> StgName -> StgName -> M DataCon
      lookupDataCon u m t d = case lookup (MkUnitId u, MkModuleName m, t, d) dcMap of
        Nothing => stg_error $ "missing wired in data con: " ++ show (u, m, t, d)
        Just dc => pure dc

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
                , (uId, mName, bName) <- wiredInClosures
                , MkUnitId uId == moduleUnitId
                , MkModuleName mName == moduleName
                , bName == topBinding.binderName
                ]

      lookupClosure : StgName -> StgName -> StgName -> M Atom
      lookupClosure u m n = case lookup (u, m, n) closureMap of
        Nothing => stg_error $ "missing wired in closure: " ++ show (u, m, n)-- ++ "\n" ++ unlines (map show $ Map.keys closureMap)
        Just b  => lookupEnv empty b

  -- validation, needed for rts support
  _ <- lookupDataCon "ghc-prim" "GHC.Tuple" "Tuple2" "(,)"

  let wiredIns = MkWiredIns
        { rtsCharCon      = !(lookupDataCon "ghc-prim" "GHC.Types" "Char" "C#")
        , rtsIntCon       = !(lookupDataCon "ghc-prim" "GHC.Types" "Int"  "I#")
        , rtsInt8Con      = !(lookupDataCon "ghc-internal" "GHC.Internal.Int" "Int8"  "I8#")
        , rtsInt16Con     = !(lookupDataCon "ghc-internal" "GHC.Internal.Int" "Int16" "I16#")
        , rtsInt32Con     = !(lookupDataCon "ghc-internal" "GHC.Internal.Int" "Int32" "I32#")
        , rtsInt64Con     = !(lookupDataCon "ghc-internal" "GHC.Internal.Int" "Int64" "I64#")
        , rtsWordCon      = !(lookupDataCon "ghc-prim" "GHC.Types" "Word" "W#")
        , rtsWord8Con     = !(lookupDataCon "ghc-internal" "GHC.Internal.Word" "Word8"  "W8#")
        , rtsWord16Con    = !(lookupDataCon "ghc-internal" "GHC.Internal.Word" "Word16" "W16#")
        , rtsWord32Con    = !(lookupDataCon "ghc-internal" "GHC.Internal.Word" "Word32" "W32#")
        , rtsWord64Con    = !(lookupDataCon "ghc-internal" "GHC.Internal.Word" "Word64" "W64#")
        , rtsPtrCon       = !(lookupDataCon "ghc-internal" "GHC.Internal.Ptr"  "Ptr"    "Ptr")
        , rtsFunPtrCon    = !(lookupDataCon "ghc-internal" "GHC.Internal.Ptr"  "FunPtr" "FunPtr")
        , rtsFloatCon     = !(lookupDataCon "ghc-prim" "GHC.Types" "Float"  "F#")
        , rtsDoubleCon    = !(lookupDataCon "ghc-prim" "GHC.Types" "Double" "D#")
        , rtsStablePtrCon = !(lookupDataCon "ghc-internal" "GHC.Internal.Stable" "StablePtr" "StablePtr")
        , rtsTrueCon      = !(lookupDataCon "ghc-prim" "GHC.Types" "Bool" "True")
        , rtsFalseCon     = !(lookupDataCon "ghc-prim" "GHC.Types" "Bool" "False")
        , rtsTopHandlerFlushStdHandles = !(lookupClosure "ghc-internal" "GHC.Internal.TopHandler" "flushStdHandles")
        , rtsApplyFun1Arg = !(lookupClosure ":ext-stg"  ":ExtStg.RTS.Support"    "applyFun1Arg")
        , rtsTuple2Proj0  = !(lookupClosure ":ext-stg"  ":ExtStg.RTS.Support"    "tuple2Proj0")
        }
  modify {ssWiredIns := Just wiredIns}

export
extStgRtsSupportModule : IO Module
extStgRtsSupportModule = reconModule $ MkModule
  {- modulePhase              -} "ext-stg interpreter"
  {- moduleUnitId             -} (MkUnitId ":ext-stg")
  {- moduleName               -} (MkModuleName ":ExtStg.RTS.Support")
  {- moduleSourceFilePath     -} Nothing
  {- moduleForeignStubs       -} NoStubs
  {- moduleHasForeignExported -} False
  {- moduleDependency         -} [(MkUnitId "ghc-prim", [MkModuleName "GHC.Tuple"])]
  {- moduleExternalTopIds     -} []
  {- moduleTyCons             -} [(MkUnitId "ghc-prim", [(MkModuleName "GHC.Tuple", [tup2STyCon])])]
  {- moduleTopBindings        -} [tuple2Proj0, applyFun1Arg]
   where
      u : Int -> Unique
      u = MkUnique '+'

      sbinder : Int -> StgName -> Scope -> SBinder
      sbinder i n s = MkSBinder
                      {- sbinderName     -} n
                      {- sbinderId       -} (MkBinderId $ u i)
                      {- sbinderType     -} (SingleValue LiftedRep)
                      {- sbinderTypeSig  -} ""
                      {- sbinderScope    -} s
                      {- sbinderDetails  -} VanillaId
                      {- sbinderInfo     -} ""
                      {- sbinderDefLoc   -} (UnhelpfulSpan $ UnhelpfulOther "ext-stg-interpreter-rts")

      localLiftedVanillaId : Int -> StgName -> (BinderId, SBinder)
      localLiftedVanillaId i n = (MkBinderId $ u i, sbinder i n ClosurePrivate)

      exportedLiftedVanillaId : Int -> StgName -> SBinder
      exportedLiftedVanillaId i n = sbinder i n ModulePublic

      tup2DCOcc       = MkDataConId $ u 0
      tup2SDataCon    = MkSDataCon
                        {- sdcName   -} "(,)"
                        {- sdcId     -} tup2DCOcc
                        {- sdcRep    -} (AlgDataCon [LiftedRep, LiftedRep])
                        {- sdcWorker -} (exportedLiftedVanillaId 666 "fake ext-stg Tup2 worker")
                        {- sdcDefLoc -} (UnhelpfulSpan $ UnhelpfulOther "ext-stg-interpreter-rts")

      tup2TCOcc       = MkTyConId $ u 1
      tup2STyCon      = MkSTyCon
                        {- stcName     -} "(,)"
                        {- stcId       -} tup2TCOcc
                        {- stcDataCons -} [tup2SDataCon]
                        {- stcDefLoc   -} (UnhelpfulSpan $ UnhelpfulOther "ext-stg-interpreter-rts")

      -- code for tuple2Proj0 = \t -> case t of GHC.Tuple.(,) a b -> a
      aOcc    = fst $ localLiftedVanillaId 100 "a"
      aBnd    = snd $ localLiftedVanillaId 100 "a"
      bBnd    = snd $ localLiftedVanillaId 101 "b"
      tOcc    = fst $ localLiftedVanillaId 102 "t"
      tBnd    = snd $ localLiftedVanillaId 102 "t"
      rBnd    = snd $ localLiftedVanillaId 103 "r"

      tuple2Proj0Bnd  = exportedLiftedVanillaId 104 "tuple2Proj0"
      tuple2Proj0     = StgTopLifted $ StgNonRec tuple2Proj0Bnd $ StgRhsClosure [] Updatable [tBnd] $
                          StgCase (StgApp tOcc []) rBnd (AlgAlt tup2TCOcc)
                            [ MkAlt
                              {- altCon      -} (AltDataCon tup2DCOcc)
                              {- altBinders  -} [aBnd, bBnd]
                              {- altRHS      -} (StgApp aOcc [])
                            ]

      -- code for applyFun1Arg = \f p -> f p
      fOcc    = fst $ localLiftedVanillaId 200 "f"
      fBnd    = snd $ localLiftedVanillaId 200 "f"
      pOcc    = fst $ localLiftedVanillaId 201 "p"
      pBnd    = snd $ localLiftedVanillaId 201 "p"

      applyFun1ArgBnd = exportedLiftedVanillaId 202 "applyFun1Arg"
      applyFun1Arg    = StgTopLifted $ StgNonRec applyFun1ArgBnd $ StgRhsClosure [] Updatable [fBnd, pBnd] $
                          StgApp fOcc [StgVarArg pOcc]
