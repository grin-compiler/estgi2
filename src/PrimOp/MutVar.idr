module PrimOp.MutVar

import Data.SortedMap
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- newMutVar# :: a -> State# s -> (# State# s, MutVar# s a #)
  ( "newMutVar#", [a, st]) => do
    next <- gets ssNextMutVar
    modify {ssMutVars $= insert next a, ssNextMutVar := 1 + next}
    pure [MutVar next]

  -- readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)
  ( "readMutVar#", [MutVar m, st]) => do
    a <- lookupMutVar m
    pure [a]

  -- writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
  ( "writeMutVar#", [MutVar m, a, st]) => do
    _ <- lookupMutVar m -- check existence
    modify {ssMutVars $= insert m a}
    pure []

  -- atomicModifyMutVar2# :: MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
  ( "atomicModifyMutVar2#", [MutVar m, fun, st]) => do
    w <- getWiredIns
    -- NOTE: CPU atomic
    old <- lookupMutVar m

    -- transform through fun, get a pair result
    Closure hoIsLNE hoName hoCloBody hoEnv hoCloArgs hoCloMissing <- readHeapClosure w.rtsApplyFun1Arg
      | ho => stg_error $ "atomicModifyMutVar2# - expected Closure, got: " ++ show ho
    lazyNewTup2Value <- HeapPtr <$> allocAndStore (Closure hoIsLNE hoName hoCloBody hoEnv [fun, old] 0)

    -- get the first value of the pair
    Closure hoIsLNE hoName hoCloBody hoEnv hoCloArgs hoCloMissing <- readHeapClosure w.rtsTuple2Proj0
      | ho => stg_error $ "atomicModifyMutVar2# - expected Closure, got: " ++ show ho
    lazyNewMutVarValue <- HeapPtr <$> allocAndStore (Closure hoIsLNE hoName hoCloBody hoEnv [lazyNewTup2Value] 0)

    -- update mutvar
    modify {ssMutVars $= insert m lazyNewMutVarValue}
    pure [old, lazyNewTup2Value]

  _ => fallback op args t tc
