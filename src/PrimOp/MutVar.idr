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

  _ => fallback op args t tc
