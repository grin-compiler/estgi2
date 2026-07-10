module PrimOp.StablePointer

import Data.SortedMap
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- makeStablePtr# :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  ( "makeStablePtr#", [a, st]) => do
    next <- gets ssNextStablePointer
    modify {ssStablePointers $= insert next a, ssNextStablePointer := 1 + next}
    pure [PtrAtom (StablePtr next) next]

  _ => fallback op args t tc
