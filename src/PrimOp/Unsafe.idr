module PrimOp.Unsafe

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- reallyUnsafePtrEquality# :: a -> b -> Int#
  ( "reallyUnsafePtrEquality#", [a, b]) => do
    pure [IntAtom $ if a == b then 1 else 0]

  _ => fallback op args t tc
