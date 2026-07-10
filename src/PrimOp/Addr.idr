module PrimOp.Addr

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- eqAddr# :: Addr# -> Addr# -> Int#
  ( "eqAddr#", [PtrAtom _ a, PtrAtom _ b])         => pure [IntAtom $ if a == b then 1 else 0]

  _ => fallback op args t tc
