module PrimOp.Int32

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- int32ToInt# :: Int32# -> Int#
  ( "int32ToInt#",    [IntAtom a])           => pure [IntAtom a]

  _ => fallback op args t tc
