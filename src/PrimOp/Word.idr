module PrimOp.Word

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- word2Int# :: Word# -> Int#
  ( "word2Int#", [WordAtom a])           => pure [IntAtom $ cast a] -- HINT: noop ; same bit level representation

  _ => fallback op args t tc
