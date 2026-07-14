module PrimOp.Word8

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- word8ToWord# :: Word8# -> Word#
  ( "word8ToWord#",  [WordAtom a])           => pure [WordAtom a]

  _ => fallback op args t tc
