module PrimOp.Word64

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- word64ToWord# :: Word64# -> Word#
  ( "word64ToWord#",  [WordAtom a])            => pure [WordAtom a]

  -- plusWord64# :: Word64# -> Word64# -> Word64#
  ( "plusWord64#",    [WordAtom a, WordAtom b]) => pure [WordAtom $ a + b] -- HINT: WordAtom is Bits64

  _ => fallback op args t tc
