module PrimOp.Word

import Control.Monad.State
import Data.Bits

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- word2Int# :: Word# -> Int#
  ( "word2Int#", [WordAtom a])           => pure [IntAtom $ cast a] -- HINT: noop ; same bit level representation

  -- or# :: Word# -> Word# -> Word#
  ( "or#",   [WordAtom a, WordAtom b])   => pure [WordAtom $ a .|. b]

  -- and# :: Word# -> Word# -> Word#
  ( "and#",  [WordAtom a, WordAtom b])   => pure [WordAtom $ a .&. b]

  _ => fallback op args t tc
