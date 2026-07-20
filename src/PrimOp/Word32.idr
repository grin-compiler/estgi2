module PrimOp.Word32

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = do
 let
    w32 : Bits64 -> Bits32
    w32  = cast
    w : Bits32 -> Bits64
    w   = cast
 case (op, args) of

  -- word32ToWord# :: Word32# -> Word#
  ( "word32ToWord#",  [WordAtom a])          => pure [WordAtom a]

  -- wordToWord32# :: Word# -> Word32#
  ( "wordToWord32#",  [WordAtom a])          => pure [WordAtom . w . w32 $ a]

  _ => fallback op args t tc
