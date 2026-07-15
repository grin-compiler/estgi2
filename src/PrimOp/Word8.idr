module PrimOp.Word8

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = do
 let
    w8 : Bits64 -> Bits8
    w8  = cast
    w : Bits8 -> Bits64
    w   = cast
 case (op, args) of

  -- word8ToWord# :: Word8# -> Word#
  ( "word8ToWord#",  [WordAtom a])           => pure [WordAtom a]

  -- wordToWord8# :: Word# -> Word8#
  ( "wordToWord8#",  [WordAtom a])           => pure [WordAtom . w . w8 $ a]

  _ => fallback op args t tc
