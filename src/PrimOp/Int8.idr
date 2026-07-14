module PrimOp.Int8

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = do
 let
    i8 : Int -> Int8
    i8  = cast
    i : Int8 -> Int
    i   = cast
 case (op, args) of

  -- int8ToInt# :: Int8# -> Int#
  ( "int8ToInt#",    [IntAtom a])           => pure [IntAtom a]

  -- int8ToWord8# :: Int8# -> Word8#
  ( "int8ToWord8#",  [IntAtom a])           => pure [WordAtom $ cast a]

  -- intToInt8# :: Int# -> Int8#
  ( "intToInt8#",    [IntAtom a])           => pure [IntAtom . i $ i8 a]

  _ => fallback op args t tc
