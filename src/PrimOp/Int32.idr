module PrimOp.Int32

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = do
 let
    i32 : Int -> Int32
    i32  = cast
    i : Int32 -> Int
    i   = cast
 case (op, args) of

  -- int32ToInt# :: Int32# -> Int#
  ( "int32ToInt#",    [IntAtom a])           => pure [IntAtom a]

  -- intToInt32# :: Int# -> Int32#
  ( "intToInt32#",    [IntAtom a])           => pure [IntAtom . i $ i32 a]

  -- plusInt32# :: Int32# -> Int32# -> Int32#
  ( "plusInt32#",     [IntAtom a, IntAtom b]) => pure [IntAtom . i $ i32 a + i32 b]

  -- int32ToWord32# :: Int32# -> Word32#
  ( "int32ToWord32#", [IntAtom a])           => pure [WordAtom $ cast a]

  _ => fallback op args t tc
