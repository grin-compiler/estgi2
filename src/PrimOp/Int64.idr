module PrimOp.Int64

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = do
 let
    i64 : Int -> Int64
    i64  = cast
    i : Int64 -> Int
    i   = cast
 case (op, args) of

  -- intToInt64# :: Int# -> Int64#
  ( "intToInt64#",    [IntAtom a])             => pure [IntAtom . i $ i64 a]

  -- int64ToWord64# :: Int64# -> Word64#
  ( "int64ToWord64#", [IntAtom a])           => pure [WordAtom $ cast a]

  -- int64ToInt# :: Int64# -> Int#
  ( "int64ToInt#",    [IntAtom a])           => pure [IntAtom a]

  _ => fallback op args t tc
