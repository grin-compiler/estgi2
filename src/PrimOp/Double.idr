module PrimOp.Double

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- /## :: Double# -> Double# -> Double#
  ( "/##", [DoubleAtom a, DoubleAtom b]) => pure [DoubleAtom $ a / b]

  -- negateDouble# :: Double# -> Double#
  ( "negateDouble#", [DoubleAtom a]) => pure [DoubleAtom (-a)]

  -- -## :: Double# -> Double# -> Double#
  ( "-##", [DoubleAtom a, DoubleAtom b]) => pure [DoubleAtom $ a - b]

  -- <## :: Double# -> Double# -> Int#
  ( "<##", [DoubleAtom a, DoubleAtom b]) => pure [IntAtom $ if a < b  then 1 else 0]

  -- double2Float# :: Double# -> Float#
  ( "double2Float#", [DoubleAtom a]) => pure [FloatAtom $ cast a]

  _ => fallback op args t tc
