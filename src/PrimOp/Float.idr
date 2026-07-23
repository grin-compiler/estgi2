module PrimOp.Float

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

%foreign "scheme:decode-float-to-tuple"
prim_decodeFloat : () -> Double -> (Int, Int, Int)

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- negateFloat# :: Float# -> Float#
  ( "negateFloat#",  [FloatAtom a]) => pure [FloatAtom (-a)]

  -- minusFloat# :: Float# -> Float# -> Float#
  ( "minusFloat#",   [FloatAtom a, FloatAtom b]) => pure [FloatAtom $ a - b]

  -- eqFloat# :: Float# -> Float# -> Int#
  ( "eqFloat#",      [FloatAtom a, FloatAtom b]) => pure [IntAtom $ if a == b then 1 else 0]

  -- gtFloat# :: Float# -> Float# -> Int#
  ( "gtFloat#",      [FloatAtom a, FloatAtom b]) => pure [IntAtom $ if a > b  then 1 else 0]

  -- ltFloat# :: Float# -> Float# -> Int#
  ( "ltFloat#",      [FloatAtom a, FloatAtom b]) => pure [IntAtom $ if a < b  then 1 else 0]

  -- timesFloat# :: Float# -> Float# -> Float#
  ( "timesFloat#",   [FloatAtom a, FloatAtom b]) => pure [FloatAtom $ a * b]

  -- plusFloat# :: Float# -> Float# -> Float#
  ( "plusFloat#",    [FloatAtom a, FloatAtom b]) => pure [FloatAtom $ a + b]

  -- divideFloat# :: Float# -> Float# -> Float#
  ( "divideFloat#",  [FloatAtom a, FloatAtom b]) => pure [FloatAtom $ a / b]

  -- atanFloat# :: Float# -> Float#
  ( "atanFloat#",    [FloatAtom a]) => pure [FloatAtom $ atan a]

  -- leFloat# :: Float# -> Float# -> Int#
  ( "leFloat#",      [FloatAtom a, FloatAtom b]) => pure [IntAtom $ if a <= b then 1 else 0]

  -- fabsFloat# :: Float# -> Float#
  ( "fabsFloat#",    [FloatAtom a]) => pure [FloatAtom (abs a)]

  -- geFloat# :: Float# -> Float# -> Int#
  ( "geFloat#",      [FloatAtom a, FloatAtom b]) => pure [IntAtom $ if a >= b then 1 else 0]

  -- sinFloat# :: Float# -> Float#
  ( "sinFloat#",     [FloatAtom a]) => pure [FloatAtom $ sin a]

  -- cosFloat# :: Float# -> Float#
  ( "cosFloat#",     [FloatAtom a]) => pure [FloatAtom $ cos a]

  -- decodeFloat_Int# :: Float# -> (# Int#, Int# #)
  ( "decodeFloat_Int#", [FloatAtom a]) => do
    let (mantissa, exponent, sign) = prim_decodeFloat (believe_me (1, 2, 3)) a
    pure [IntAtom mantissa, IntAtom exponent]

  _ => fallback op args t tc
