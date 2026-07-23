module PrimOp.Int

import Control.Monad.State
import Data.Bits

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- <=# :: Int# -> Int# -> Int#
  ( "<=#", [IntAtom a, IntAtom b]) => pure [IntAtom $ if a <= b then 1 else 0]

  -- +# :: Int# -> Int# -> Int#
  ( "+#", [IntAtom a, IntAtom b]) => pure [IntAtom $ a + b]

  -- -# :: Int# -> Int# -> Int#
  ( "-#", [IntAtom a, IntAtom b]) => pure [IntAtom $ a - b]

  -- chr# :: Int# -> Char#
  ( "chr#",                [IntAtom a]) => pure [Literal (LitChar $ cast a)] -- HINT: noop ; same bit level representation

  -- andI# :: Int# -> Int# -> Int#
  ( "andI#",           [IntAtom a, IntAtom b]) => pure [IntAtom $ a .&. b]

  -- *# :: Int# -> Int# -> Int#
  ( "*#", [IntAtom a, IntAtom b]) => pure [IntAtom $ a * b]

  -- >=# :: Int# -> Int# -> Int#
  ( ">=#", [IntAtom a, IntAtom b]) => pure [IntAtom $ if a >= b then 1 else 0]

  -- <# :: Int# -> Int# -> Int#
  ( "<#",  [IntAtom a, IntAtom b]) => pure [IntAtom $ if a < b  then 1 else 0]

  -- ># :: Int# -> Int# -> Int#
  ( ">#",  [IntAtom a, IntAtom b]) => pure [IntAtom $ if a > b  then 1 else 0]

  -- negateInt# :: Int# -> Int#
  ( "negateInt#",      [IntAtom a]) => pure [IntAtom (-a)]

  -- uncheckedIShiftL# :: Int# -> Int# -> Int#
  ( "uncheckedIShiftL#",   [IntAtom a, IntAtom b]) => do
    let Just ix = integerToFin (cast b) 64 | _ => stg_error "uncheckedIShiftL# bound"
    pure [IntAtom $ shiftL a ix]

  -- uncheckedIShiftRA# :: Int# -> Int# -> Int#
  ( "uncheckedIShiftRA#",  [IntAtom a, IntAtom b]) => do
    let Just ix = integerToFin (cast b) 64 | _ => stg_error "uncheckedIShiftRA# bound"
    pure [IntAtom $ shiftR a ix] -- Shift right arithmetic

  -- ==# :: Int# -> Int# -> Int#
  ( "==#", [IntAtom a, IntAtom b]) => pure [IntAtom $ if a == b then 1 else 0]

  -- int2Word# :: Int# -> Word#
  ( "int2Word#",           [IntAtom a]) => pure [WordAtom $ cast a] -- HINT: noop ; same bit level representation

  -- int2Double# :: Int# -> Double#
  ( "int2Double#",         [IntAtom a] ) => pure [DoubleAtom $ cast a]

  -- quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
  ( "quotRemInt#",     [IntAtom a, IntAtom b]) => pure [IntAtom $ a `div` b, IntAtom $ a `mod` b]

  -- int2Float# :: Int# -> Float#
  ( "int2Float#",          [IntAtom a] ) => pure [FloatAtom $ cast a]

  -- addIntC# :: Int# -> Int# -> (# Int#, Int# #)
  ( "addIntC#",        [IntAtom a, IntAtom b]) => do
    let int64Max : Integer
        int64Max = 9223372036854775807

        int64Min : Integer
        int64Min = -9223372036854775808

        carry : Integer -> Int
        carry x = if x < int64Min || x > int64Max then 1 else 0
    pure [IntAtom $ a + b, IntAtom . carry $ cast a + cast b]

  _ => fallback op args t tc
