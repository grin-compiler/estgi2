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

  _ => fallback op args t tc
