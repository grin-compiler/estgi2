module PrimOp.Addr

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

--%foreign "scheme:peek-elem-off"
--prim_peekElemOff : {a : Type} -> String -> Int -> Int -> PrimIO a
{-
procedure: (foreign-ref type address offset)
-}

%foreign "scheme:eval-str2"
prim_eval : {a : Type} -> String -> PrimIO a

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of
{-
  -- eqAddr# :: Addr# -> Addr# -> Int#
  ( "eqAddr#", [PtrAtom a_o a, PtrAtom b_o b]) => do
    -- HACK, temporary
    putStrLn $ "eqAddr# " ++ show args
    pure [IntAtom $ if a_o == b_o then 1 else 0]
-}
  ( "eqAddr#", [PtrAtom _ a, PtrAtom _ b])         => pure [IntAtom $ if a == b then 1 else 0]

  -- plusAddr# :: Addr# -> Int# -> Addr#
  ( "plusAddr#", [PtrAtom origin p, IntAtom offset])  => pure [PtrAtom origin $ p + offset]

  -- readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int8# #)
  ( "readInt8OffAddr#", [PtrAtom _ p, IntAtom index, st]) => do
    v <- primIO $ prim_eval "(foreign-ref 'integer-8 \{p} \{index})"
    pure [IntAtom v]

  -- indexCharOffAddr# :: Addr# -> Int# -> Char#
  ( "indexCharOffAddr#", [PtrAtom _ p, IntAtom index]) => do
    -- 8 bit char
    v <- primIO $ prim_eval "(foreign-ref 'integer-8 \{p} \{index})"
    putStrLn $ " *** " ++ show (op, args, v, Literal (LitChar $ chr v))
    pure [Literal (LitChar $ chr v)]

  -- writeInt8OffAddr# :: Addr# -> Int# -> Int8# -> State# s -> State# s
  ( "writeInt8OffAddr#", [PtrAtom _ p, IntAtom index, IntAtom value, st]) => do
    primIO $ prim_eval "(foreign-set! 'integer-8 \{p} \{index} \{value})"
    pure []

  -- writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
  ( "writeWideCharOffAddr#", [PtrAtom _ p, IntAtom index, Literal $ LitChar value, st]) => do
    -- 32 bit unicode char
    primIO $ prim_eval "(foreign-set! 'integer-32 \{p} \{4 * index} \{ord value})"
    pure []

  -- readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWideCharOffAddr#", [PtrAtom _ p, IntAtom index, st]) => do
    -- 32 bit unicode char
    v <- primIO $ prim_eval "(foreign-ref 'unsigned-32 \{p} \{4 * index})"
    pure [Literal $ LitChar $ chr v]

  -- writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
  ( "writeAddrOffAddr#", [PtrAtom _ p, IntAtom index, PtrAtom _ value, st]) => do
    primIO $ prim_eval "(foreign-set! 'integer-64 \{p} \{8 * index} \{value})"
    pure []

  -- writeWord64OffAddr# :: Addr# -> Int# -> Word64# -> State# s -> State# s
  ( "writeWord64OffAddr#", [PtrAtom _ p, IntAtom index, WordAtom value, st]) => do
    primIO $ prim_eval "(foreign-set! 'unsigned-64 \{p} \{8 * index} \{value})"
    pure []

  -- writeWord8OffAddr# :: Addr# -> Int# -> Word8# -> State# s -> State# s
  ( "writeWord8OffAddr#", [PtrAtom _ p, IntAtom index, WordAtom value, st]) => do
    primIO $ prim_eval "(foreign-set! 'unsigned-8 \{p} \{index} \{value})"
    pure []

  -- readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word64# #)
  ( "readWord64OffAddr#", [PtrAtom _ p, IntAtom index, st]) => do
    v <- primIO $ prim_eval "(foreign-ref 'unsigned-64 \{p} \{8 * index})"
    pure [WordAtom v]

  _ => fallback op args t tc
