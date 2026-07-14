module PrimOp.ByteArray

import Data.SortedMap
import Data.Buffer
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

%foreign "scheme:lock-object"
prim__lockBuffer : Buffer -> PrimIO ()

newByteArray : Int -> Int -> Bool -> M ByteArrayIdx
newByteArray size alignment pinned = do
  -- HINT: the implementation always uses pinned byte array because the primop implementation is not atomic
  --        GC may occur and the content data pointer must stay in place
  --        but this is only an interpreter implementation constraint
  Just ba <- lift $ newBuffer size
    | _ => stg_error "newBuffer - alloc"
  primIO $ prim__lockBuffer ba
  -- hint: idris buffer is initialized with zeros

  next <- gets ssNextMutableByteArray
  let desc = MkByteArrayDescriptor
        { baaMutableByteArray = ba
        , baaByteArray        = Nothing
        , baaPinned           = pinned
        , baaAlignment        = alignment
        }

  modify {ssMutableByteArrays $= insert next desc, ssNextMutableByteArray := 1 + next}

  pure $ MkByteArrayIdx
    { baId        = next
    , baPinned    = pinned
    , baAlignment = alignment
    }

%foreign "scheme:bytevector-contents"
prim__bytevectorContents : Buffer -> (offset : Int) -> PrimIO Int

getByteArrayContentPtr : Int -> M Int
getByteArrayContentPtr i = do
  d <- lookupByteArrayDescriptor i
  primIO $ prim__bytevectorContents d.baaMutableByteArray 0

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- newAlignedPinnedByteArray# :: Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "newAlignedPinnedByteArray#", [IntAtom size, IntAtom alignment, st]) => do
    baIdx <- newByteArray size alignment True
    pure [MutableByteArray baIdx]

  -- unsafeFreezeByteArray# :: MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
  ( "unsafeFreezeByteArray#", [MutableByteArray baIdx, st]) => do
    desc <- lookupByteArrayDescriptor baIdx.baId
    case desc.baaByteArray of
      Just{}  => pure ()
      Nothing => do
        let ba = desc.baaMutableByteArray
        let newDesc = {baaByteArray := Just ba} desc
        modify {ssMutableByteArrays $= insert baIdx.baId newDesc}
    pure [ByteArray baIdx]

  -- byteArrayContents# :: ByteArray# -> Addr#
  ( "byteArrayContents#", [ByteArray baIdx]) => do
    ptr <- getByteArrayContentPtr baIdx.baId
    pure [PtrAtom (ByteArrayPtr baIdx) ptr]

  -- mutableByteArrayContents# :: MutableByteArray# t0 -> Addr#
  ( "mutableByteArrayContents#", [MutableByteArray baIdx]) => do
    ptr <- getByteArrayContentPtr baIdx.baId
    pure [PtrAtom (ByteArrayPtr baIdx) ptr]

  -- newPinnedByteArray# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "newPinnedByteArray#", [IntAtom size, st]) => do
    baIdx <- newByteArray size 1 True
    pure [MutableByteArray baIdx]

  _ => fallback op args t tc
