module PrimOp.ByteArray

import Data.SortedMap
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

%foreign "scheme:foreign-alloc"
prim__foreignAlloc : Int -> PrimIO Int

newByteArray : Int -> Int -> Bool -> M ByteArrayIdx
newByteArray size alignment pinned = do
  -- HINT: the implementation always uses pinned byte array because the primop implementation is not atomic
  --        GC may occur and the content data pointer must stay in place
  --        but this is only an interpreter implementation constraint
  ba <- primIO $ prim__foreignAlloc size
  -- TODO: clear with zeros?

  next <- gets ssNextMutableByteArray
  let desc = MkByteArrayDescriptor
        { baaMutableByteArray = ba
        , baaByteArray        = Nothing
        , baaPinned           = pinned
        , baaAlignment        = alignment
        , baaSize             = size
        }

  modify {ssMutableByteArrays $= insert next desc, ssNextMutableByteArray := 1 + next}

  pure $ MkByteArrayIdx
    { baId        = next
    , baPinned    = pinned
    , baAlignment = alignment
    }

getByteArrayContentPtr : Int -> M Int
getByteArrayContentPtr i = do
  d <- lookupByteArrayDescriptor i
  pure d.baaMutableByteArray

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
