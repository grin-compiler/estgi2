module PrimOp.ByteArray

import Data.SortedMap
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

%foreign "scheme:foreign-alloc"
prim__foreignAlloc : Int -> PrimIO Int

%foreign "scheme:eval-str2"
prim_eval : {a : Type} -> String -> PrimIO a

%foreign "scheme:c-memcpy-ptr"
prim_memcpy : Int -> Int -> Int -> PrimIO Int

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

  -- newByteArray# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "newByteArray#", [IntAtom size, st]) => do
    baIdx <- newByteArray size 1 False
    pure [MutableByteArray baIdx]

  -- writeWord8Array# :: MutableByteArray# s -> Int# -> Word8# -> State# s -> State# s
  ( "writeWord8Array#", [MutableByteArray bai, IntAtom index, WordAtom value, st]) => do
    p <- getByteArrayContentPtr bai.baId
    primIO $ prim_eval "(foreign-set! 'unsigned-8 \{p} \{index} \{value})"
    pure []

  -- shrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> State# s
  ( "shrinkMutableByteArray#", [MutableByteArray bai, IntAtom size, st]) => do
    --ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    --liftIO $ BA.shrinkMutableByteArray baaMutableByteArray size
    -- TODO:
    pure []

  -- copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyByteArray#",
      [ ByteArray srcIdx,        IntAtom offsetSrc
      , MutableByteArray dstIdx, IntAtom offsetDst
      , IntAtom length, st
      ]
   ) => do
    src <- getByteArrayContentPtr srcIdx.baId
    dst <- getByteArrayContentPtr dstIdx.baId
    _ <- primIO $ prim_memcpy (dst + offsetDst) (src + offsetSrc) length
    pure []

  -- FIXME: GHC Core Coercions allow this:
  ( "byteArrayContents#", [MutableByteArray baIdx]) => do
    ptr <- getByteArrayContentPtr baIdx.baId
    pure [PtrAtom (ByteArrayPtr baIdx) ptr]

  _ => fallback op args t tc
