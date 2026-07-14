module FFI

import Control.Monad.State
import Data.SortedSet
import Data.SortedMap
import Data.List
import Data.String
import System

import Stg.Syntax
import Stg.JSON
import Base

import Rts
import FFI.Rts

import GHC.Symbols

rtsSymbolSet : SortedSet StgName
rtsSymbolSet = fromList $ map getSymbolName rtsSymbols

%foreign "scheme:foreign-entry"
prim_foreignEntry : String -> PrimIO Int

export
getFFILabelPtrAtom : StgName -> LabelSpec -> M Atom
getFFILabelPtrAtom labelName labelSpec = do
  lift $ do
    v <- primIO $ prim_foreignEntry labelName
    pure $ PtrAtom (LabelPtr labelName labelSpec) v

{-
getFFISymbol :: Name -> M (FunPtr a)
getFFISymbol name
  | Set.member name rtsSymbolSet
  = case name of
      "enabled_capabilities" -> do
        gets $ castPtrToFunPtr . rtsDataSymbol_enabled_capabilities . ssRtsSupport
      "RtsFlags" -> do
        pure $ error "TODO: deferred error for RtsFlags foreign symbol"
      _ -> do
        stgErrorM $ "native RTS symbol dereference is not implemented yet: " ++ BS8.unpack name
getFFISymbol name = do
  dl <- gets ssCBitsMap
  funPtr <- liftIO . BS8.useAsCString name $ c_dlsym (packDL dl)
  case funPtr == nullFunPtr of
    False -> pure funPtr
    True  -> if Set.member name rtsSymbolSet
      then stgErrorM $ "this RTS symbol is not implemented yet: " ++ BS8.unpack name
      else stgErrorM $ "unknown foreign symbol: " ++ BS8.unpack name

mkFFIArg :: Atom -> M (Maybe FFI.Arg)
mkFFIArg = \case
  Void              -> pure Nothing
  PtrAtom _ p       -> pure . Just $ FFI.argPtr p
  IntV i            -> pure . Just $ FFI.argInt64 $ fromIntegral i
  Int8V i           -> pure . Just $ FFI.argInt8 $ fromIntegral i
  Int16V i          -> pure . Just $ FFI.argInt16 $ fromIntegral i
  Int32V i          -> pure . Just $ FFI.argInt32 $ fromIntegral i
  Int64V i          -> pure . Just $ FFI.argInt64 $ fromIntegral i
  WordV w           -> pure . Just $ FFI.argWord64 $ fromIntegral w
  Word8V w          -> pure . Just $ FFI.argWord8 $ fromIntegral w
  Word16V w         -> pure . Just $ FFI.argWord16 $ fromIntegral w
  Word32V w         -> pure . Just $ FFI.argWord32 $ fromIntegral w
  Word64V w         -> pure . Just $ FFI.argWord64 $ fromIntegral w
  FloatAtom f       -> pure . Just . FFI.argCFloat $ CFloat f
  DoubleAtom d      -> pure . Just . FFI.argCDouble $ CDouble d
  ByteArray bai -> do
    ba <- baaMutableByteArray <$> lookupByteArrayDescriptorI bai
    pure . Just . FFI.argPtr $ BA.mutableByteArrayContents ba
  MutableByteArray bai -> do
    ba <- baaMutableByteArray <$> lookupByteArrayDescriptorI bai
    pure . Just . FFI.argPtr $ BA.mutableByteArrayContents ba
  Literal LitNullAddr -> pure . Just $ FFI.argPtr nullPtr
  a -> error $ "mkFFIArg - unsupported atom: " ++ show a
-}
mkFFIArgTy : Atom -> Maybe String
mkFFIArgTy = \case
  Void                => Nothing
  PtrAtom{}           => Just "void*"
  IntAtom{}           => Just "integer-64"
  WordAtom{}          => Just "unsigned-64"
  {-
  IntV{}              => Just "integer-64"
  Int8V{}             => Just "integer-8"
  Int16V{}            => Just "integer-16"
  Int32V{}            => Just "integer-32"
  Int64V{}            => Just "integer-64"
  WordV{}             => Just "unsigned-64"
  Word8V{}            => Just "unsigned-8"
  Word16V{}           => Just "unsigned-16"
  Word32V{}           => Just "unsigned-32"
  Word64V{}           => Just "unsigned-64"
  -}
  --FloatAtom{}         => Just "single-float"
  DoubleAtom{}        => Just "double-float"
  ByteArray{}         => Just "u8*"
  MutableByteArray{}  => Just "u8*"
  Literal LitNullAddr => Just "u8*"
  a => assert_total $ idris_crash $ "mkFFIArg - unsupported atom: " ++ show a

{-
%foreign "scheme:save-fasl"
prim__save_fasl : {a : Type} -> a -> PrimIO ()

save_fasl : HasIO io => {a : Type} -> a -> io ()
save_fasl val = primIO (prim__save_fasl val)
-}
--public export
data ArgList : Type where [external]

%foreign "scheme:list"
emptyArgs : ArgList

%foreign "scheme:list-cons"
addArg : {a : Type} -> a -> ArgList -> ArgList

addFFIArg : ArgList -> Atom -> ArgList
addFFIArg args = \case
  Void              => args
  PtrAtom _ p       => addArg p args
  IntAtom i         => addArg i args
  WordAtom w        => addArg w args
  --FloatAtom f       -> pure . Just . FFI.argCFloat $ CFloat f
  DoubleAtom d      => addArg d args
  {-
  ByteArray bai -> do
    ba <- baaMutableByteArray <$> lookupByteArrayDescriptorI bai
    pure . Just . FFI.argPtr $ BA.mutableByteArrayContents ba
  MutableByteArray bai -> do
    ba <- baaMutableByteArray <$> lookupByteArrayDescriptorI bai
    pure . Just . FFI.argPtr $ BA.mutableByteArrayContents ba
  -}
  Literal LitNullAddr => addArg 0 args
  a => assert_total $ idris_crash $ "addFFIArg - unsupported atom: " ++ show a

%foreign "scheme:call-ffi"
prim_callFFI : {a : Type} -> String -> ArgList -> PrimIO a

{-
  (foreign-procedure __collect_safe "sleep" (unsigned) unsigned)
-}
callFFI : {a : Type} -> String -> String -> List String -> ArgList -> IO a
callFFI funPtr retTy cArgTys cArgs = do
  primIO (prim_callFFI "(foreign-procedure #f \{show funPtr} (\{unwords cArgTys}) \{retTy})" cArgs)

evalForeignCall : String -> List String -> ArgList -> StgType -> IO (List Atom)
evalForeignCall funPtr cArgTys cArgs retType = case retType of
  UnboxedTuple [] => do
    result <- callFFI {a=()} funPtr "void" cArgTys cArgs
    pure []

  UnboxedTuple [IntRep] => do
    result <- callFFI funPtr "integer-64" cArgTys cArgs
    pure [IntAtom result]

  UnboxedTuple [Int8Rep] => do
    result <- callFFI funPtr "integer-8" cArgTys cArgs
    pure [IntAtom result]

  UnboxedTuple [Int16Rep] => do
    result <- callFFI funPtr "integer-16" cArgTys cArgs
    pure [IntAtom result]

  UnboxedTuple [Int32Rep] => do
    result <- callFFI funPtr "integer-32" cArgTys cArgs
    pure [IntAtom result]

  UnboxedTuple [Int64Rep] => do
    result <- callFFI funPtr "integer-64" cArgTys cArgs
    pure [IntAtom result]

  UnboxedTuple [WordRep] => do
    result <- callFFI funPtr "unsigned-64" cArgTys cArgs
    pure [WordAtom result]

  UnboxedTuple [Word8Rep] => do
    result <- callFFI funPtr "unsigned-8" cArgTys cArgs
    pure [WordAtom result]

  UnboxedTuple [Word16Rep] => do
    result <- callFFI funPtr "unsigned-16" cArgTys cArgs
    pure [WordAtom result]

  UnboxedTuple [Word32Rep] => do
    result <- callFFI funPtr "unsigned-32" cArgTys cArgs
    pure [WordAtom result]

  UnboxedTuple [Word64Rep] => do
    result <- callFFI funPtr "unsigned-64" cArgTys cArgs
    pure [WordAtom result]

  UnboxedTuple [AddrRep] => do
    result <- callFFI funPtr "void*" cArgTys cArgs
    pure [PtrAtom RawPtr result]
  {-
  UnboxedTuple [FloatRep] -> do
    CFloat result <- FFI.callFFI funPtr "single-float" cArgTys cArgs
    pure [FloatAtom result]
  -}
  UnboxedTuple [DoubleRep] => do
    result <- callFFI funPtr "double-float" cArgTys cArgs
    pure [DoubleAtom result]

  _ => die $ "unsupported retType: " ++ show retType

export
evalFCallOp : EvalOnNewThread -> ForeignCall -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalFCallOp evalOnNewThread fCall@(MkForeignCall foreignCTarget foreignCConv foreignCSafety) args t tc = do
    --liftIO $ putStrLn $ "[evalFCallOp]  " ++ show foreignCTarget ++ " " ++ show args
    case foreignCTarget of

      ----------------
      -- GHC RTS FFI
      ----------------
      {-
      -- support for exporting haskell function (GHC RTS specific)
      StaticTarget _ "createAdjustor" _ _
        | [ IntV 1
          , PtrAtom StablePtr{} sp
          , Literal (LitLabel wrapperName _)
          , PtrAtom CStringPtr{} _
          , Void
          ] <- args
        , UnboxedTuple [AddrRep] <- t
        -> do
          --promptM $ putStrLn $ "[createAdjustor FFI]"
          fun@HeapPtr{} <- lookupStablePointerPtr sp
          cwrapperDesc <- lookupCWrapperHsType wrapperName
          -- FIXME: _freeWrapper needs to be called otherwise it will leak the memory!!!!
          (funPtr, _freeWrapper) <- createAdjustor evalOnNewThread fun cwrapperDesc
          pure [PtrAtom RawPtr $ castFunPtrToPtr funPtr]

      -- GHC RTS global store getOrSet function implementation
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol globalStoreSymbols
        , [value, Void] <- args
        -> do
            --promptM $ putStrLn $ "[global store FFI] " ++ show foreignSymbol
            -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
            store <- gets $ rtsGlobalStore . ssRtsSupport
            case Map.lookup foreignSymbol store of
              Nothing -> state $ \s@StgState{..} -> ([value], s {ssRtsSupport = ssRtsSupport {rtsGlobalStore = Map.insert foreignSymbol value store}})
              Just v  -> pure [v]
      -}
      StaticTarget _ foreignSymbol _ _ =>
        -- GHC RTS global store getOrSet function implementation
        if contains foreignSymbol globalStoreSymbols then do
          let [value, Void] = args | _ => stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args
          -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
          store <- gets $ rtsGlobalStore . ssRtsSupport
          case lookup foreignSymbol store of
            Nothing => state $ \s => ({ssRtsSupport.rtsGlobalStore := insert foreignSymbol value store} s, [value])
            Just v  => pure [v]

        -- calls to GHC RTS
        else if contains foreignSymbol rtsSymbolSet then do
          FFI.Rts.evalFCallOp evalOnNewThread fCall args t tc

        -- user FFI ; only unsafe calls for now
        else if foreignCSafety == PlayRisky then do
          let cArgTys = catMaybes $ map mkFFIArgTy args
              cArgs   = foldl addFFIArg emptyArgs $ reverse args
          lift $ evalForeignCall foreignSymbol cArgTys cArgs t

        else stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

      {-
      -- calls to emulated lib native functions
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol emulatedLibrarySymbolSet
        -> do
          --promptM $ putStrLn $ "[emulated user FFI] " ++ show foreignSymbol
          EmulatedLibFFI.evalFCallOp evalOnNewThread fCall args t tc

      --------------
      -- user FFI
      --------------
      StaticTarget _ foreignSymbol _ _
        -> do
          let blacklist =
                [ "__gmpn"
                ]
          { -
          unless (any (`BS8.isPrefixOf` foreignSymbol) blacklist) $ do
            liftIO $ do
              now <- getCurrentTime
              putStrLn $ "[foreign call]  " ++ show now ++ "  " ++ show foreignSymbol ++ " " ++ show args ++ show foreignCTarget
          - }
          --promptM $ putStrLn $ "[user FFI] " ++ show foreignSymbol
          ts <- getCurrentThreadState
          unless (tsLabel ts == Just "resource-pool: reaper") $ do
            traceLog $ show foreignSymbol ++ "\t" ++ show args

          cArgs <- catMaybes <$> mapM mkFFIArg args
          funPtr <- getFFISymbol foreignSymbol
          liftIOAndBorrowStgState $ do
            evalForeignCall funPtr cArgs t

      DynamicTarget
        | (PtrAtom RawPtr funPtr) : funArgs <- args
        -> do
          cArgs <- catMaybes <$> mapM mkFFIArg funArgs
          liftIOAndBorrowStgState $ do
            evalForeignCall (castPtrToFunPtr funPtr) cArgs t
      -}
      _ => stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args
