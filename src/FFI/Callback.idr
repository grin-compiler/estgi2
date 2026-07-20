module FFI.Callback

import Control.Monad.State
import Data.SortedMap
import Data.List
import System

import Stg.Syntax
import Stg.JSON
import Base

%foreign "scheme:eval-str2"
prim_eval : {a : Type} -> String -> PrimIO a

mkWiredInCon : (WiredIns -> DataCon) -> List Atom -> M Atom
mkWiredInCon conFun args = do
  HeapPtr <$> allocAndStore (Con False (MkDC (conFun !getWiredIns)) args)

unboxFFIAtom : StgName -> Atom -> M Atom
unboxFFIAtom hsFFIType a = case (hsFFIType, a) of
  ("()",      HeapPtr{})  => pure Void
  ("Unit",    HeapPtr{})  => pure Void
  ("Int",     HeapPtr{})  => con1Unbox
  ("Int32",   HeapPtr{})  => con1Unbox
  ("Double",  HeapPtr{})  => con1Unbox
  -- TODO: make this complete
  x => stg_error $ "unboxFFIAtom - unknown pattern: " ++ show x
 where
  con1Unbox = do
    readHeap a >>= \case
      Con _ _ [x] => pure x
      o => stg_error $ "unboxFFIAtom " ++ show (hsFFIType, a, o)

boxFFIAtom : StgName -> Atom -> M Atom
boxFFIAtom hsFFIType a = case (hsFFIType, a) of
  -- boxed Char
  ("Char", WordAtom{})  => mkWiredInCon rtsCharCon    [a]

  -- boxed Ints
  ("Int", IntAtom{})    => mkWiredInCon rtsIntCon     [a]
  ("Int8", IntAtom{})   => mkWiredInCon rtsInt8Con    [a]
  ("Int16", IntAtom{})  => mkWiredInCon rtsInt16Con   [a]
  ("Int32", IntAtom{})  => mkWiredInCon rtsInt32Con   [a]
  ("Int64", IntAtom{})  => mkWiredInCon rtsInt64Con   [a]

  -- boxed Words
  ("Word", WordAtom{})   => mkWiredInCon rtsWordCon    [a]
  ("Word8", WordAtom{})  => mkWiredInCon rtsWord8Con   [a]
  ("Word16", WordAtom{}) => mkWiredInCon rtsWord16Con  [a]
  ("Word32", WordAtom{}) => mkWiredInCon rtsWord32Con  [a]
  ("Word64", WordAtom{}) => mkWiredInCon rtsWord64Con  [a]

  ("Ptr", PtrAtom RawPtr _)       => mkWiredInCon rtsPtrCon     [a]
  ("FunPtr", PtrAtom RawPtr _)    => mkWiredInCon rtsFunPtrCon  [a]

  ("Float", FloatAtom _)          => mkWiredInCon rtsFloatCon   [a]
  ("Double", DoubleAtom _)        => mkWiredInCon rtsDoubleCon  [a]

  ("StablePtr", PtrAtom RawPtr _) => mkWiredInCon rtsStablePtrCon   [a]
  ("Bool", IntAtom i)             => mkWiredInCon (if i == 0 then rtsFalseCon else rtsTrueCon) []
  ("String", PtrAtom RawPtr _)    => stg_error "TODO: support C string FFI arg boxing"

  x => stg_error $ "boxFFIAtom - unknown pattern: " ++ show x

public export
CWrapperDesc : Type
CWrapperDesc = (Bool, StgName, List StgName)

export
lookupCWrapperHsType : StgName -> M CWrapperDesc
lookupCWrapperHsType name = do
  lookup name <$> gets ssCWrapperHsTypeMap >>= \case
    Nothing => stgErrorM $ "unknown CWrapper label: " ++ show name
    Just a  => pure a

{-
  idris bug:

:exec putStrLn $ show [a | a@True <- [False]]
[False]

-}

export
buildCWrapperHsTypeMap : List Module -> M ()
buildCWrapperHsTypeMap mods =  do
  let v : List (String, CWrapperDesc)
      v = do
        MkModule _ _ _ _ moduleForeignStubs _ _ _ _ _ <- mods
        let MkForeignStubs _ _ _ _ fsDecls = moduleForeignStubs | _ => empty
        StubDeclImport _ x <- fsDecls | _ => empty
        let Just (StubImplImportCWrapper name _ isIOCall retType argTypes) = x | _ => empty
        pure (name, (isIOCall, retType, argTypes))

      check : SortedMap StgName CWrapperDesc -> (StgName, CWrapperDesc) -> M (SortedMap StgName CWrapperDesc)
      check s (k, a) = do
        let Nothing = lookup k s
              | Just b => stg_error $ "CWrapper name duplication: " ++ show k ++ " with hsTypes: " ++ show (a, b)
        pure $ insert k a s
  m <- foldlM check empty v
  modify {ssCWrapperHsTypeMap := m}
  putStrLn $ "CWrappers:"
  for_ (kvList m) $ putStrLn . show


-- NOTE: LiftedRep and UnliftedRep is not used in FFIRep only AddrRep
record FFIRep where
  constructor MkFFIRep
  unFFIRep : PrimRep

ffiTypeToFFIRep : StgName -> FFIRep
ffiTypeToFFIRep = MkFFIRep . \case
  "Unit"      => VoidRep
  "()"        => VoidRep
  "Char"      => WordRep
  "Int"       => IntRep
  "Int8"      => Int8Rep
  "Int16"     => Int16Rep
  "Int32"     => Int32Rep
  "Int64"     => Int64Rep
  "Word"      => WordRep
  "Word8"     => Word8Rep
  "Word16"    => Word16Rep
  "Word32"    => Word32Rep
  "Word64"    => Word64Rep
  "Ptr"       => AddrRep
  "FunPtr"    => AddrRep
  "Float"     => FloatRep
  "Double"    => DoubleRep
  "StablePtr" => AddrRep
  "Bool"      => AddrRep
  "String"    => AddrRep

  -- additional allowed ffi import types
  "Array#"              => AddrRep
  "MutableArray#"       => AddrRep

  "SmallArray#"         => AddrRep
  "MutableSmallArray#"  => AddrRep

  "ArrayArray#"         => AddrRep
  "MutableArrayArray#"  => AddrRep

  "ByteArray#"          => AddrRep
  "MutableByteArray#"   => AddrRep

  x => assert_total $ idris_crash $ "ffiTypeToFFIRep - unsupported: " ++ show x
{-
ffiRepToCType :: FFIRep -> Ptr FFI.CType
ffiRepToCType (FFIRep r) = case r of
  VoidRep     -> FFI.ffi_type_void
  LiftedRep   -> FFI.ffi_type_pointer
  UnliftedRep -> FFI.ffi_type_pointer
  Int8Rep     -> FFI.ffi_type_sint8
  Int16Rep    -> FFI.ffi_type_sint16
  Int32Rep    -> FFI.ffi_type_sint32
  Int64Rep    -> FFI.ffi_type_sint64
  IntRep      -> FFI.ffi_type_sint64
  Word8Rep    -> FFI.ffi_type_uint8
  Word16Rep   -> FFI.ffi_type_uint16
  Word32Rep   -> FFI.ffi_type_uint32
  Word64Rep   -> FFI.ffi_type_uint64
  WordRep     -> FFI.ffi_type_uint64
  AddrRep     -> FFI.ffi_type_pointer
  FloatRep    -> FFI.ffi_type_float
  DoubleRep   -> FFI.ffi_type_double
  rep         -> error $ "ffiRepToCType - unsupported: " ++ show rep
-}

ffiRepToGetter : FFIRep -> Int -> IO Atom
ffiRepToGetter (MkFFIRep r) p = case r of
  VoidRep     => pure Void
  Int64Rep    => IntAtom <$> primIO (prim_eval "(foreign-ref 'integer-64 \{p} 0)")
  Int32Rep    => IntAtom <$> primIO (prim_eval "(foreign-ref 'integer-32 \{p} 0)")
  Int16Rep    => IntAtom <$> primIO (prim_eval "(foreign-ref 'integer-16 \{p} 0)")
  Int8Rep     => IntAtom <$> primIO (prim_eval "(foreign-ref 'integer-8 \{p} 0)")
  IntRep      => IntAtom <$> primIO (prim_eval "(foreign-ref 'integer-64 \{p} 0)")
  Word64Rep   => WordAtom <$> primIO (prim_eval "(foreign-ref 'unsigned-64 \{p} 0)")
  Word32Rep   => WordAtom <$> primIO (prim_eval "(foreign-ref 'unsigned-32 \{p} 0)")
  Word16Rep   => WordAtom <$> primIO (prim_eval "(foreign-ref 'unsigned-16 \{p} 0)")
  Word8Rep    => WordAtom <$> primIO (prim_eval "(foreign-ref 'unsigned-8 \{p} 0)")
  WordRep     => WordAtom <$> primIO (prim_eval "(foreign-ref 'unsigned-64 \{p} 0)")
  AddrRep     => PtrAtom RawPtr <$> primIO (prim_eval "(foreign-ref 'integer-64 \{p} 0)")
  FloatRep    => FloatAtom  <$> primIO (prim_eval "(foreign-ref 'single-float \{p} 0)")
  DoubleRep   => DoubleAtom <$> primIO (prim_eval "(foreign-ref 'double-float \{p} 0)")
  rep         => die $ "ffiRepToGetter - unsupported: " ++ show rep

ffiRepToSetter : FFIRep -> Int -> Atom -> StgName -> IO ()
ffiRepToSetter (MkFFIRep r) p a retTypeName = case (r, a) of
  (VoidRep,   Void)          => pure ()
  (FloatRep,  FloatAtom v)   => primIO $ prim_eval "(foreign-set! 'single-float \{p} 0 \{show v})"
  (DoubleRep, DoubleAtom v)  => primIO $ prim_eval "(foreign-set! 'double-float \{p} 0 \{show v})"
  (Int64Rep,  IntAtom v)     => primIO $ prim_eval "(foreign-set! 'integer-64 \{p} 0 \{v})"
  (Int32Rep,  IntAtom v)     => primIO $ prim_eval "(foreign-set! 'integer-32 \{p} 0 \{v})"
  (Int16Rep,  IntAtom v)     => primIO $ prim_eval "(foreign-set! 'integer-16 \{p} 0 \{v})"
  (Int8Rep,   IntAtom v)     => primIO $ prim_eval "(foreign-set! 'integer-8 \{p} 0 \{v})"
  (IntRep,    IntAtom v)     => primIO $ prim_eval "(foreign-set! 'integer-64 \{p} 0 \{v})"
  (Word64Rep, WordAtom v)    => primIO $ prim_eval "(foreign-set! 'unsigned-64 \{p} 0 \{v})"
  (Word32Rep, WordAtom v)    => primIO $ prim_eval "(foreign-set! 'unsigned-32 \{p} 0 \{v})"
  (Word16Rep, WordAtom v)    => primIO $ prim_eval "(foreign-set! 'unsigned-16 \{p} 0 \{v})"
  (Word8Rep,  WordAtom v)    => primIO $ prim_eval "(foreign-set! 'unsigned-8 \{p} 0 \{v})"
  (WordRep,   WordAtom v)    => primIO $ prim_eval "(foreign-set! 'unsigned-64 \{p} 0 \{v})"
  (AddrRep,   PtrAtom RawPtr v) => primIO $ prim_eval "(foreign-set! 'integer-64 \{p} 0 \{v})"
  x => die $ "ffiRepToSetter - unsupported: " ++ show (x, retTypeName)

--IDEA: adjustor is a dep typed function where the arity varies according to the type signature
export
createAdjustor : EvalOnNewThread -> Atom -> (Bool, StgName, List StgName) -> M (Int, IO ())
createAdjustor evalOnNewThread fun cwrapperDesc@(_, retTy, argTys) = do
  putStrLn $ "created adjustor: " ++ show fun ++ " " ++ show cwrapperDesc
  let str = """
        (let ([x (foreign-callable
                   (lambda (x y) (pretty-print (cons x (* y 2))))
                   (string integer-32)
                   void)])
          (lock-object x)
          (foreign-callable-entry-point x))
        """
      x : M Int
      x = primIO $ prim_eval str
  i <- x
  putStrLn $ "cb addr: " ++ show i
  ?todo_adjustor
  {-
  let (retCType :: argsCType) = map (ffiRepToCType . ffiTypeToFFIRep) $ retTy :: argTys
  stateStore <- gets $ unPrintableMVar . ssStateStore
  liftIO $ FFI.wrapper retCType argsCType (ffiCallbackBridge evalOnNewThread stateStore fun cwrapperDesc)
`-}
