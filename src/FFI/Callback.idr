module FFI.Callback

import Control.Monad.State
import Data.SortedMap
import Data.List
import Data.String
import Data.IORef
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

ffiRepToCType : FFIRep -> String
ffiRepToCType (MkFFIRep r) = case r of
  VoidRep     => "void"
  LiftedRep   => "void*"
  UnliftedRep => "void*"
  Int8Rep     => "integer-8"
  Int16Rep    => "integer-16"
  Int32Rep    => "integer-32"
  Int64Rep    => "integer-64"
  IntRep      => "integer-64"
  Word8Rep    => "unsigned-8"
  Word16Rep   => "unsigned-16"
  Word32Rep   => "unsigned-32"
  Word64Rep   => "unsigned-64"
  WordRep     => "unsigned-64"
  AddrRep     => "void*"
  FloatRep    => "single-float"
  DoubleRep   => "double-float"
  rep         => assert_total $ idris_crash $ "ffiRepToCType - unsupported: " ++ show rep

public export
data ArgList : Type where [external]

export
%foreign "scheme:list"
emptyArgs : ArgList

export
%foreign "scheme:list-cons"
addArg : {a : Type} -> a -> ArgList -> ArgList

export
%foreign "scheme:list-head"
headArg : {a : Type} -> ArgList -> a

export
%foreign "scheme:cdr"
tailArgs : ArgList -> ArgList

ffiRepToGetter : FFIRep -> ArgList -> Atom
ffiRepToGetter (MkFFIRep r) args = case r of
  VoidRep     => Void
  Int64Rep    => IntAtom $ headArg args
  Int32Rep    => IntAtom $ headArg args
  Int16Rep    => IntAtom $ headArg args
  Int8Rep     => IntAtom $ headArg args
  IntRep      => IntAtom $ headArg args
  Word64Rep   => WordAtom $ headArg args
  Word32Rep   => WordAtom $ headArg args
  Word16Rep   => WordAtom $ headArg args
  Word8Rep    => WordAtom $ headArg args
  WordRep     => WordAtom $ headArg args
  AddrRep     => PtrAtom RawPtr $ headArg args
  FloatRep    => FloatAtom  $ headArg args
  DoubleRep   => DoubleAtom $ headArg args
  rep         => assert_total $ idris_crash $ "ffiRepToGetter - unsupported: " ++ show rep

ffiRepToSetter : FFIRep -> ArgList -> Atom -> StgName -> ArgList
ffiRepToSetter (MkFFIRep r) args a retTypeName = case (r, a) of
  (VoidRep,   Void)          => addArg () args
  (FloatRep,  FloatAtom v)   => addArg v args
  (DoubleRep, DoubleAtom v)  => addArg v args
  (Int64Rep,  IntAtom v)     => addArg v args
  (Int32Rep,  IntAtom v)     => addArg v args
  (Int16Rep,  IntAtom v)     => addArg v args
  (Int8Rep,   IntAtom v)     => addArg v args
  (IntRep,    IntAtom v)     => addArg v args
  (Word64Rep, WordAtom v)    => addArg v args
  (Word32Rep, WordAtom v)    => addArg v args
  (Word16Rep, WordAtom v)    => addArg v args
  (Word8Rep,  WordAtom v)    => addArg v args
  (WordRep,   WordAtom v)    => addArg v args
  (AddrRep,   PtrAtom RawPtr v) => addArg v args
  x => assert_total $ idris_crash $ "ffiRepToSetter - unsupported: " ++ show (x, retTypeName)

%foreign "scheme:make-cb-fun"
prim_make_cb_fun : (ArgList -> ArgList) -> PrimIO Int
-- head / tail = car / cdr

ffiCallbackBridge : EvalOnNewThread -> IORef (Maybe StgState) -> Atom -> CWrapperDesc -> List Atom -> IO ArgList

--IDEA: adjustor is a dep typed function where the arity varies according to the type signature
export
createAdjustor : EvalOnNewThread -> Atom -> (Bool, StgName, List StgName) -> M (Int, IO ())
createAdjustor evalOnNewThread fun cwrapperDesc@(_, retTy, argTys) = do
  putStrLn $ "created adjustor: " ++ show fun ++ " " ++ show cwrapperDesc
  let retCType  = ffiRepToCType $ ffiTypeToFFIRep retTy
      argsFFIType = map ffiTypeToFFIRep argTys
      argsCType = map (ffiRepToCType . ffiTypeToFFIRep) argTys
  putStrLn "1idr"
  let str = """
        (lambda (f)
          (begin
          (display "1a\n")
          (lock-object f)
          (display "2a\n")
          (let ([x (foreign-callable
                   (lambda args
                      (display "callback-fun") (newline) (display "args: ") (pretty-print args)
                      (car (f args)))
                   (\{unwords argsCType})
                   \{retCType})])
          (display "3a\n")
          (lock-object x)
          (display "4a\n")
          (let ([r (foreign-callable-entry-point x)])
            (display "5a\n")
            (display r) (newline)
            (display x) (newline)
            (display f) (newline)
            r)
          )))
        """
      --does not work x : M ((AnyPtr -> ()) -> PrimIO Int)
      x : M ((ArgList -> ArgList) -> Int)
      x = primIO $ prim_eval str
  putStrLn "2idr"

  MkPrintable (mutex, stateStore) <- gets ssStateStore

  let cb : ArgList -> IO ArgList
      cb i = do
        putStrLn $ "hello cb1 "
        --putStrLn $ "hello cb2 " ++ show i
        putStrLn $ "called adjustor: " ++ show fun ++ " " ++ show cwrapperDesc
        --ffiRepToGetter : FFIRep -> ArgList -> Atom
        let args : List Atom
            args = reverse $ fst $ foldl (\(atoms, l), t => (ffiRepToGetter t l :: atoms, tailArgs l)) ([],i) argsFFIType
        putStrLn $ "hello cb1 args: " ++ show args

        primIO $ prim_mutexAcquire mutex
        putStrLn $ "begin hello cb1 args: " ++ show args
        result <- ffiCallbackBridge evalOnNewThread stateStore fun cwrapperDesc args
        putStrLn $ "finished hello cb1 args: " ++ show args ++ " fun " ++ show fun
        primIO $ prim_mutexRelease mutex
        pure result

  putStrLn "3idr"
  {-
  putStrLn $ "create cb"
  x0 <- x
  i <- primIO $ x0 cb
  -}
  --i <- primIO $ my_prim_make_cb_fun (\n => unsafePerformIO $ cb n)
  xx <- x
  putStrLn "4idr"
  let i = xx (\n => unsafePerformIO $ cb n)
  putStrLn "5idr"
  --i <- primIO xxx
  putStrLn "6idr"
  putStrLn $ "cb addr: " ++ show i
  let (True, "Unit", _) = cwrapperDesc
        | _ => ?todo_adjustor
  pure (i, pure ())
  {-
  let (retCType :: argsCType) = map (ffiRepToCType . ffiTypeToFFIRep) $ retTy :: argTys
  stateStore <- gets $ unPrintableMVar . ssStateStore
  liftIO $ FFI.wrapper retCType argsCType (ffiCallbackBridge evalOnNewThread stateStore fun cwrapperDesc)
-}

ffiCallbackBridge evalOnNewThread stateStore fun (isIOCall, retTypeName, argTypeNames) argAtoms = do
  -- read args from ffi
  --argAtoms <- zipWithM (ffiRepToGetter . ffiTypeToFFIRep) argTypeNames argsStorage
  --let argAtoms : List Atom
  --    argAtoms = []

  Just before <- readIORef stateStore
    | _ => ?errorffiCallbackBridge1

  (after, unboxedResult) <- runStateT before $ do
    funStr <- readHeap fun

    oldThread <- gets ssCurrentThreadId

    boxedResult <- evalOnNewThread $ do
      -- TODO: box FFI arg atoms
      --  i.e. rts_mkWord8
      -- TODO: check how the stubs are generated and what types are need to be boxed
      --liftIO $ putStrLn $ "[step 2]"
      boxedArgs <- sequence $ zipWith boxFFIAtom argTypeNames argAtoms
      --liftIO $ putStrLn $ "[step 3] boxedArgs: " ++ show boxedArgs
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      -- Q: what stack shall we use here?
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      stackPush $ RunScheduler SR_ThreadFinishedFFICallback -- return from callback
      stackPush $ Apply [] -- force result to WHNF ; is this needed?
      --liftIO $ putStrLn $ "[step 4]"
      stackPush $ Apply $ boxedArgs ++ if isIOCall then [Void] else []
      --liftIO $ putStrLn $ "[step 5]"
      --modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
      pure [fun]

    --liftIO $ putStrLn $ "[pre - callback END]   " ++ show fun ++ " boxed-result: " ++ show boxedResult
    --requestContextSwitch
    switchToThread oldThread
    sequence $ zipWith unboxFFIAtom [retTypeName] boxedResult
{-
--=============================================================================
    -- force result to WHNF
    resultLazy <- evalOnNewThread fun $ boxedArgs ++ [Void]
    finalResult <- case resultLazy of
      []            -> pure resultLazy
      [valueThunk]  -> evalOnNewThread valueThunk []
    switchToThread oldThread
--=============================================================================
    pure finalResult
-}
  --putStrLn $ "[pre - callback END]   " ++ show fun ++ " result: " ++ show unboxedResult
  writeIORef stateStore (Just after)
  --putStrLn $ "[callback END]   " ++ show fun ++ " result: " ++ show unboxedResult

  -- HINT: need some kind of channel between the IO world and the interpreters StateT IO
  -- NOTE: stg apply fun argAtoms
  case unboxedResult of
    []        => pure $ addArg () emptyArgs
    [retAtom] => pure $ ffiRepToSetter (ffiTypeToFFIRep retTypeName) emptyArgs retAtom retTypeName
      -- write result to ffi
      -- NOTE: only single result is supported
      --ffiRepToSetter (ffiTypeToFFIRep retTypeName) retStorage retAtom retTypeName
    _ => ?ffi_cb_todo
