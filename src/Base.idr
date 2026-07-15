module Base

import Derive.Prelude

import public Data.Primitives.Interpolation
import System
import System.Clock
import Data.SortedSet
import Data.SortedMap
import Stg.Syntax
import Stg.JSON
import Control.Monad.State
import Data.List
import Data.Array
import Data.Buffer

%hide Language.Reflection.TTImp.AltType

%language ElabReflection

Addr = Int

public export
data ArrIdxT
  = MutArrIdx Int
  | ArrIdx    Int
%runElab derive "ArrIdxT" [Show, Eq, Ord]

public export
data SmallArrIdxT
  = SmallMutArrIdx Int
  | SmallArrIdx    Int
%runElab derive "SmallArrIdxT" [Show, Eq, Ord]

public export
data ArrayArrIdxT
  = ArrayMutArrIdx Int
  | ArrayArrIdx    Int
%runElab derive "ArrayArrIdxT" [Show, Eq, Ord]

public export
record ByteArrayIdx where
  constructor MkByteArrayIdx
  baId        : Int
  baPinned    : Bool
  baAlignment : Int
%runElab derive "ByteArrayIdx" [Show, Eq, Ord]

public export
data PtrOrigin
  = CStringPtr    String           -- null terminated string
  | ByteArrayPtr  ByteArrayIdx     -- raw ptr to the byte array
  | RawPtr                          -- raw ptr to a values with unknown origin (i.e. FFI)
  | InfoTablePtr                    -- GHC Cmm STG machine's info table
  | CostCentreStackPtr              -- GHC Cmm STG machine's cost centre stack
  | StablePtr     Int              -- stable pointer must have AddrRep
  | LabelPtr      String LabelSpec  -- foreign symbol/label name + label sepcification (i.e. data or function)
%runElab derive "PtrOrigin" [Show, Eq, Ord]

public export
record IdT where
  constructor MkId
  binder : Binder

export Show IdT where show i = i.binder.binderUniqueName
export Eq   IdT where a == b = a.binder.binderUniqueName == b.binder.binderUniqueName -- TODO: use hash
export Ord  IdT where compare a b = compare a.binder.binderUniqueName b.binder.binderUniqueName -- TODO: use hash

public export
record DC where
  constructor MkDC
  datacon : DataCon

export Show DC where show i = i.datacon.dcUniqueName
export Eq   DC where a == b = a.datacon.dcUniqueName == b.datacon.dcUniqueName -- TODO: use hash
export Ord  DC where compare a b = compare a.datacon.dcUniqueName b.datacon.dcUniqueName -- TODO: use hash

export Show (Ptr Bits8) where show _ = "Ptr Bits8" -- TODO
export Eq (Ptr Bits8) where a == b = True -- TODO
export Ord (Ptr Bits8) where compare a b = EQ -- TODO


-- TODO: detect coercions during the evaluation
public export
data Atom     -- Q: should atom fit into a cpu register? A: yes
  = HeapPtr       Addr
  | Literal       Lit  -- TODO: remove this
  | Void
  | PtrAtom       PtrOrigin Int -- TODO (Ptr Bits8)
  | IntAtom       Int
  | WordAtom      Bits64
  | FloatAtom     Double -- TODO
  | DoubleAtom    Double
  | MVar          Int
  | MutVar        Int
  | TVar          Int
  | Array             ArrIdxT
  | MutableArray      ArrIdxT
  | SmallArray        SmallArrIdxT
  | SmallMutableArray SmallArrIdxT
  | ArrayArray        ArrayArrIdxT
  | MutableArrayArray ArrayArrIdxT
  | ByteArray         ByteArrayIdx
  | MutableByteArray  ByteArrayIdx
  | WeakPointer       Int
  | StableName        Int
  | ThreadId          Int
  | LiftedUndefined
  | Rubbish
  | Unbinded          IdT -- program point that created this value (used for debug purposes)
%runElab derive "Atom" [Show, Eq, Ord]

public export
data CutShow a = MkCutShow a
export Show (CutShow a) where show i = "CutShow"
export (Eq k) => Eq (CutShow k) where (MkCutShow a) == (MkCutShow b) = a == b
export (Ord k) => Ord (CutShow k) where compare (MkCutShow a) (MkCutShow b) = compare a b

public export
data StaticOrigin
  = SO_CloArg
  | SO_Let
  | SO_Scrut
  | SO_AltArg
  | SO_TopLevel
  | SO_Builtin
  | SO_ClosureResult
%runElab derive "StaticOrigin" [Show, Eq, Ord]

Env = SortedMap IdT (StaticOrigin, Atom)   -- NOTE: must contain only the defined local variables

export (Ord k, Ord v) => Ord (SortedMap k v) where compare a b = compare (kvList a) (kvList b)
export (Ord k) => Ord (SortedSet k) where compare a b = compare (Prelude.toList a) (Prelude.toList b)

StgRhsClosureT = Rhs  -- NOTE: must be StgRhsClosure only!

record TLogEntry where
  constructor MkTLogEntry
  tleObservedGlobalValue  : Atom
  tleCurrentLocalValue    : Atom
%runElab derive "TLogEntry" [Show, Eq, Ord]

TLog = SortedMap Int TLogEntry

public export
data ScheduleReason
  = SR_ThreadFinished
  | SR_ThreadFinishedMain
  | SR_ThreadFinishedFFICallback
  | SR_ThreadBlocked
  | SR_ThreadYield
%runElab derive "ScheduleReason" [Show, Eq, Ord]

public export
data StackContinuation
  -- basic block related
  = CaseOf  Env IdT (CutShow AltType) (CutShow $ List Alt)  -- pattern match on the result ; carries the closure's local environment
  -- closure related
  | Update  Addr                         -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   (List Atom)                       -- apply args on the result heap object
  -- exception related
  | Catch         Atom Bool Bool         -- catch frame ; exception handler, block async exceptions, interruptible
  | RestoreExMask  (Bool, Bool) Bool Bool -- saved: block async exceptions, interruptible -- old -> new-to-restore-to
  -- thread related
  | RunScheduler  ScheduleReason
  -- stm related
  | Atomically    Atom
  | CatchRetry    Atom Atom Bool TLog      -- first STM action, alternative STM action, is running alt code?
  | CatchSTM      Atom Atom             -- catch STM frame ; stm action, exception handler
  -- * special primop calling stack frames
  -- STM + async exception related
  | AtomicallyOp  Atom
  -- tag/enum related
  | DataToTagOp
  -- rts helper
  | RaiseOp       Atom
  -- object lifetime related
  | KeepAlive     Atom
  -- ext stg interpreter debug related
  -- | DebugFrame    DebugFrame             -- for debug purposes, it does not required for STG evaluation
--%runElab derive "StackContinuation" [Show]
%runElab derive "StackContinuation" [Show, Eq, Ord]

public export
data HeapObject : Type where
  Con : (hoIsLNE : Bool) -> (hoCon : DC) -> (hoConArgs : List Atom) -> HeapObject

  Closure :
    (hoIsLNE       : Bool) ->
    (hoName        : IdT) ->
    (hoCloBody     : CutShow StgRhsClosureT) ->
    (hoEnv         : Env) ->    -- local environment ; with live variables only, everything else is pruned
    (hoCloArgs     : List Atom) ->
    (hoCloMissing  : Nat) ->    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    HeapObject

  BlackHole : -- NOTE: each blackhole has exactly one corresponding thread and one update frame
    (hoBHOwnerThreadId : Int) ->        -- owner thread id
    (hoBHOriginalThunk : HeapObject) -> -- original heap object
    (hoBHWaitQueue     : List Int) ->   -- blocking queue of thread ids
    HeapObject

  ApStack : (hoResult : List Atom) -> (hoStack : List StackContinuation) -> HeapObject -- HINT: needed for the async exceptions
  RaiseException : Atom -> HeapObject
--  deriving (Show, Eq, Ord)
%runElab derive "HeapObject" [Show, Eq, Ord]


data AsyncExceptionMask
  = NotBlocked
  | Blocked     Bool -- isInterruptible
--  deriving (Eq, Ord, Show)
%runElab derive "AsyncExceptionMask" [Show, Eq, Ord]

-- NOTE: the BlockReason data type is some kind of reification of the blocked operation
public export
data BlockReason
  = BlockedOnMVar         Int (Maybe Atom) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole    Int       -- heap address
  | BlockedOnThrowAsyncEx Int       -- target thread id
  | BlockedOnSTM          TLog
  | BlockedOnForeignCall            -- RTS name: BlockedOnCCall
  | BlockedOnRead         Int       -- file descriptor
  | BlockedOnWrite        Int       -- file descriptor
  | BlockedOnDelay        (Clock UTC)   -- target time to wake up thread
--  deriving (Eq, Ord, Show)
%runElab derive "BlockReason" [Show, Eq, Ord]

public export
data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled
--  deriving (Eq, Ord, Show)
%runElab derive "ThreadStatus" [Show, Eq, Ord]

public export
record ThreadState where
  constructor MkThreadState
  tsCurrentResult     : List Atom -- Q: do we need this? A: yes, i.e. MVar read primops can write this after unblocking the thread
  tsStack             : List StackContinuation
  tsStatus            : ThreadStatus
  tsBlockedExceptions : List (Int, Atom) -- ids of the threads waiting to send an async exception + exception
  tsBlockExceptions   : Bool  -- block async exceptions
  tsInterruptible     : Bool  -- interruptible blocking of async exception
--  , tsAsyncExMask     :: !AsyncExceptionMask
  tsBound             : Bool
  tsLocked            : Bool  -- NOTE: can the thread be moved across capabilities? this is related to multicore haskell's load balancing
  tsCapability        : Int   -- NOTE: the thread is running on this capability ; Q: is this necessary?
  tsLabel             : (Maybe String)
  -- STM
  tsActiveTLog        : (Maybe TLog) -- elems: (global value, local value)
  tsTLogStack         : List TLog
%runElab derive "ThreadState" [Show, Eq, Ord]

--  deriving (Eq, Ord, Show)

public export
record WeakPtrDescriptor where
  constructor MkWeakPtrDescriptor
  wpdKey          : Atom
  wpdValue        : Maybe Atom -- live or dead
  wpdFinalizer    : Maybe Atom -- closure
  wpdCFinalizers  : List (Atom, Maybe Atom, Atom) -- fun, env ptr, data ptr
%runElab derive "WeakPtrDescriptor" [Show, Eq, Ord]
--  deriving (Show, Eq, Ord)

public export
record ByteArrayDescriptor where
  constructor MkByteArrayDescriptor
  baaMutableByteArray : Int -- raw ptr to the buffer
  baaByteArray        : (Maybe Int)  -- HINT: ByteArray can only be created via unsafeFreeze from a MutableByteArray
  baaPinned           : Bool
  baaAlignment        : Int
  baaSize             : Int

Show ByteArrayDescriptor where show _ = "ByteArrayDescriptor (TODO)"
Eq ByteArrayDescriptor where _ == _ = True -- TODO
Ord ByteArrayDescriptor where _ `compare` _ = EQ -- TODO

public export
record MVarDescriptor where
  constructor MkMVarDescriptor
  mvdValue    : Maybe Atom
  mvdQueue    : List Int -- thread id, blocking in this mvar ; this is required only for the fairness ; INVARIANT: BlockedOnReads are present at the beginning of the queue
%runElab derive "MVarDescriptor" [Show, Eq, Ord]
--  deriving (Show, Eq, Ord)

record TVarDescriptor where
  constructor MkTVarDescriptor
  tvdValue  : Atom
  tvdQueue  : SortedSet Int -- thread id, STM wake up queue
%runElab derive "TVarDescriptor" [Show, Eq, Ord]
--  deriving (Show, Eq, Ord)

Vector = Data.Array.Indexed.Array
Heap = SortedMap Int HeapObject
Stack = List StackContinuation


public export
record Rts where
  constructor MkRts
  {-
  -- data constructors needed for FFI argument boxing from the base library
  { rtsCharCon      :: DataCon
  , rtsIntCon       :: DataCon
  , rtsInt8Con      :: DataCon
  , rtsInt16Con     :: DataCon
  , rtsInt32Con     :: DataCon
  , rtsInt64Con     :: DataCon
  , rtsWordCon      :: DataCon
  , rtsWord8Con     :: DataCon
  , rtsWord16Con    :: DataCon
  , rtsWord32Con    :: DataCon
  , rtsWord64Con    :: DataCon
  , rtsPtrCon       :: DataCon
  , rtsFunPtrCon    :: DataCon
  , rtsFloatCon     :: DataCon
  , rtsDoubleCon    :: DataCon
  , rtsStablePtrCon :: DataCon
  , rtsTrueCon      :: DataCon
  , rtsFalseCon     :: DataCon

  -- closures used by FFI wrapper code ; heap address of the closure
  , rtsUnpackCString              :: Atom
  , rtsTopHandlerRunIO            :: Atom
  , rtsTopHandlerRunNonIO         :: Atom
  -}
  rtsTopHandlerFlushStdHandles  : Atom
  {-
  -- closures used by the exception primitives
  , rtsDivZeroException   :: Atom
  , rtsUnderflowException :: Atom
  , rtsOverflowException  :: Atom

  -- closures used by the STM primitives
  , rtsNestedAtomically   :: Atom -- (exception)

  -- closures used by the GC deadlock detection
  , rtsBlockedIndefinitelyOnMVar  :: Atom -- (exception)
  , rtsBlockedIndefinitelyOnSTM   :: Atom -- (exception)
  , rtsNonTermination             :: Atom -- (exception)

  -- rts helper custom closures
  , rtsApplyFun1Arg :: Atom
  , rtsTuple2Proj0  :: Atom
  -}
  -- builtin special store, see FFI (i.e. getOrSetGHCConcSignalSignalHandlerStore)
  rtsGlobalStore  : SortedMap StgName Atom
  {-
  -- program contants
  , rtsProgName     :: String
  , rtsProgArgs     :: [String]

  -- native C data symbols
  , rtsDataSymbol_enabled_capabilities  :: Ptr CInt
  }
  deriving (Show)
  -}
%runElab derive "Rts" [Show]

emptyRts : Rts
emptyRts = MkRts
  { rtsTopHandlerFlushStdHandles = Rubbish
  , rtsGlobalStore  = empty
  }

public export
record StgState where
  constructor MkStgState
  ssHeap                : Heap
  ssStaticGlobalEnv     : Env   -- NOTE: top level bindings only!
  ssDynamicHeapStart    : Int

  {-
  -- GC
  , ssLastGCTime          :: !UTCTime
  , ssLastGCAddr          :: !Int
  , ssGCInput             :: PrintableMVar ([Atom], StgState)
  , ssGCOutput            :: PrintableMVar RefSet
  , ssGCIsRunning         :: Bool
  , ssGCCounter           :: Int
  , ssRequestMajorGC      :: Bool
  , ssCAFSet              :: IntSet

  -- let-no-escape support
  , ssTotalLNECount       :: !Int
  -}
  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  ssCStringConstants    : SortedMap String Atom

  -- threading
  ssThreads             : SortedMap Int ThreadState

  -- thread scheduler related
  ssCurrentThreadId     : Int
  ssScheduledThreadIds  : List Int  -- HINT: one round
  ssThreadStepBudget    : Int

  -- primop related

  --ssStableNameMap       : SortedMap Atom Int -- TODO
  ssWeakPointers        : SortedMap Int WeakPtrDescriptor
  ssStablePointers      : SortedMap Int Atom
  ssMutableByteArrays   : SortedMap Int ByteArrayDescriptor
  ssMVars               : SortedMap Int MVarDescriptor
  ssTVars               : SortedMap Int TVarDescriptor
  ssMutVars             : SortedMap Int Atom
  ssArrays              : SortedMap Int (Vector Atom)
  ssMutableArrays       : SortedMap Int (Vector Atom)
  ssSmallArrays         : SortedMap Int (Vector Atom)
  ssSmallMutableArrays  : SortedMap Int (Vector Atom)
  ssArrayArrays         : SortedMap Int (Vector Atom)
  ssMutableArrayArrays  : SortedMap Int (Vector Atom)

  ssNextThreadId          : Int
  ssNextHeapAddr          : Int
  ssNextStableName        : Int
  ssNextWeakPointer       : Int
  ssNextStablePointer     : Int
  ssNextMutableByteArray  : Int
  ssNextMVar              : Int
  ssNextMutVar            : Int
  ssNextTVar              : Int
  ssNextArray             : Int
  ssNextMutableArray      : Int
  ssNextSmallArray        : Int
  ssNextSmallMutableArray : Int
  ssNextArrayArray        : Int
  ssNextMutableArrayArray : Int
{-
  -- FFI related
  , ssCBitsMap            :: DL
  , ssStateStore          :: PrintableMVar StgState

  -- FFI + createAdjustor
  , ssCWrapperHsTypeMap   :: !(Map Name (Bool, Name, [Name]))
-}
  -- RTS related
  ssRtsSupport            : Rts
{-
  -- debug
  , ssIsQuiet             :: Bool
  , ssLocalEnv            :: [Atom]
  , ssCurrentClosureEnv   :: Env
  , ssCurrentClosure      :: Maybe Id
  , ssCurrentClosureAddr  :: Int
  , ssExecutedClosures    :: !(Set Int)
  , ssExecutedClosureIds  :: !(Set Id)
  , ssExecutedPrimOps     :: !(Set Name)
  , ssExecutedFFI         :: !(Set ForeignCall)
  , ssExecutedPrimCalls   :: !(Set PrimCall)
  , ssClosureCallCounter  :: !Int
  , ssPrimOpTrace         :: !Bool

  -- call graph
  , ssCallGraph           :: !CallGraph
  , ssCurrentProgramPoint :: !ProgramPoint

  -- debugger API
  , ssDebuggerChan        :: DebuggerChan

  , ssEvaluatedClosures   :: !(Set Name)
  , ssBreakpoints         :: !(Map Breakpoint Int)
  , ssStepCounter         :: !Int
  , ssDebugFuel           :: !(Maybe Int)
  , ssDebugState          :: DebugState
  , ssStgErrorAction      :: Printable (M ())

  -- region tracker
  , ssMarkers             :: !(Map Name (Set Region))
  , ssRegionStack         :: !(Map (Int, Region) [(Int, AddressState, CallGraph)]) -- HINT: key = threadId + region ; value = index + start + call-graph
  , ssRegionInstances     :: !(Map Region (IntMap (AddressState, AddressState))) -- region => instance-index => start end
  , ssRegionCounter       :: !(Map Region Int)

  -- retainer db
  , ssReferenceMap        :: !(Map GCSymbol (Set GCSymbol))
  , ssRetainerMap         :: !(Map GCSymbol (Set GCSymbol))
  , ssGCRootSet           :: !(Set GCSymbol)

  -- tracing
  , ssTracingState        :: TracingState

  -- origin db
  , ssOrigin              :: !(IntMap (Id, Int, Int)) -- HINT: closure, closure address, thread id

  -- GC marker
  , ssGCMarkers           :: ![AddressState]

  -- tracing primops
  , ssTraceEvents         :: ![(String, AddressState)]
  , ssTraceMarkers        :: ![(String, Int, AddressState)]

  -- internal dev mode debug settings
  , ssDebugSettings       :: DebugSettings
-}
--  deriving (Show)
%runElab derive "StgState" [Show]

M = StateT StgState IO

export
stackPush : StackContinuation -> M ()
stackPush sc = modify $ \s => {ssThreads $= updateExisting {tsStack $= (sc ::)} s.ssCurrentThreadId} s

export
stackPop : M (Maybe StackContinuation)
stackPop = state $ \s =>
    ( {ssThreads $= updateExisting {tsStack $= drop 1} s.ssCurrentThreadId} s
    , lookup s.ssCurrentThreadId s.ssThreads >>= head' . tsStack
    )

export
freshHeapAddress : M Addr
freshHeapAddress = state $ \s => ({ssNextHeapAddr $= (+) 1} s, s.ssNextHeapAddr)

export
store : Addr -> HeapObject -> M ()
store a o = modify {ssHeap $= insert a o}

-- TODO: Q: how to implement pointer arithmetics and how to handle string constants and string literal pointers?

-- string constants
-- NOTE: the string gets extended with a null terminator

%foreign "scheme:string->foreign-buffer"
prim_allocString : String -> Int -> PrimIO Int

export
getCStringConstantPtrAtom : String -> M Atom
getCStringConstantPtrAtom key = do
  strMap <- gets ssCStringConstants
  case lookup key strMap of
    Just a  => pure a
    Nothing => do
      let bsCString = key ++ "\0"-- zero ending is added in scheme
          len = stringByteLength bsCString
      v <- primIO $ prim_allocString bsCString (cast len)
      let a = PtrAtom (CStringPtr bsCString) v
      --putStrLn $ "alloc top string: " ++ show v --a
      modify {ssCStringConstants $= insert key a}
      pure a

export
mylog : String -> M ()
mylog _ = pure ()

export
createThread : M (Int, ThreadState)
createThread = do
  let ts = MkThreadState
        { tsCurrentResult     = []
        , tsStack             = []
        , tsStatus            = ThreadRunning
        , tsBlockedExceptions = []
        , tsBlockExceptions   = False
        , tsInterruptible     = False
        , tsBound             = False
        , tsLocked            = False
        , tsCapability        = 0 -- TODO: implement capability handling
        , tsLabel             = Nothing
        , tsActiveTLog        = Nothing
        , tsTLogStack         = []
        }
  threadId <- gets ssNextThreadId
  modify {ssThreads $= insert threadId ts, ssNextThreadId := 1 + threadId}
  pure (threadId, ts)

export
scheduleToTheEnd : Int -> M ()
scheduleToTheEnd tid = do
  modify {ssScheduledThreadIds $= (++ [tid])}

export
switchToThread : Int -> M () -- TODO: check what code uses this
switchToThread tid = do
  modify {ssCurrentThreadId := tid}

export
stgErrorM : String -> M a
stgErrorM s = lift $ die s -- pure $ assert_total $ idris_crash s
{-
stgErrorM msg = do
  tid <- gets ssCurrentThreadId
  liftIO $ do
    putStrLn $ " * stgErrorM: " ++ show msg
    putStrLn $ "current thread id: " ++ show tid
  reportThread tid
  curClosure <- gets ssCurrentClosure
  liftIO $ do
    putStrLn $ "current closure: " ++ show curClosure
    putStrLn $ " * native estgi call stack:"
    putStrLn $ prettyCallStack callStack
  action <- unPrintable <$> gets ssStgErrorAction
  action
  error "stgErrorM"
-}

export
getThreadState : Int -> M ThreadState
getThreadState tid = do
  Just a <- lookup tid <$> gets ssThreads
    | Nothing => stgErrorM $ "unknown ThreadState: " ++ show tid
  pure a

export
updateThreadState : Int -> ThreadState -> M ()
updateThreadState tid ts = modify {ssThreads $= insert tid ts}

export
isThreadLive : ThreadStatus -> Bool
isThreadLive = \case
  ThreadFinished  => False
  ThreadDied      => False
  _               => True

export
emptyStgState : StgState
emptyStgState = MkStgState
  { ssHeap                = empty
  , ssStaticGlobalEnv     = empty
  , ssDynamicHeapStart    = 0
  , ssCStringConstants    = empty

  -- threading
  , ssThreads             = empty

  -- thread scheduler related
  , ssCurrentThreadId     = -1
  , ssScheduledThreadIds  = []
  , ssThreadStepBudget    = 0

  -- primop related

  --ssStableNameMap       : SortedMap Atom Int -- TODO
  , ssWeakPointers        = empty
  , ssStablePointers      = empty
  , ssMutableByteArrays   = empty
  , ssMVars               = empty
  , ssTVars               = empty
  , ssMutVars             = empty
  , ssArrays              = empty
  , ssMutableArrays       = empty
  , ssSmallArrays         = empty
  , ssSmallMutableArrays  = empty
  , ssArrayArrays         = empty
  , ssMutableArrayArrays  = empty

  , ssNextThreadId          = 0
  , ssNextHeapAddr          = 0
  , ssNextStableName        = 0
  , ssNextWeakPointer       = 0
  , ssNextStablePointer     = 0
  , ssNextMutableByteArray  = 0
  , ssNextMVar              = 0
  , ssNextMutVar            = 0
  , ssNextTVar              = 0
  , ssNextArray             = 0
  , ssNextMutableArray      = 0
  , ssNextSmallArray        = 0
  , ssNextSmallMutableArray = 0
  , ssNextArrayArray        = 0
  , ssNextMutableArrayArray = 0
  , ssRtsSupport            = emptyRts
  }

export
lookupEnvSO : Env -> Binder -> M (StaticOrigin, Atom)
lookupEnvSO localEnv b = do
  env <- if binderTopLevel b
          then gets ssStaticGlobalEnv
          else pure localEnv
  let Nothing = lookup (MkId b) env
        | Just a => pure a
  case b.binderUniqueName of
    -- HINT: GHC.Prim module does not exist it's a wired in module
    "ghc-prim_GHC.Prim.void#"           => pure (SO_Builtin, Void)
    "ghc-prim_GHC.Prim.realWorld#"      => pure (SO_Builtin, Void)
    "ghc-prim_GHC.Prim.coercionToken#"  => pure (SO_Builtin, Void)
    "ghc-prim_GHC.Prim.proxy#"          => pure (SO_Builtin, Void)
    "ghc-prim_GHC.Prim.(##)"            => pure (SO_Builtin, Void)
    _ => stgErrorM $ "unknown variable: " ++ show b

export
lookupEnv : Env -> Binder -> M Atom
lookupEnv localEnv b = snd <$> lookupEnvSO localEnv b

export
stg_error : String -> M a
stg_error = lift . die

export
readHeap : Atom -> M HeapObject
readHeap (HeapPtr l) = do
  h <- gets ssHeap
  case lookup l h of
    Nothing => stgErrorM $ "unknown heap address: " ++ show l
    Just o  => pure o
readHeap v = stg_error $ "readHeap: could not read heap object: " ++ show v

export
readHeapCon : Atom -> M HeapObject
readHeapCon a = readHeap a >>= \o => case o of
    Con{} => pure o
    _     => stgErrorM $ "expected con but got: "-- ++ show o

export
addManyBindersToEnv : StaticOrigin -> List Binder -> List Atom -> Env -> Env
addManyBindersToEnv so [] [] env = env
addManyBindersToEnv so (b :: binders) (v :: values) env = addManyBindersToEnv so binders values $ insert (MkId b) (so, v) env
addManyBindersToEnv so (b :: binders) values env = addManyBindersToEnv so binders values $ insert (MkId b) (so, Unbinded (MkId b)) env
addManyBindersToEnv so binders values env = assert_total $ idris_crash $ "addManyBindersToEnv - length mismatch: " ++ show (so, [(MkId b, binderType b, binderTypeSig b) | b <- binders], values)

export
addBinderToEnv : StaticOrigin -> Binder -> Atom -> Env -> Env
addBinderToEnv so b a = insert (MkId b) (so, a)

export
addZippedBindersToEnv : StaticOrigin -> List (Binder, Atom) -> Env -> Env
addZippedBindersToEnv so bvList env = foldl (\e, (b, v) => insert (MkId b) (so, v) e) env bvList

PrimOpEval = StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)

export
getCurrentThreadState : M ThreadState
getCurrentThreadState = do
  tid <- gets ssCurrentThreadId
  getThreadState tid

EvalOnNewThread = M (List Atom) -> M (List Atom)

export
lookupWeakPointerDescriptor : Int -> M WeakPtrDescriptor
lookupWeakPointerDescriptor wpId = do
  lookup wpId <$> gets ssWeakPointers >>= \case
    Nothing => stgErrorM $ "unknown WeakPointer: " ++ show wpId
    Just a  => pure a

export
lookupMVar : Int -> M MVarDescriptor
lookupMVar m = do
  lookup m <$> gets ssMVars >>= \case
    Nothing => stgErrorM $ "unknown MVar: " ++ show m
    Just a  => pure a

export
wakeupBlackHoleQueueThreads : Int -> M ()
wakeupBlackHoleQueueThreads addr = do
  BlackHole hoBHOwnerThreadId hoBHOriginalThunk hoBHWaitQueue <- readHeap (HeapPtr addr)
    | x => stg_error $ "internal error - expected BlackHole, got: " ++ show x
  -- wake up blocked threads
  for_ hoBHWaitQueue $ \waitingTid => do
    waitingTS <- getThreadState waitingTid
    case tsStatus waitingTS of
      ThreadBlocked (BlockedOnBlackHole dstAddr) => do
        updateThreadState waitingTid ({tsStatus := ThreadRunning} waitingTS)
      _ => stg_error $ "internal error - invalid thread status: " ++ show (tsStatus waitingTS)

export
lookupArray : Int -> M (Vector Atom)
lookupArray m = do
  lookup m <$> gets ssArrays >>= \case
    Nothing => stgErrorM $ "unknown Array: " ++ show m
    Just a  => pure a

export
lookupMutableArray : Int -> M (Vector Atom)
lookupMutableArray m = do
  lookup m <$> gets ssMutableArrays >>= \case
    Nothing => stgErrorM $ "unknown MutableArray: " ++ show m
    Just a  => pure a

export
allocAndStore : HeapObject -> M Addr
allocAndStore o = do
  a <- freshHeapAddress
  store a o
  pure a

export
lookupMutVar : Int -> M Atom
lookupMutVar m = do
  lookup m <$> gets ssMutVars >>= \case
    Nothing => stgErrorM $ "unknown MutVar: " ++ show m
    Just a  => pure a

export
lookupByteArrayDescriptor : Int -> M ByteArrayDescriptor
lookupByteArrayDescriptor m = do
  lookup m <$> gets ssMutableByteArrays >>= \case
    Nothing => stgErrorM $ "unknown ByteArrayDescriptor: " ++ show m
    Just a  => pure a

export
gettops : Module -> List TopBinding
gettops (MkModule
  modulePhase
  moduleUnitId
  moduleName
  moduleSourceFilePath
  moduleForeignStubs
  moduleHasForeignExported
  moduleDependency
  moduleExternalTopIds
  moduleTyCons
  moduleTopBindings
  ) = moduleTopBindings

