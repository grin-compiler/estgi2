module Base

import System.Clock
import Data.Buffer
import Data.SortedSet
import Data.SortedMap
import Stg.Syntax
import Control.Monad.State
import Data.List

Addr = Int

data ArrIdxT
  = MutArrIdx Int
  | ArrIdx    Int
--  deriving (Show, Eq, Ord)

data SmallArrIdxT
  = SmallMutArrIdx Int
  | SmallArrIdx    Int
--  deriving (Show, Eq, Ord)

data ArrayArrIdxT
  = ArrayMutArrIdx Int
  | ArrayArrIdx    Int
--  deriving (Show, Eq, Ord)

record ByteArrayIdx where
  constructor MkByteArrayIdx
  baId        : Int
  baPinned    : Bool
  baAlignment : Int

--  deriving (Show, Eq, Ord)

data PtrOrigin
  = CStringPtr    String           -- null terminated string
  | ByteArrayPtr  ByteArrayIdx     -- raw ptr to the byte array
  | RawPtr                          -- raw ptr to a values with unknown origin (i.e. FFI)
  | InfoTablePtr                    -- GHC Cmm STG machine's info table
  | CostCentreStackPtr              -- GHC Cmm STG machine's cost centre stack
  | StablePtr     Int              -- stable pointer must have AddrRep
  | LabelPtr      String LabelSpec  -- foreign symbol/label name + label sepcification (i.e. data or function)
--  deriving (Show, Eq, Ord)

public export
IdT : Type
IdT = Int -- TODO

public export
DC : Type
DC = Int -- TODO

-- TODO: detect coercions during the evaluation
public export
data Atom     -- Q: should atom fit into a cpu register? A: yes
  = HeapPtr       Addr
  | Literal       Lit  -- TODO: remove this
  | Void
  | PtrAtom       PtrOrigin (Ptr Bits8)
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
--  deriving (Show, Eq, Ord)

CutShow : a -> a
CutShow = id

data StaticOrigin
  = SO_CloArg
  | SO_Let
  | SO_Scrut
  | SO_AltArg
  | SO_TopLevel
  | SO_Builtin
  | SO_ClosureResult
--  deriving (Show, Eq, Ord)

Env = SortedMap IdT (StaticOrigin, Atom)   -- NOTE: must contain only the defined local variables

StgRhsClosureT = Rhs  -- NOTE: must be StgRhsClosure only!

record TLogEntry where
  constructor MkTLogEntry
  tleObservedGlobalValue  : Atom
  tleCurrentLocalValue    : Atom
--  deriving (Show, Eq, Ord)

TLog = SortedMap Int TLogEntry

public export
data ScheduleReason
  = SR_ThreadFinished
  | SR_ThreadFinishedMain
  | SR_ThreadFinishedFFICallback
  | SR_ThreadBlocked
  | SR_ThreadYield
--  deriving (Show, Eq, Ord)

public export
data StackContinuation
  -- basic block related
  = CaseOf  Int IdT Env IdT (CutShow AltType) (CutShow $ List Alt)  -- closure addr & name (debug) ; pattern match on the result ; carries the closure's local environment
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
--  deriving (Show, Eq, Ord)

data HeapObject : Type where
  Con : (hoIsLNE : Bool) -> (hoCon : DC) -> (hoConArgs : List Atom) -> HeapObject

  Closure :
    (hoIsLNE       : Bool) ->
    (hoName        : IdT) ->
    (hoCloBody     : CutShow StgRhsClosureT) ->
    (hoEnv         : Env) ->    -- local environment ; with live variables only, everything else is pruned
    (hoCloArgs     : List Atom) ->
    (hoCloMissing  : Int) ->    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    HeapObject

  BlackHole : -- NOTE: each blackhole has exactly one corresponding thread and one update frame
    (hoBHOwnerThreadId : Int) ->        -- owner thread id
    (hoBHOriginalThunk : HeapObject) -> -- original heap object
    (hoBHWaitQueue     : List Int) ->   -- blocking queue of thread ids
    HeapObject

  ApStack : (hoResult : List Atom) -> (hoStack : List StackContinuation) -> HeapObject -- HINT: needed for the async exceptions
  RaiseException : Atom -> HeapObject
--  deriving (Show, Eq, Ord)


data AsyncExceptionMask
  = NotBlocked
  | Blocked     Bool -- isInterruptible
--  deriving (Eq, Ord, Show)

-- NOTE: the BlockReason data type is some kind of reification of the blocked operation
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

public export
data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled
--  deriving (Eq, Ord, Show)

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

--  deriving (Eq, Ord, Show)

record WeakPtrDescriptor where
  constructor MkWeakPtrDescriptor
  wpdKey          : Atom
  wpdValue        : Maybe Atom -- live or dead
  wpdFinalizer    : Maybe Atom -- closure
  wpdCFinalizers  : List (Atom, Maybe Atom, Atom) -- fun, env ptr, data ptr
--  deriving (Show, Eq, Ord)

record ByteArrayDescriptor where
  constructor MkByteArrayDescriptor
  baaMutableByteArray : Buffer
  baaByteArray        : (Maybe Buffer)  -- HINT: ByteArray can only be created via unsafeFreeze from a MutableByteArray
  baaPinned           : Bool
  baaAlignment        : Int

record MVarDescriptor where
  constructor MkMVarDescriptor
  mvdValue    : Maybe Atom
  mvdQueue    : List Int -- thread id, blocking in this mvar ; this is required only for the fairness ; INVARIANT: BlockedOnReads are present at the beginning of the queue
--  deriving (Show, Eq, Ord)

record TVarDescriptor where
  constructor MkTVarDescriptor
  tvdValue  : Atom
  tvdQueue  : SortedSet Int -- thread id, STM wake up queue
--  deriving (Show, Eq, Ord)

Vector = List
Heap = SortedMap Int HeapObject
Stack = List StackContinuation


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

  -- RTS related
  , ssRtsSupport          :: Rts

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
{-
-- TODO: Q: how to implement pointer arithmetics and how to handle string constants and string literal pointers?

-- string constants
-- NOTE: the string gets extended with a null terminator
getCStringConstantPtrAtom : String -> M Atom
getCStringConstantPtrAtom key = do
  strMap <- gets ssCStringConstants
  case lookup key strMap of
    Just a  -> pure a
    Nothing -> do
      let bsCString = BS8.snoc key '\0'
          (bsFPtr, bsOffset, _bsLen) = BS.toForeignPtr bsCString
          a = PtrAtom (CStringPtr bsCString) $ plusPtr (unsafeForeignPtrToPtr bsFPtr) bsOffset
      modify' $ \s -> s {ssCStringConstants = Map.insert key a strMap}
      pure a
-}

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

export
getThreadState : Int -> M ThreadState
getThreadState tid = do
  Just a <- lookup tid <$> gets ssThreads
    | Nothing => stgErrorM $ "unknown ThreadState: " ++ show tid
  pure a

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
  }

export
lookupEnv : Env -> Binder -> M Atom
--lookupEnv localEnv b = snd <$> lookupEnvSO localEnv b
