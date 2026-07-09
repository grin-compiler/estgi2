module Interpreter

import Data.List
import System
import Control.Monad.State
import Data.SortedMap
import Data.SortedSet

import Stg.Syntax
import Stg.JSON

import Base

import PrimOp.Exceptions
import PrimOp.Concurrency
import PrimOp.WeakPointer

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

evalLiteral : Lit -> M Atom
evalLiteral = \case
  LitLabel name spec  => pure $ PtrAtom (LabelPtr name spec) 0 -- TODO: getFFILabelPtrAtom name spec
  {-
  LitString str       -> getCStringConstantPtrAtom str
  -}
  LitFloat n d  => pure . FloatAtom $ cast n / cast d
  LitDouble n d => pure . DoubleAtom $ cast n / cast d
  LitNullAddr   => pure $ PtrAtom RawPtr 0
  LitNumber LitNumInt n     => pure . IntAtom $ cast n
  LitNumber LitNumInt8 n    => pure . IntAtom $ cast n
  LitNumber LitNumInt16 n   => pure . IntAtom $ cast n
  LitNumber LitNumInt32 n   => pure . IntAtom $ cast n
  LitNumber LitNumInt64 n   => pure . IntAtom $ cast n
  LitNumber LitNumWord n    => pure . WordAtom $ cast n
  LitNumber LitNumWord8 n   => pure . WordAtom $ cast n
  LitNumber LitNumWord16 n  => pure . WordAtom $ cast n
  LitNumber LitNumWord32 n  => pure . WordAtom $ cast n
  LitNumber LitNumWord64 n  => pure . WordAtom $ cast n
  c@(LitChar{})             => pure $ Literal c
  LitRubbish{} => pure Rubbish
  l => assert_total $ idris_crash $ "unsupported: " ++ show l

evalArg : Env -> Arg -> M Atom
evalArg localEnv = \case
  StgLitArg l => evalLiteral l
  StgVarArg b => lookupEnv localEnv b

storeRhs : Bool -> Env -> Binder -> Addr -> Rhs -> M ()
storeRhs isLetNoEscape localEnv i addr = \case
  StgRhsCon dc l => do
    args <- traverse (evalArg localEnv) l
    store addr (Con isLetNoEscape (MkDC dc) args)

  cl@(StgRhsClosure freeVars _ paramNames _) => do
    let liveSet   = fromList $ map MkId freeVars
        prunedEnv = intersectionMap localEnv liveSet -- HINT: do pruning to keep only the live/later referred variables
    store addr (Closure isLetNoEscape (MkId i) (MkCutShow cl) prunedEnv [] (length paramNames))

export
declareTopBindings : List Module -> M ()
declareTopBindings mods = do
  let isStringLit = \case
        StgTopStringLit{} => True
        _                 => False
      (strings, closures) = partition isStringLit $ (concatMap gettops) mods
  -- bind string lits
  stringEnv <- for strings $ \case
    StgTopStringLit b str => do
      --TODO: strPtr <- getCStringConstantPtrAtom str
      let strPtr = PtrAtom (CStringPtr str) 0
      pure (MkId b, (SO_TopLevel, strPtr))
    _ => ?impossible1
  -- bind closures
  let getBindings : TopBinding -> List (Binder, Rhs)
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) => [(i, rhs)]
        StgTopLifted (StgRec l) => l
        _ => ?imossible2
      bindings = concatMap getBindings closures
  (closureEnv, rhsList) <- map unzip . for bindings $ \(b, rhs) => do
    addr <- freshHeapAddress
    pure ((MkId b, (SO_TopLevel, HeapPtr addr)), (b, addr, rhs))

  -- set the top level binder env
  modify {ssStaticGlobalEnv := fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  for_ rhsList $ \(b, addr, rhs) => storeRhs False empty b addr rhs


export
killAllThreads : M ()
{-
killAllThreads = do
  mylog "[estgi] - killAllThreads"
  -- TODO: check if there are running threads
  tsList <- gets $ IntMap.toList . ssThreads
  let runnableThreads = [tid | (tid, ts) <- tsList, tsStatus ts == ThreadRunning]
  isQuiet <- gets ssIsQuiet
  unless isQuiet $ when (runnableThreads /= []) $ do
    reportThreads
    error "killing all running threads"
  pure () -- TODO
-}

export
evalStackContinuation : List Atom -> StackContinuation -> M $ List Atom

export
evalStackMachine : List Atom -> M $ List Atom
evalStackMachine result = do
  Just stackCont <- stackPop
    | Nothing => pure result
  evalStackContinuation result stackCont >>= evalStackMachine

export
evalOnThread : Bool -> M (List Atom) -> M (List Atom)
evalOnThread isMainThread setupAction = do
  -- create main thread
  (tid, ts) <- createThread
  scheduleToTheEnd tid
  switchToThread tid
  stackPush $ RunScheduler $ if isMainThread then SR_ThreadFinishedMain else SR_ThreadFinished
  result0 <- setupAction
  --liftIO $ putStrLn $ "evalOnThread result0 = " ++ show result0
  --Debugger.reportState

  let loop : List Atom -> M (List Atom)
      loop resultIn = do
        resultOut <- evalStackMachine resultIn
        ts <- getThreadState tid
        case isThreadLive ts.tsStatus of
          True  => loop resultOut -- HINT: the new scheduling is ready
          False => do
            when isMainThread $ do
              mylog "[estgi] - main hs thread finished"
              killAllThreads
            pure ts.tsCurrentResult
  loop result0

export
evalOnMainThread : M (List Atom) -> M (List Atom)
evalOnMainThread = evalOnThread True

export
evalOnNewThread : M (List Atom) -> M (List Atom)
evalOnNewThread = evalOnThread False

assertWHNF : List Atom -> AltType -> Binder -> M ()
assertWHNF [hp@(HeapPtr{})] aty res = do
  o <- readHeap hp
  case o of
    Con _ dc args => pure ()
    (Closure hoIsLNE hoName hoCloBody hoEnv hoCloArgs hoCloMissing) => do
      when (hoCloMissing == 0 && aty /= MultiValAlt 1) $ lift $ do
            putStrLn "Thunk"
            putStrLn ""
            print aty
            putStrLn ""
            print res
            putStrLn ""
            die "Thunk"
    BlackHole{} => stg_error "BlackHole"
    RaiseException{} => pure ()
    _ => stg_error $ "assertWHNF: " ++ show o
assertWHNF _ _ _ = pure ()

evalExpr : Env -> Expr -> M (List Atom)

builtinStgEval : StaticOrigin -> Atom -> M (List Atom)
builtinStgEval so a@(HeapPtr l) = do
  o <- readHeap a
  case o of
    {-
    ApStack{..} -> do
      tid <- gets ssCurrentThreadId
      let HeapPtr l = a
      -- HINT: prevent duplicate computation
      store l BlackHole
        { hoBHOwnerThreadId = tid
        , hoBHOriginalThunk = o
        , hoBHWaitQueue     = []
        }
      stackPush (Update l)      -- HINT: ensure sharing, ApStack is always created from Update frame
      mapM_ stackPush (reverse hoStack)
      pure hoResult
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}       -> pure [a]
    BlackHole{..} -> do
      let HeapPtr addr = a
      tid <- gets ssCurrentThreadId
      ts <- getThreadState tid
      updateThreadState tid (ts {tsStatus = ThreadBlocked (BlockedOnBlackHole addr)})
      store addr o {hoBHWaitQueue = tid : hoBHWaitQueue}
      stackPush (Apply []) -- retry evaluation next time also
      stackPush $ RunScheduler SR_ThreadBlocked
      pure [a]
    -}
    (Closure hoIsLNE hoName hoCloBody hoEnv hoCloArgs hoCloMissing) => do
        0 <- pure hoCloMissing
              | _ => pure [a]

        let MkCutShow (StgRhsClosure _ uf params e) = hoCloBody
              | _ => stg_error "impossible stg-eval, expected StgRhsClosure"
            extendedEnv = addManyBindersToEnv SO_CloArg params hoCloArgs hoEnv

        unless (length params == length hoCloArgs) $ do
          stgErrorM $ "builtinStgEval - Closure - length mismatch: " ++ show (params, hoCloArgs)

        -- TODO: env or free var handling
        case uf of
          ReEntrant => do
            -- closure may be entered multiple times, but should not be updated or blackholed.
            evalExpr extendedEnv e

          Updatable => do
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            -- Q: what is eager and lazy blackholing?
            --  read: http://mainisusuallyafunction.blogspot.com/2011/10/thunks-and-lazy-blackholes-introduction.html
            --  read: https://www.microsoft.com/en-us/research/wp-content/uploads/2005/09/2005-haskell.pdf
            stackPush (Update l)
            tid <- gets ssCurrentThreadId
            store l (BlackHole tid o [])
            evalExpr extendedEnv e

          SingleEntry => do
            evalExpr extendedEnv e

    _ => stgErrorM $ "expected evaluable heap object, got: " ++ show a ++ " heap-object: " ++ show o ++ " static-origin: " ++ show so

builtinStgEval so a = stgErrorM $ "expected a thunk, got: " ++ show a ++ ", static-origin: " ++ show so

builtinStgApply : StaticOrigin -> Atom -> List Atom -> M (List Atom)
builtinStgApply so a [] = builtinStgEval so a
builtinStgApply so a@(HeapPtr addr) args = do
  let argCount      = length args
  o <- readHeap a
  case o of
    {-
    ApStack{..} -> do
      let HeapPtr l = a
      tid <- gets ssCurrentThreadId
      -- HINT: prevent duplicate computation
      store l BlackHole
        { hoBHOwnerThreadId = tid
        , hoBHOriginalThunk = o
        , hoBHWaitQueue     = []
        }
      stackPush (Apply args)
      stackPush (Update l)      -- HINT: ensure sharing, ApStack is always created from Update frame
      mapM_ stackPush (reverse hoStack)
      pure hoResult
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}             -> stgErrorM $ "unexpected con at apply: " ++ show o ++ ", args: " ++ show args ++ ", static-origin: " ++ show so
    BlackHole{..} -> do
      tid <- gets ssCurrentThreadId
      ts <- getThreadState tid
      updateThreadState tid (ts {tsStatus = ThreadBlocked (BlockedOnBlackHole addr)})
      store addr o {hoBHWaitQueue = tid : hoBHWaitQueue}
      stackPush (Apply args) -- retry evaluation next time also
      stackPush $ RunScheduler SR_ThreadBlocked
      pure [a]
    -}

    (Closure hoIsLNE hoName hoCloBody hoEnv hoCloArgs hoCloMissing) => case compare hoCloMissing argCount of
      -- under saturation
      -- hoCloMissing > argCount
      GT => do
        newAp <- freshHeapAddress
        store newAp (Closure hoIsLNE hoName hoCloBody hoEnv (hoCloArgs ++ args) (minus hoCloMissing argCount))
        pure [HeapPtr newAp]
      -- over saturation
      -- hoCloMissing < argCount

      LT => do
        let (satArgs, remArgs) = splitAt hoCloMissing args
        stackPush (Apply remArgs)
        builtinStgApply so a satArgs

      -- saturation
      -- hoCloMissing == argCount
      EQ => do
        newAp <- freshHeapAddress
        store newAp (Closure hoIsLNE hoName hoCloBody hoEnv (hoCloArgs ++ args) 0)
        builtinStgEval so (HeapPtr newAp)

    _ => stgErrorM $ "builtinStgApply - expected closure, got: " ++ show o ++ ", args: " ++ show args ++ ", static-origin: " ++ show so

builtinStgApply so a args = stgErrorM $ "builtinStgApply - expected a closure (ptr), got: " ++
  show a ++ ", args: " ++ show args ++ ", static-origin: " ++ show so


matchCon : DataCon -> AltCon -> Bool
matchCon a = \case
  AltDataCon dc => dcUNameHash a == dcUNameHash dc && dcUniqueName a == dcUniqueName dc
  AltLit{}      => False
  AltDefault    => True

matchFirstCon : IdT -> Env -> HeapObject -> List Alt -> M (List Atom)
matchFirstCon resultId localEnv (Con _ (MkDC dc) args) alts = do
  let altsWithDefault = case alts of
        d@(MkAlt AltDefault _ _) :: xs => xs ++ [d]
        xs => xs
  case [a | a@(MkAlt altCon _ _) <- altsWithDefault, matchCon dc altCon] of
    []  => stgErrorM $ "no matching alts for: " ++ show resultId
    (MkAlt altCon altBinders altRHS) :: _ => do
      let extendedEnv = case altCon of
                          AltDataCon{}  => addManyBindersToEnv SO_AltArg altBinders args localEnv
                          _             => localEnv
      evalExpr extendedEnv altRHS
matchFirstCon resultId localEnv ho alts = stg_error $ "matchFirstCon - impossible: " ++ show ho

convertAltLit : Lit -> Atom
convertAltLit lit = case lit of
  LitFloat n d              => FloatAtom $ cast n / cast d
  LitDouble n d             => DoubleAtom $ cast n / cast d
  LitNullAddr               => PtrAtom RawPtr 0
  LitNumber LitNumInt n     => IntAtom $ cast n
  LitNumber LitNumInt8 n    => IntAtom $ cast n
  LitNumber LitNumInt16 n   => IntAtom $ cast n
  LitNumber LitNumInt32 n   => IntAtom $ cast n
  LitNumber LitNumInt64 n   => IntAtom $ cast n
  LitNumber LitNumWord n    => WordAtom $ cast n
  LitNumber LitNumWord8 n   => WordAtom $ cast n
  LitNumber LitNumWord16 n  => WordAtom $ cast n
  LitNumber LitNumWord32 n  => WordAtom $ cast n
  LitNumber LitNumWord64 n  => WordAtom $ cast n
  LitLabel{}                => assert_total $ idris_crash $ "invalid alt pattern: " ++ show lit
  LitString{}               => assert_total $ idris_crash $ "invalid alt pattern: " ++ show lit
  c@(LitChar{})             => Literal c
  l => assert_total $ idris_crash $ "unsupported: " ++ show l

matchLit : Atom -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  => False
  AltLit l      => a == convertAltLit l
  AltDefault    => True

matchFirstLit : IdT -> Env -> Atom -> List Alt -> M (List Atom)
matchFirstLit resultId localEnv a [MkAlt AltDefault _ rhs] = evalExpr localEnv rhs
matchFirstLit resultId localEnv atom alts = do
  let altsWithDefault = case alts of
        d@(MkAlt AltDefault _ _) :: xs => xs ++ [d]
        xs => xs
  case [a | a@(MkAlt altCon _ _) <- altsWithDefault, matchLit atom altCon] of
    [] => stg_error $ "no lit match" ++ show (resultId, atom, [altCon | MkAlt altCon _ _ <- alts])
    (MkAlt _ _ altRHS) :: _ => evalExpr localEnv altRHS

evalStackContinuation result = \case
  Apply args => do
      let [fun@(HeapPtr{})] = result
            | x => stg_error $ "expected HeapPtr: " ++ show x ++ ", result: " ++ show result
      builtinStgApply SO_ClosureResult fun args
  {-

  Update dstAddr
    | [src@HeapPtr{}] <- result
    -> do
      wakeupBlackHoleQueueThreads dstAddr
      o <- readHeap src
      store dstAddr o
      dynamicHeapStartAddr <- gets ssDynamicHeapStart
      when (dstAddr < dynamicHeapStartAddr) $ do
        modify' $ \s@StgState{..} -> s {ssCAFSet = IntSet.insert dstAddr ssCAFSet}
      pure result
  -}
  -- HINT: STG IR uses 'case' expressions to chain instructions with strict evaluation
  CaseOf {-curClosureAddr curClosure-} localEnv resultId@(MkId resultBinder) (MkCutShow altType) (MkCutShow alts) => do
    assertWHNF result altType resultBinder
    case altType of
      AlgAlt tc => do
        let [v] = result | _ => stg_error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v localEnv
        con <- readHeapCon v
        matchFirstCon resultId extendedEnv con alts

      PrimAlt r => do
        let [lit] = result
              | _ => stg_error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder lit localEnv
        matchFirstLit resultId extendedEnv lit alts

      MultiValAlt n => do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [MkAlt altCon altBinders altRHS] = alts
              | _ => stg_error $ "expected single alt, got: " ++ show alts
        when (n /= length altBinders) $ do
          stgErrorM $ "evalStackContinuation - MultiValAlt n broken assumption 2: " ++ show (n, altBinders, result)
        let extendedEnv = if n == 1 && altBinders == []
                            then addManyBindersToEnv SO_Scrut [resultBinder] result localEnv
                            else addManyBindersToEnv SO_Scrut altBinders result localEnv
        --unless (length altBinders == length result) $ do
        --  stgErrorM $ "evalStackContinuation - MultiValAlt - length mismatch: " ++ show (n, altBinders, result)

        evalExpr extendedEnv altRHS

      _ => ?todo67
{-
      PolyAlt -> do
        let [Alt{..}]   = getCutShowItem alts
            [v]         = result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v $                 -- HINT: bind the result
                          localEnv
                          --addManyBindersToEnv SO_AltArg altBinders result localEnv  -- HINT: bind alt params
        { -
        unless (length altBinders == length result) $ do
          stgErrorM $ "evalStackContinuation - PolyAlt - length mismatch: " ++ show (altBinders, result)
        - }
        setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId resultBinder) 0
        evalExpr extendedEnv altRHS

  s@(RestoreExMask oldMask blockAsyncEx isInterruptible) -> do
    tid <- gets ssCurrentThreadId
    ts <- getCurrentThreadState
    updateThreadState tid $ ts {tsBlockExceptions = blockAsyncEx, tsInterruptible = isInterruptible}
    case tsBlockedExceptions ts of
      (thowingTid, exception) : waitingTids
        | blockAsyncEx == False
        -> do
          -- try wake up thread
          throwingTS <- getThreadState thowingTid
          when (tsStatus throwingTS == ThreadBlocked (BlockedOnThrowAsyncEx tid)) $ do
            updateThreadState thowingTid throwingTS {tsStatus = ThreadRunning}
          -- raise exception
          ts <- getCurrentThreadState
          updateThreadState tid ts {tsBlockedExceptions = waitingTids}
          PrimConcurrency.raiseAsyncEx result tid exception
      _ -> pure ()
    pure result

  Catch h b i -> do
    -- TODO: is anything to do??
    -- assert if current mask is the same as the one in stack frame
    ts@ThreadState{..} <- getCurrentThreadState
    when (tsBlockExceptions /= b || tsInterruptible /= i) $ do
      error $ "Catch frame assertion failure - ex mask mismatch, expected: " ++ show (b, i) ++ " got: " ++ show (tsBlockExceptions, tsInterruptible)
    pure result
  -}
  {-
  Atomically stmAction -> PrimSTM.commitOrRestart stmAction result

  CatchSTM{} -> PrimSTM.mergeNestedOrRestart result -- Q: check how this is implemented in the native RTS

  CatchRetry{} -> PrimSTM.mergeNestedOrRestart result

  RunScheduler sr -> do
    --liftIO $ print (RunScheduler sr)
    Scheduler.runScheduler result sr

  -- HINT: dataToTag# has an eval call in the middle, that's why we need this continuation, it is the post-returning part of the op implementation
  DataToTagOp -> PrimTagToEnum.dataToTagOp result

  AtomicallyOp stmAction -> do
    promptM $ putStrLn "[ AtomicallyOp ]"
    PrimSTM.atomicallyOp stmAction

  RaiseOp ex -> do
    ctid <- gets ssCurrentThreadId
    mylog $ "ctid: " ++ show ctid ++ " " ++ show (RaiseOp ex)
    PrimExceptions.raiseEx ex

  KeepAlive{} -> do
    pure result

  DebugFrame df -> evalDebugFrame result df
  -}
  x => stg_error $ "unsupported continuation: " ++ show x ++ ", result: " ++ show result

declareBinding : Bool -> Env -> Binding -> M Env
declareBinding isLetNoEscape localEnv = \case
  StgNonRec b rhs => do
    addr <- freshHeapAddress
    storeRhs isLetNoEscape localEnv b addr rhs
    pure $ addBinderToEnv SO_Let b (HeapPtr addr) localEnv

  StgRec l => do
    (ls, newEnvItems) <- map unzip . for l $ \(b, _) => do
      addr <- freshHeapAddress
      pure (addr, (b, (HeapPtr addr)))
    let extendedEnv = addZippedBindersToEnv SO_Let newEnvItems localEnv
    for_ (zip ls l) $ \(addr, (b, rhs)) => do
      storeRhs isLetNoEscape extendedEnv b addr rhs
    pure extendedEnv

evalPrimOp : StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)

evalExpr localEnv = \case
  StgTick _ e       => evalExpr localEnv e
  StgLit l          => pure <$> evalLiteral l
  {-
  StgConApp dc l _
    -- HINT: make and return unboxed tuple
    | UnboxedTupleCon{} <- dcRep dc
    -> mapM (evalArg localEnv) l   -- Q: is this only for unboxed tuple? could datacon be heap allocated?

    -- HINT: create boxed datacon on the heap
    | otherwise
    -> do
      args <- mapM (evalArg localEnv) l
      loc <- allocAndStore (Con False (DC dc) args)
      pure [HeapPtr loc]
  -}
  StgLet b e => do
    extendedEnv <- declareBinding False localEnv b
    evalExpr extendedEnv e

  StgLetNoEscape b e => do -- TODO: do not allocate closure on heap, instead put into env (stack) allocated closure ; model stack allocated heap objects
    extendedEnv <- declareBinding True localEnv b
    evalExpr extendedEnv e

  -- var (join id)
  StgApp i [] => case binderDetails i of
    JoinId 0 _ => do
      -- HINT: join id-s are always closures, needs eval
      -- NOTE: join id's type tells the closure return value representation
      (so, v) <- lookupEnvSO localEnv i
      builtinStgEval so v
    JoinId x _ => stgErrorM $ "join-id var arity error, expected 0, got: " ++ show x ++ " id: " ++ show i

    -- var (non join id)
    _ => case binderType i of
      SingleValue LiftedRep => do
        -- HINT: must be HeapPtr ; read heap ; check if Con or Closure ; eval if Closure ; return HeapPtr if Con
        (so, v) <- lookupEnvSO localEnv i
        builtinStgEval so v

      SingleValue _ => do
        v <- lookupEnv localEnv i
        pure [v]

      UnboxedTuple [] => do
        case binderUniqueName i of
          "ghc-prim_GHC.Prim.coercionToken#" => pure [] -- wired in coercion token ; FIXME: handle wired-in names with a better design
          r => stgErrorM $ "unsupported var rep: " ++ show r ++ " " ++ show i -- unboxed: is it possible??

      r => stgErrorM $ "unsupported var rep: " ++ show r ++ " " ++ show i -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  --  A: the join id type is for the return value representation and not for the id representation, so it can be unlifted.
  StgApp i l => case binderDetails i of
    JoinId _ _ => do
      args <- traverse (evalArg localEnv) l
      (so, v) <- lookupEnvSO localEnv i
      builtinStgApply so v args

    {- non-join id -}
    _ => case binderType i of
      SingleValue LiftedRep => do
        args <- traverse (evalArg localEnv) l
        (so, v) <- lookupEnvSO localEnv i
        builtinStgApply so v args
      r => stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid


  StgCase e scrutineeResult altType alts => do
    stackPush (CaseOf localEnv (MkId scrutineeResult) (MkCutShow altType) $ MkCutShow alts)
    evalExpr localEnv e

  StgOpApp (StgPrimOp op) l t tc => do
    args <- traverse (evalArg localEnv) l
    evalPrimOp op args t tc
{-
  StgOpApp (StgFCallOp foreignCall) l t tc -> do
    -- check foreign target region and breakpoint
    case foreignCTarget foreignCall of
      StaticTarget _ targetName _ _ -> do
        Debugger.checkBreakpoint (envToAtoms localEnv) $ BkpFFISymbol targetName
        Debugger.checkRegion targetName
      _ -> pure ()

    markFFI foreignCall
    args <- case foreignCTarget foreignCall of
      StaticTarget _ "createAdjustor" _ _
        -- void* createAdjustor (int cconv, StgStablePtr hptr, StgFunPtr wptr, char *typeString);
        | [arg0_cconv, arg1_hptr, StgLitArg arg2_wptr, arg3_typeString, arg4_void] <- l
        -> do
            -- HINT:
            --  do not resolve the wrapper function label
            --  the label name is used for FFI type signature lookup
            [arg0_cconvAtom, arg1_hptrAtom, arg3_typeStringAtom, arg4_voidAtom] <- mapM (evalArg localEnv) [arg0_cconv, arg1_hptr, arg3_typeString, arg4_void]
            pure [arg0_cconvAtom, arg1_hptrAtom, Literal arg2_wptr, arg3_typeStringAtom, arg4_voidAtom]
      StaticTarget _ "createAdjustor" _ _
        -> do
            liftIO $ do
              putStrLn "illegal createAdjustor call:"
              putStrLn $ "  foreignCall: " ++ show foreignCall
              putStrLn $ "  type:        " ++ show t
              putStrLn   "  args:"
              forM_ l $ \a -> do
                putStrLn $ "    " ++ show a
            stgErrorM $ "illegal createAdjustor call"
      _ -> mapM (evalArg localEnv) l
    --mylog $ show ("executing", foreignCall, args)
    result <- evalFCallOp evalOnNewThread foreignCall args t tc
    --mylog $ show (foreignCall, args, result)
    pure result

  StgOpApp (StgPrimCallOp primCall) l t tc -> do
    markPrimCall primCall
    args <- mapM (evalArg localEnv) l
    result <- evalPrimCallOp primCall args t tc
    --liftIO $ print (primCall, args, result)
    pure result

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t
  -}
  x => stg_error $ "unsupported expr: " ++ show x

evalPrimOp =
  {-
  PrimAddr.evalPrimOp $
  PrimArray.evalPrimOp $
  PrimSmallArray.evalPrimOp $
  PrimArrayArray.evalPrimOp $
  PrimByteArray.evalPrimOp $
  PrimChar.evalPrimOp $
  -}
  PrimOp.Concurrency.evalPrimOp $
  {-
  PrimDelayWait.evalPrimOp $
  PrimParallelism.evalPrimOp $
  -}
  PrimOp.Exceptions.evalPrimOp $
  {-
  PrimFloat.evalPrimOp $
  PrimDouble.evalPrimOp $
  PrimInt64.evalPrimOp $
  PrimInt32.evalPrimOp $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
  PrimMutVar.evalPrimOp $
  PrimMVar.evalPrimOp $
  PrimNarrowings.evalPrimOp $
  PrimPrefetch.evalPrimOp $
  PrimStablePointer.evalPrimOp $
  PrimSTM.evalPrimOp $
  -}
  PrimOp.WeakPointer.evalPrimOp $
  {-
  PrimWord64.evalPrimOp $
  PrimWord32.evalPrimOp $
  PrimWord16.evalPrimOp $
  PrimWord8.evalPrimOp $
  PrimWord.evalPrimOp $
  PrimTagToEnum.evalPrimOp $
  PrimUnsafe.evalPrimOp $
  PrimMiscEtc.evalPrimOp $
  PrimObjectLifetime.evalPrimOp $
  PrimInfoTableOrigin.evalPrimOp $
  -}
  unsupported where
    unsupported : StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
    unsupported op args t tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
