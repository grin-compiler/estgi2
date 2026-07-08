module Interpreter

import Data.List
import System
import Control.Monad.State
import Data.SortedMap
import Data.SortedSet

import Stg.Syntax
import Stg.JSON

import Base

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

evalStackContinuation result = \case
  {-
  Apply args
    | [fun@HeapPtr{}] <- result
    -> do
      argsS <- peekAtoms args
      resultS <- peekAtoms result
      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show args ++ " to " ++ show result
      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show argsS ++ " to " ++ show resultS

      out <- builtinStgApply SO_ClosureResult fun args
      outS <- peekAtoms out

      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show args ++ " to " ++ show result ++ " output-result: " ++ show out
      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show argsS ++ " to " ++ show resultS ++ " output-result: " ++ show outS
      pure out

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

  -- HINT: STG IR uses 'case' expressions to chain instructions with strict evaluation
  CaseOf curClosureAddr curClosure localEnv (Id resultBinder) (CutShow altType) alts -> do
    modify' $ \s -> s {ssCurrentClosure = Just curClosure, ssCurrentClosureAddr = curClosureAddr}
    assertWHNF result altType resultBinder
    let resultId = (Id resultBinder)
    case altType of
      AlgAlt tc -> do
        let v = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v localEnv
        con <- readHeapCon v
        matchFirstCon resultId extendedEnv con $ getCutShowItem alts

      PrimAlt _r -> do
        let lit = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder lit localEnv
        matchFirstLit resultId extendedEnv lit $ getCutShowItem alts

      MultiValAlt n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [Alt{..}] = getCutShowItem alts
        when (n /= length altBinders) $ do
          stgErrorM $ "evalStackContinuation - MultiValAlt n broken assumption 2: " ++ show (n, altBinders, result)
        let extendedEnv = if n == 1 && altBinders == []
                            then addManyBindersToEnv SO_Scrut [resultBinder] result localEnv
                            else addManyBindersToEnv SO_Scrut altBinders result localEnv
        --unless (length altBinders == length result) $ do
        --  stgErrorM $ "evalStackContinuation - MultiValAlt - length mismatch: " ++ show (n, altBinders, result)

        setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId resultBinder) 0
        evalExpr extendedEnv altRHS

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
  x => assert_total $ idris_crash $ "unsupported continuation: " ++ show x ++ ", result: " ++ show result
