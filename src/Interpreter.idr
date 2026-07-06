module Interpreter

import Data.List
import System
import Control.Monad.State

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
{-
declareTopBindings : List Module -> M ()
declareTopBindings mods = do
  let isStringLit = \case
        StgTopStringLit{} => True
        _                 => False
      (strings, closures) = partition isStringLit $ (concatMap gettopss) mods

  -- bind string lits
  stringEnv <- forM strings $ \(StgTopStringLit b str) -> do
    strPtr <- getCStringConstantPtrAtom str
    pure (Id b, (SO_TopLevel, strPtr))

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l

  (closureEnv, rhsList) <- fmap unzip . forM bindings $ \(b, rhs) -> do
    addr <- freshHeapAddress
    pure ((Id b, (SO_TopLevel, HeapPtr addr)), (b, addr, rhs))

  -- set the top level binder env
  modify' $ \s@StgState{..} -> s {ssStaticGlobalEnv = Map.fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  forM_ rhsList $ \(b, addr, rhs) -> storeRhs False mempty b addr rhs
-}

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
