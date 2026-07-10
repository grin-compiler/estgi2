module PrimOp.Exceptions

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

import PrimOp.Concurrency

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  {-
    catch# :: (State# RealWorld -> (# State# RealWorld, a #) )
           -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
           -> State# RealWorld
           -> (# State# RealWorld, a #)
  -}
  ( "catch#", [f, h, w]) => do
    -- get async exception masking state
    ts <- getCurrentThreadState

    stackPush $ Catch h ts.tsBlockExceptions ts.tsInterruptible
    stackPush $ Apply [w]
    pure [f]

  -- getMaskingState# :: State# RealWorld -> (# State# RealWorld, Int# #)
  ( "getMaskingState#", [st]) => do
    ts <- getCurrentThreadState
    {-
       returns: 0 == unmasked,
                1 == masked, non-interruptible,
                2 == masked, interruptible
    -}
    let status = case (ts.tsBlockExceptions, ts.tsInterruptible) of
          (False, False)  => 0
          (True,  False)  => 1
          (True,  True)   => 2
          (False, True)   => assert_total $ idris_crash "impossible exception mask, tsBlockExceptions: False, tsInterruptible: True"
    pure [IntAtom status]

  -- maskAsyncExceptions# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)
  ( "maskAsyncExceptions#", [f, w]) => do

    -- get async exception masking state
    ts <- getCurrentThreadState
    tid <- gets ssCurrentThreadId

    -- set new masking state
    unless (ts.tsBlockExceptions == True && ts.tsInterruptible == True) $ do
      updateThreadState tid $ {tsBlockExceptions := True, tsInterruptible := True} ts
      --liftIO $ putStrLn $ "set mask - " ++ show tid ++ " maskAsyncExceptions# b:True i:True"
      stackPush $ RestoreExMask (True, True) ts.tsBlockExceptions ts.tsInterruptible

    -- run action
    stackPush $ Apply [w]
    pure [f]

  -- unmaskAsyncExceptions# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)
  ( "unmaskAsyncExceptions#", [f, w]) => do

    -- get async exception masking state
    ts <- getCurrentThreadState
    tid <- gets ssCurrentThreadId

    case tsBlockedExceptions ts of
      (thowingTid, exception) :: waitingTids
        => do
          -- try wake up thread
          throwingTS <- getThreadState thowingTid
          when (tsStatus throwingTS == ThreadBlocked (BlockedOnThrowAsyncEx tid)) $ do
            updateThreadState thowingTid $ {tsStatus := ThreadRunning} throwingTS
          -- raise exception
          ts <- getCurrentThreadState
          updateThreadState tid $ {tsBlockedExceptions := waitingTids} ts
          -- run action
          stackPush $ Apply [w] -- HINT: the stack may be captured by ApStack if there is an Update frame,
                                --        so we have to setup the continuation properly
          PrimOp.Concurrency.raiseAsyncEx [f] tid exception
          pure []
      [] => do
          -- set new masking state
          unless (tsBlockExceptions ts == False && tsInterruptible ts == False) $ do
            updateThreadState tid $ {tsBlockExceptions := False, tsInterruptible := False} ts
            --liftIO $ putStrLn $ "set mask - " ++ show tid ++ " unmaskAsyncExceptions# b:False i:False"
            stackPush $ RestoreExMask (False, False) (tsBlockExceptions ts) (tsInterruptible ts)
          pure ()
          -- run action
          stackPush $ Apply [w]
          pure [f]

  _ => fallback op args t tc
