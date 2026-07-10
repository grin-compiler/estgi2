module PrimOp.Exceptions

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

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

  _ => fallback op args t tc
