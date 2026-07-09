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

  _ => fallback op args t tc
