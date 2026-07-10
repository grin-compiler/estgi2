module PrimOp.Concurrency

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- myThreadId# :: State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "myThreadId#", [state_token]) => do
    tid <- gets ssCurrentThreadId
    pure [ThreadId tid]

  -- noDuplicate# :: State# s -> State# s
  ( "noDuplicate#", [st]) => do
    -- NOTE: the stg interpreter is not parallel, so this is a no-op
    pure []

  _ => fallback op args t tc
