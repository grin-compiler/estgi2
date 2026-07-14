module PrimOp.ObjectLifetime

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- keepAlive# :: v -> State# RealWorld -> (State# RealWorld -> p) -> p
  ( "keepAlive#", [managedObject, st, ioAction@(HeapPtr{})]) => do
    stackPush $ KeepAlive managedObject
    stackPush $ Apply [st]
    pure [ioAction]

  _ => fallback op args t tc
