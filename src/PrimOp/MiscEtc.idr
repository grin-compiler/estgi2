module PrimOp.MiscEtc

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- traceMarker# :: Addr# -> State# s -> State# s
  ( "traceMarker#", [PtrAtom _ p, st]) => do
    pure []

  _ => fallback op args t tc
