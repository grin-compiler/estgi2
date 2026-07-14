module PrimOp.Char

import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- eqChar# :: Char# -> Char# -> Int#
  ( "eqChar#", [Literal $ LitChar a, Literal $ LitChar b]) => pure [IntAtom $ if a == b then 1 else 0]

  -- ord# :: Char# -> Int#
  ( "ord#",    [Literal $ LitChar c]) => pure [IntAtom $ ord c]

  _ => fallback op args t tc
