module PrimOp.TagToEnum

import Data.List
import Data.Fin
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

export
dataToTagOp : List Atom -> M (List Atom)
dataToTagOp [whnf@(HeapPtr{})] = assert_total $ do -- TODO: check that dataCon.datacon.dcTyCon.tycon.tcDataCons is not empty
  -- NOTE: the GHC dataToTag# primop works for any Data Con regardless its arity
  (Con _ dataCon _) <- readHeapCon whnf

  case findIndex (\d => d.dcId == dataCon.datacon.dcId) dataCon.datacon.dcTyCon.tycon.tcDataCons of
    Just i  => pure [IntAtom $ cast $ finToInteger i]
    _       => stgErrorM $ "Data constructor tag is not found for " ++ show (dcUniqueName dataCon.datacon)
dataToTagOp result = stgErrorM $ "dataToTagOp expected [HeapPtr], got: " ++ show result

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  ( "dataToTagLarge#", [ho@(HeapPtr{})]) => do
    -- HINT: do the work after getting the WHNF result back
    stackPush DataToTagOp
     -- HINT: force thunks
    stackPush $ Apply []
    pure [ho]

  -- tagToEnum# :: Int# -> a
  ( "tagToEnum#", [IntAtom i]) => do
    let Just tyc = tc
          | _ => ?error_tag_to_enum
    let Just dc = head' $ drop (cast $ i - 1) tyc.tcDataCons
          | _ => ?error_tag_to_enum2
    loc <- allocAndStore (Con False (MkDC dc) [])
    pure [HeapPtr loc]

  _ => fallback op args t tc
