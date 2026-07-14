module PrimOp.WeakPointer

import Data.SortedMap
import Control.Monad.State

import Stg.Syntax
import Stg.JSON
import Base

newWeakPointer : Atom -> Atom -> Maybe Atom -> M Int
newWeakPointer key value finalizer = do
  next <- gets ssNextWeakPointer
  let desc = MkWeakPtrDescriptor
        { wpdKey          = key
        , wpdValue        = Just value
        , wpdFinalizer    = finalizer
        , wpdCFinalizers  = []
        }
  modify {ssWeakPointers $= insert next desc, ssNextWeakPointer := 1 + next}
  pure next

export
evalPrimOp : PrimOpEval -> StgName -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalPrimOp fallback op args t tc = case (op, args) of

  -- mkWeak# :: o -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeak#", [key, value, finalizer, st]) => do
    wpId <- newWeakPointer key value (Just finalizer)
    pure [WeakPointer wpId]

  -- mkWeakNoFinalizer# :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeakNoFinalizer#", [key, value, w]) => do
    wpId <- newWeakPointer key value Nothing
    pure [WeakPointer wpId]

  -- touch# :: o -> State# RealWorld -> State# RealWorld
  ( "touch#", [o, st]) => do
    -- see more about 'touch#': https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch
    pure []

  _ => fallback op args t tc
