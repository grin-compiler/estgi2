module FFI

import Control.Monad.State
import Data.SortedSet
import Data.SortedMap

import Stg.Syntax
import Stg.JSON
import Base

import Rts
import FFI.Rts

import GHC.Symbols

rtsSymbolSet : SortedSet StgName
rtsSymbolSet = fromList $ map getSymbolName rtsSymbols

export
evalFCallOp : EvalOnNewThread -> ForeignCall -> List Atom -> StgType -> Maybe TyCon -> M (List Atom)
evalFCallOp evalOnNewThread fCall@(MkForeignCall foreignCTarget foreignCConv foreignCSafety) args t tc = do
    --liftIO $ putStrLn $ "[evalFCallOp]  " ++ show foreignCTarget ++ " " ++ show args
    case foreignCTarget of

      ----------------
      -- GHC RTS FFI
      ----------------
      {-
      -- support for exporting haskell function (GHC RTS specific)
      StaticTarget _ "createAdjustor" _ _
        | [ IntV 1
          , PtrAtom StablePtr{} sp
          , Literal (LitLabel wrapperName _)
          , PtrAtom CStringPtr{} _
          , Void
          ] <- args
        , UnboxedTuple [AddrRep] <- t
        -> do
          --promptM $ putStrLn $ "[createAdjustor FFI]"
          fun@HeapPtr{} <- lookupStablePointerPtr sp
          cwrapperDesc <- lookupCWrapperHsType wrapperName
          -- FIXME: _freeWrapper needs to be called otherwise it will leak the memory!!!!
          (funPtr, _freeWrapper) <- createAdjustor evalOnNewThread fun cwrapperDesc
          pure [PtrAtom RawPtr $ castFunPtrToPtr funPtr]

      -- GHC RTS global store getOrSet function implementation
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol globalStoreSymbols
        , [value, Void] <- args
        -> do
            --promptM $ putStrLn $ "[global store FFI] " ++ show foreignSymbol
            -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
            store <- gets $ rtsGlobalStore . ssRtsSupport
            case Map.lookup foreignSymbol store of
              Nothing -> state $ \s@StgState{..} -> ([value], s {ssRtsSupport = ssRtsSupport {rtsGlobalStore = Map.insert foreignSymbol value store}})
              Just v  -> pure [v]
      -}
      StaticTarget _ foreignSymbol _ _ =>
        -- GHC RTS global store getOrSet function implementation
        if contains foreignSymbol globalStoreSymbols then do
          let [value, Void] = args | _ => stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args
          -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
          store <- gets $ rtsGlobalStore . ssRtsSupport
          case lookup foreignSymbol store of
            Nothing => state $ \s => ({ssRtsSupport.rtsGlobalStore := insert foreignSymbol value store} s, [value])
            Just v  => pure [v]

        -- calls to GHC RTS
        else if contains foreignSymbol rtsSymbolSet then do
          FFI.Rts.evalFCallOp evalOnNewThread fCall args t tc

        else stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

      {-
      -- calls to emulated lib native functions
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol emulatedLibrarySymbolSet
        -> do
          --promptM $ putStrLn $ "[emulated user FFI] " ++ show foreignSymbol
          EmulatedLibFFI.evalFCallOp evalOnNewThread fCall args t tc

      --------------
      -- user FFI
      --------------
      StaticTarget _ foreignSymbol _ _
        -> do
          let blacklist =
                [ "__gmpn"
                ]
          { -
          unless (any (`BS8.isPrefixOf` foreignSymbol) blacklist) $ do
            liftIO $ do
              now <- getCurrentTime
              putStrLn $ "[foreign call]  " ++ show now ++ "  " ++ show foreignSymbol ++ " " ++ show args ++ show foreignCTarget
          - }
          --promptM $ putStrLn $ "[user FFI] " ++ show foreignSymbol
          ts <- getCurrentThreadState
          unless (tsLabel ts == Just "resource-pool: reaper") $ do
            traceLog $ show foreignSymbol ++ "\t" ++ show args

          cArgs <- catMaybes <$> mapM mkFFIArg args
          funPtr <- getFFISymbol foreignSymbol
          liftIOAndBorrowStgState $ do
            evalForeignCall funPtr cArgs t

      DynamicTarget
        | (PtrAtom RawPtr funPtr) : funArgs <- args
        -> do
          cArgs <- catMaybes <$> mapM mkFFIArg funArgs
          liftIOAndBorrowStgState $ do
            evalForeignCall (castPtrToFunPtr funPtr) cArgs t
      -}
      _ => stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args
