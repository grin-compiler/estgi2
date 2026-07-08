module Main

import System
import System.File
import JSON.Simple
import Stg.Syntax
import Stg.JSON
import Stg.Reconstruct
import Stg.GenMySyntax

import Data.SortedMap

import Control.Monad.State

import Base
import Interpreter

export
decodeSModule : String -> Either String SModule
--decodeSModule : String -> Either String Syntax.Unique -- ok
--decodeSModule : String -> Either String Syntax.BinderId -- ok
--decodeSModule : String -> Either String Syntax.SrcSpan
--decodeSModule : String -> Either String Syntax.RealSrcSpan
--decodeSModule : String -> Either String Syntax.DataConRep
--decodeSModule : String -> Either String (List Syntax.PrimRep)
decodeSModule = decodeEither

export
decodePaths : String -> Either String (List String)
decodePaths = decodeEither

loadSModule : String -> IO SModule
loadSModule fp = do
  Right only <- readFile fp
    | Left err => die (show err)

  let Right json = decodeSModule only
        | Left err => die (show err)

  let MkModule _ (MkUnitId u) (MkModuleName m) _ _ _ _ _ _ _ = json
  putStrLn "\{u} - \{m}"
  pure json

loadModule : String -> IO Module
loadModule fp = do
  m <- loadSModule fp
  let MkModule _ (MkUnitId u) (MkModuleName mn) _ _ _ _ _ _ _ = m
  --putStrLn "reconstructing: \{mn}"
  r <- reconModule m
  --putStrLn "reconstructed: \{mn}"
  pure r

loadProgram : String -> IO (List Module)
loadProgram fp = do
  putStrLn "converting modules to json"
  (out, 0) <- run "stgapp gen-json \{fp}"
    | (err, _) => die err

  let Right paths = decodePaths out
        | Left err => die (show err)

  putStrLn "loading modules"

  traverse loadModule paths

main : IO ()
main = do
  _ :: fp :: _ <- getArgs
    | _ => putStrLn "usage: idr-stgapp FILE"

  putStrLn "parsing: \{fp}"
  mods <- loadProgram fp

  let tops = concatMap topBindings $ concatMap gettops mods

  rootMain <- case [i | i <- tops, i.binderUniqueName == "main_:Main.main"] of
              [mainId]  => pure mainId
              []        => die "main_:Main.main not found"
              _         => die "multiple main_:Main.main have found"

  putStrLn $ show rootMain
  putStrLn "SUCCESS"

  let run = do
        --when switchCWD $ liftIO $ setCurrentDirectory stgappDir
        declareTopBindings mods
        --buildCWrapperHsTypeMap mods
        --initRtsSupport progName progArgs mods
        limit <- gets ssNextHeapAddr
        modify {ssDynamicHeapStart := limit}

        -- TODO: check how it is done in the native RTS: call hs_main
        mainAtom <- lookupEnv empty rootMain

        evalOnMainThread $ do
          stackPush $ Apply [Void]
          pure [mainAtom]

  s <- execStateT emptyStgState run
  pure ()


{-
main : IO ()
main = do
  _ :: fp :: _ <- getArgs
    | _ => putStrLn "usage: idr-stgapp FILE"

  putStrLn "parsing: \{fp}"
  m <- loadSModule fp
  putStrLn "loaded: \{fp}"
  let r = reconModule m
  putStrLn "reconstructed: \{fp}"
  putStrLn "SUCCESS"
-}