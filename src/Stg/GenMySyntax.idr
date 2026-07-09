module Stg.GenMySyntax

import Deriving.Common
import Language.Reflection
import Stg.Syntax

-- gen haskell module

%language ElabReflection

defListADT : List TTImp
defListADT =
  [ `(Stg.Syntax.Unique)
  , `(Stg.Syntax.UnhelpfulSpanReason)
  , `(Stg.Syntax.SrcSpan)
  , `(Stg.Syntax.Tickish)
  , `(Stg.Syntax.PrimElemRep)
  , `(Stg.Syntax.PrimRep)
  , `(Stg.Syntax.StgType)
  , `(Stg.Syntax.TyConId)
  , `(Stg.Syntax.DataConId)
  , `(Stg.Syntax.DataConRep)
  , `(Stg.Syntax.UnitId)
  , `(Stg.Syntax.ModuleName)
  , `(Stg.Syntax.CbvMark)
  , `(Stg.Syntax.IdDetails)
  , `(Stg.Syntax.BinderId)
  , `(Stg.Syntax.Scope)
  , `(Stg.Syntax.LitNumType)
  , `(Stg.Syntax.LabelSpec)
  , `(Stg.Syntax.Lit)
  , `(Stg.Syntax.AltType' tcOcc)
  , `(Stg.Syntax.UpdateFlag)
  , `(Stg.Syntax.Arg' idOcc)
  , `(Stg.Syntax.AltCon' dcOcc)
  , `(Stg.Syntax.Safety)
  , `(Stg.Syntax.CCallConv)
  , `(Stg.Syntax.SourceText)
  , `(Stg.Syntax.CCallTarget)
  , `(Stg.Syntax.PrimCall)
  , `(Stg.Syntax.StgOp)
  , `(Stg.Syntax.Binding' idBnd idOcc dcOcc tcOcc)
  , `(Stg.Syntax.TopBinding' idBnd idOcc dcOcc tcOcc)
  , `(Stg.Syntax.Expr' idBnd idOcc dcOcc tcOcc)
  , `(Stg.Syntax.Rhs' idBnd idOcc dcOcc tcOcc)
  , `(Stg.Syntax.Alt' idBnd idOcc dcOcc tcOcc)
  , `(Stg.Syntax.Header)
  , `(Stg.Syntax.CImportSpec)
  , `(Stg.Syntax.CExportSpec)
  , `(Stg.Syntax.ForeignImport)
  , `(Stg.Syntax.ForeignExport)
  , `(Stg.Syntax.StubImpl)
  , `(Stg.Syntax.StubDecl' idOcc)
  , `(Stg.Syntax.ModuleLabelKind)
  , `(Stg.Syntax.ModuleCLabel)
  ]

defListRec : List TTImp
defListRec =
  [ `(Stg.Syntax.RealSrcSpan)
  , `(Stg.Syntax.BufSpan)
  , `(Stg.Syntax.SDataCon)
  , `(Stg.Syntax.STyCon)
  , `(Stg.Syntax.SBinder)
  , `(Stg.Syntax.ForeignCall)
  , `(Stg.Syntax.ForeignStubs' idOcc)
  , `(Stg.Syntax.Module' idBnd idOcc dcOcc tcBnd tcOcc)
  ]

public export
pp1 : String -> String -> Elab ()
pp1 fname str = do
  Just s <- readFile ProjectDir fname
    | Nothing => writeFile ProjectDir fname $ str ++ "\n"
  writeFile ProjectDir fname $ s ++ str ++ "\n"

public export
pp2 : String -> String -> Elab ()
pp2 fname str = do
  Just s <- readFile ProjectDir fname
    | Nothing => writeFile ProjectDir fname $ str
  writeFile ProjectDir fname $ s ++ str

public export
procType : Bool -> String -> TTImp -> Elab ()
procType isRecord fname t = do
  let pp = pp1 fname
  let pp' = pp2 fname
  it <- isType t
  --unless (null it.parameterNames) $ fail "\{show it.typeConstructor} has type parameters"
  pp "data \{show $ dropNS it.typeConstructor}\{concat $ map (((++) " ") . show . unArg . fst) $ it.parameterNames}"
  let go : String -> List (Name, TTImp) -> Elab ()
      go _ [] = pure ()
      go sym ((conName, conTm) :: xs) = do
        let Just cv = constructorView conTm
             | _ => pp "\{show conName} - constructorView fail"
        -- Only keep the visible arguments
        let args : List TTImp
            args = map snd $ mapMaybe (isExplicit . snd) cv.conArgTypes
        --unless (null cv.params) $ fail "\{show conName} has con parameters"
        if isRecord
          then pp "  \{sym} \{show $ dropNS conName}" >> pp' "\{unlines $ map (((++) "      ") . showPrec App . mapTTImp cleanup) $ args}"
          else pp "  \{sym} \{show $ dropNS conName}\{concat $ map (((++) " ") . showPrec App . mapTTImp cleanup) $ args}"
        go "|" xs
  go "=" it.dataConstructors
  --pp "$(deriveJSON defaultOptions ''\{show $ dropNS it.typeConstructor})"
  pp "  deriving Generic"
  pp ""
  pure ()

public export
procTypeADT : String -> TTImp -> Elab ()
procTypeADT = procType False

public export
procTypeRec : String -> TTImp -> Elab ()
procTypeRec = procType True

{-
  IDEA:
    use elab reflection to:
      - generate identical haskell data definitions
      - generate serialization finctions between idris and haskell
  TODO:
    done - read examples in Idris2/tests/idris2/reflection/
    done - read: Idris2/src/TTImp/Elab/RunElab.idr
    - write haskell data defs to file
      see: Idris2/tests/idris2/reflection/reflection024/src/TypeProviders.idr
-}

{-
  use ghc generic and generic aeson with options
-}
public export
procjson : String -> TTImp -> Elab ()
procjson fname t = do
  let pp = pp1 fname
  let pp' = pp2 fname
  it <- isType t
  --pp "$(deriveJSON defaultOptions ''\{show $ dropNS it.typeConstructor})"
  [] <- pure ["ToJSON " ++ (show . unArg . fst $ p) | p <- it.parameterNames]
    | l => do
            let ctx = "("++ joinBy ", " l ++ ") => "
            pp "instance \{ctx}ToJSON (\{show $ dropNS it.typeConstructor}\{concat $ map (((++) " ") . show . unArg . fst) $ it.parameterNames}) where toEncoding = genericToEncoding jsonOptions"
  pp "instance ToJSON \{show $ dropNS it.typeConstructor} where toEncoding = genericToEncoding jsonOptions"
  pure ()

%runElab do
  let fname = "MySyntax.hs"
  writeFile ProjectDir fname """
        {-# LANGUAGE DeriveGeneric #-}
        module Stg.MySyntax where

        import GHC.Generics
        import Data.Aeson

        jsonTaggedObject :: SumEncoding
        jsonTaggedObject = TaggedObject
                      { tagFieldName      = "tag"
                      , contentsFieldName = "contents"
                      }

        jsonOptions :: Options
        jsonOptions = defaultOptions
                 { fieldLabelModifier      = id
                 , constructorTagModifier  = id
                 , allNullaryToStringTag   = False
                 , omitNothingFields       = False
                 -- , allowOmittedFields      = True
                 , sumEncoding             = TwoElemArray -- jsonTaggedObject
                 , unwrapUnaryRecords      = False
                 , tagSingleConstructors   = True
                 , rejectUnknownFields     = False
                 }

        type List a = [a]
        type Pair a b = (a, b)
        type Nat = Int

        type StgName = String
        type IdInfo = String

        """
  for_ defListADT $ procTypeADT fname
  for_ defListRec $ procTypeRec fname
  for_ defListADT $ procjson fname
  for_ defListRec $ procjson fname
