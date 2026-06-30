module Stg.JSON

import JSON.Simple.Derive
import JSON.Simple
import Stg.Syntax

%language ElabReflection


defListADT : List Name
defListADT =
  [ "Syntax.Unique"
  , "Syntax.RealSrcSpan"
  , "Syntax.BufSpan"
  , "Syntax.UnhelpfulSpanReason"
  , "Syntax.SrcSpan"
  , "Syntax.Tickish"
  , "Syntax.PrimElemRep"
  , "Syntax.PrimRep"
  , "Syntax.StgType"
  , "Syntax.TyConId"
  , "Syntax.DataConId"
  , "Syntax.DataConRep"
  , "Syntax.CbvMark"
  , "Syntax.IdDetails"
  , "Syntax.Scope"
  , "Syntax.BinderId"
  , "Syntax.SBinder"
  , "Syntax.SDataCon"
  , "Syntax.STyCon"
  , "Syntax.UnitId"
  , "Syntax.ModuleName"
  , "Syntax.LitNumType"
  , "Syntax.LabelSpec"
  , "Syntax.Lit"
  , "Syntax.AltType'"
  , "Syntax.UpdateFlag"
  , "Syntax.Arg'"
  , "Syntax.AltCon'"
  , "Syntax.Safety"
  , "Syntax.CCallConv"
  , "Syntax.SourceText"
  , "Syntax.CCallTarget"
  , "Syntax.ForeignCall"
  , "Syntax.PrimCall"
  , "Syntax.StgOp"
  , "Syntax.Expr'"
  , "Syntax.Alt'"
  , "Syntax.Rhs'"
  , "Syntax.Binding'"
  , "Syntax.TopBinding'"
  , "Syntax.Header"
  , "Syntax.CImportSpec"
  , "Syntax.CExportSpec"
  , "Syntax.ForeignImport"
  , "Syntax.ForeignExport"
  , "Syntax.StubImpl"
  , "Syntax.StubDecl'"
  , "Syntax.ModuleLabelKind"
  , "Syntax.ModuleCLabel"
  , "Syntax.ForeignStubs'"
  , "Syntax.Module'"
  ]


{-
||| Specifies how to encode constructors of a sum datatype.
public export
data SumEncoding : Type where
  ||| Constructor names won't be encoded. Instead only the contents of the
  ||| constructor will be encoded as if the type had a single constructor. JSON
  ||| encodings have to be disjoint for decoding to work properly.
  |||
  ||| When decoding, constructors are tried in the order of definition. If some
  ||| encodings overlap, the first one defined will succeed.
  |||
  ||| Note: Nullary constructors are encoded as strings (using
  ||| constructorTagModifier).  Having a nullary constructor
  ||| alongside a single field constructor that encodes to a
  ||| string leads to ambiguity.
  |||
  ||| Note: Only the last error is kept when decoding, so in the case of
  ||| malformed JSON, only an error for the last constructor will be reported.
  UntaggedValue         : SumEncoding

  ||| A constructor will be encoded to an object with a single field named
  ||| after the constructor tag (modified by the constructorTagModifier) which
  ||| maps to the encoded contents of the constructor.
  ObjectWithSingleField : SumEncoding

  ||| A constructor will be encoded to a 2-element array where the first
  ||| element is the tag of the constructor (modified by the constructorTagModifier)
  ||| and the second element the encoded contents of the constructor.
  TwoElemArray          : SumEncoding

  ||| A constructor will be encoded to an object with a field `tagFieldName`
  ||| which specifies the constructor tag (modified by the
  ||| constructorTagModifier). If the constructor is a record the
  ||| encoded record fields will be unpacked into this object. So
  ||| make sure that your record doesn't have a field with the
  ||| same label as the tagFieldName.  Otherwise the tag gets
  ||| overwritten by the encoded value of that field! If the constructor
  ||| is not a record the encoded constructor contents will be
  ||| stored under the contentsFieldName field.
  TaggedObject          :  (tagFieldName : String)
                        -> (contentsFieldName : String)
                        -> SumEncoding

defaultTaggedObject : SumEncoding
defaultTaggedObject = TaggedObject "tag" "contents"

jsonTaggedObject :: SumEncoding
jsonTaggedObject = TaggedObject
              { tagFieldName      = "tag"
              , contentsFieldName = "contents"
              }


  haskell options
jsonOptions :: Options
jsonOptions = defaultOptions
         , sumEncoding             = jsonTaggedObject   -- done
         , unwrapUnaryRecords      = False
         , allNullaryToStringTag   = True
         , omitNothingFields       = False
         -- , allowOmittedFields      = True
         , tagSingleConstructors   = False
         , rejectUnknownFields     = False
         , fieldLabelModifier      = id                 -- done
         , constructorTagModifier  = id                 -- done
         }

record Options where
  constructor MkOptions
  ||| How to encode sum types
  sum                        : SumEncoding
  ||| If `True`, the single field from a unary data constructor
  ||| will be unwrapped.
  unwrapUnary                : Bool
  ||| If `True`, missing keys in a JSON objects will be
  ||| replaced with `Null` during decoding.
  replaceMissingKeysWithNull : Bool
  ||| If `True`, single constructor data types will be
  ||| encoded without a tag for the constructor name.
  unwrapRecords              : Bool
  ||| This function is used to adjust constructor names
  ||| during encoding and decoding
  constructorTagModifier     : String -> String
  ||| This function is used to adjust constructor argument names
  ||| during encoding and decoding
  fieldNameModifier          : String -> String

-}
opts2 : Options
opts2 = MkOptions TwoElemArray True False False id id

%runElab deriveMutual defListADT [Show, Eq, Ord, customToJSON Export opts2, customFromJSON Export opts2]
{-
decodeEither : FromJSON a => String -> Either String a
decodeEither s = mapFst interpolate $ decode s
-}
Show CutTyCon where show _ = "CutTyCon"
Eq CutTyCon where (==) _ _ = True
Ord CutTyCon where compare _ _ = EQ

%runElab deriveMutual ["Syntax.Binder", "Syntax.DataCon", "Syntax.TyCon"] [Show, Eq, Ord]
