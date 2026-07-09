module Stg.Syntax

--import JSON.Simple.Derive
--import Derive.FromJSON.Simple

{-
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Binary
import Data.List

-- utility

-- Binder
newtype Id = Id {unId :: Binder}

instance Eq Id where
  (Id a) == (Id b) = binderUNameHash a == binderUNameHash b && binderUniqueName a == binderUniqueName b

instance Ord Id where
  compare (Id a) (Id b) = case compare (binderUNameHash a) (binderUNameHash b) of
    EQ  -> compare (binderUniqueName a) (binderUniqueName b)
    x   -> x

instance Show Id where
  show (Id a) = BS8.unpack $ binderUniqueName a

-- DataCon
newtype DC = DC {unDC :: DataCon}

instance Eq DC where
  (DC a) == (DC b) = dcUNameHash a == dcUNameHash b && dcUniqueName a == dcUniqueName b

instance Ord DC where
  compare (DC a) (DC b) = case compare (dcUNameHash a) (dcUNameHash b) of
    EQ  -> compare (dcUniqueName a) (dcUniqueName b)
    x   -> x

instance Show DC where
  show (DC a) = BS8.unpack $ dcUniqueName a

-- TyCon
newtype TC = TC {unTC :: TyCon}

instance Eq TC where
  (TC a) == (TC b) = tcUNameHash a == tcUNameHash b && tcUniqueName a == tcUniqueName b

instance Ord TC where
  compare (TC a) (TC b) = case compare (tcUNameHash a) (tcUNameHash b) of
    EQ  -> compare (tcUniqueName a) (tcUniqueName b)
    x   -> x

instance Show TC where
  show (TC a) = BS8.unpack $ tcUniqueName a
-}
-- idinfo

IdInfo = String

-- data types

StgName = String

public export
data Unique
  = MkUnique Char Int
--  deriving (Eq, Ord, Generic)
{-
instance Read Unique where
  readsPrec _d r =
    [ (Unique c (base62ToInt numStr), s)
    | (c : numStr, s) <- lex r
    ]

instance Show Unique where
 show (Unique c n) = c : intToBase62 n

base62ToInt :: String -> Int
base62ToInt numStr = sum
  [ 62^e * i
  | (e, n) <- zip ([0..] :: [Int]) $ reverse numStr
  , Just i <- [elemIndex n chars62]
  ]
 where
  chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

intToBase62 :: Int -> String
intToBase62 n_ = go n_ "" where
  go n cs | n < 62
          = let c = chooseChar62 n in c : cs
          | otherwise
          = go q (c1 : cs) where (q, r) = quotRem n 62
                                 c1 = chooseChar62 r

  chooseChar62 :: Int -> Char
  chooseChar62 n = chars62 !! n
  chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-}
-- source location related

public export
data RealSrcSpan =
  MkRealSrcSpan'
  {-srcSpanFile   -} StgName
  {-srcSpanSLine  -} Int
  {-srcSpanSCol   -} Int
  {-srcSpanELine  -} Int
  {-srcSpanECol   -} Int
--  deriving (Eq, Ord, Generic, Show)

public export
data BufSpan =
  MkBufSpan
  {-bufSpanStart  -} Int
  {-bufSpanEnd    -} Int
--  deriving (Eq, Ord, Generic, Show)

public export
data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulWiredIn
  | UnhelpfulInteractive
  | UnhelpfulGenerated
  | UnhelpfulOther StgName
--  deriving (Eq, Ord, Generic, Show)

public export
data SrcSpan
  = MkRealSrcSpan   RealSrcSpan (Maybe BufSpan)
  | UnhelpfulSpan UnhelpfulSpanReason
--  deriving (Eq, Ord, Generic, Show)

-- tickish related

public export
data Tickish
  = ProfNote
  | HpcTick
  | Breakpoint
  | SourceNote RealSrcSpan StgName
--  deriving (Eq, Ord, Generic, Show)

-- type related

public export
data PrimElemRep
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep
--  deriving (Eq, Ord, Generic, Show)

public export
data PrimRep
  = VoidRep
  | LiftedRep
  | UnliftedRep   -- ^ Unlifted pointer
  | Int8Rep       -- ^ Signed, 8-bit value
  | Int16Rep      -- ^ Signed, 16-bit value
  | Int32Rep      -- ^ Signed, 32-bit value
  | Int64Rep      -- ^ Signed, 64 bit value (with 32-bit words only)
  | IntRep        -- ^ Signed, word-sized value
  | Word8Rep      -- ^ Unsigned, 8 bit value
  | Word16Rep     -- ^ Unsigned, 16 bit value
  | Word32Rep     -- ^ Unsigned, 32 bit value
  | Word64Rep     -- ^ Unsigned, 64 bit value (with 32-bit words only)
  | WordRep       -- ^ Unsigned, word-sized value
  | AddrRep       -- ^ A pointer, but /not/ to a Haskell value (use '(Un)liftedRep')
  | FloatRep
  | DoubleRep
  | VecRep Int PrimElemRep  -- ^ A vector
--  deriving (Eq, Ord, Generic, Show)


{-
  Q: do we want to keep haskell types OR would representation type system be enough?
  A: keep only those information that is relevant for the codegen

  HINT: extrenal STG and lambda IR should be identical
-}

public export
data StgType
  = SingleValue     PrimRep
  | UnboxedTuple    (List PrimRep)
  | PolymorphicRep
--  deriving (Eq, Ord, Generic, Show)

-- data con related

public export
data TyConId = MkTyConId Unique
--  deriving (Eq, Ord, Binary, Generic, Show)

public export
data DataConId = MkDataConId Unique
--  deriving (Eq, Ord, Binary, Generic, Show)

-- raw data con
public export
data DataConRep
  = AlgDataCon      (List PrimRep)
  | UnboxedTupleCon Int
--  deriving (Eq, Ord, Generic, Show)

data SBinder : Type

public export
data SDataCon =
  MkSDataCon
  {-sdcName   :-} StgName
  {-sdcId     :-} DataConId
  {-sdcRep    :-} DataConRep
  {-sdcWorker :-} SBinder
  {-sdcDefLoc :-} SrcSpan
--  deriving (Eq, Ord, Generic, Show)

public export
data STyCon =
  MkSTyCon
  {-stcName     :-} StgName
  {-stcId       :-} TyConId
  {-stcDataCons :-} (List SDataCon)
  {-stcDefLoc   :-} SrcSpan
--  deriving (Eq, Ord, Generic, Show)


public export
data UnitId = MkUnitId StgName
--  deriving (Eq, Ord, Binary, Generic, Show)

public export
data ModuleName = MkModuleName StgName
--  deriving (Eq, Ord, Binary, Generic, Show)

-- id info

public export
data CbvMark
  = MarkedCbv
  | NotMarkedCbv
--  deriving (Eq, Ord, Generic, Show)

public export
data IdDetails
  = VanillaId
  | RecSelId
  | DataConWorkId DataConId
  | DataConWrapId DataConId
  | ClassOpId
  | PrimOpId
  | FCallId
  | TickBoxOpId
  | DFunId
  | CoVarId
  | JoinId        Int (Maybe (List CbvMark))
  | WorkerLikeId  (List CbvMark)
  | RepPolyId
--  deriving (Eq, Ord, Generic, Show)
{-
-- stg expr related


getUnitId :: UnitId -> StgName
getUnitId (UnitId n) = n


getModuleName :: ModuleName -> StgName
getModuleName (ModuleName n) = n
-}
public export
data BinderId = MkBinderId Unique
--  deriving (Eq, Ord, Binary, Generic, Show)

public export
data Scope
  = ModulePublic    -- ^ visible for every haskell module
  | ModulePrivate   -- ^ visible for a single haskell module
  | ClosurePrivate  -- ^ visible for expression body
--  deriving (Eq, Ord, Generic, Show)

public export
data SBinder =
  MkSBinder
  {-sbinderName     :-} StgName
  {-sbinderId       :-} BinderId
  {-sbinderType     :-} StgType
  {-sbinderTypeSig  :-} StgName
  {-sbinderScope    :-} Scope
  {-sbinderDetails  :-} IdDetails
  {-sbinderInfo     :-} IdInfo
  {-sbinderDefLoc   :-} SrcSpan
--  deriving (Eq, Ord, Generic, Show)
{-


mkTyConUniqueName :: UnitId -> ModuleName -> STyCon -> StgName
mkTyConUniqueName unitId modName STyCon{..} = getUnitId unitId <> "_" <> getModuleName modName <> "." <> stcName

mkDataConUniqueName :: UnitId -> ModuleName -> SDataCon -> StgName
mkDataConUniqueName unitId modName SDataCon{..} = getUnitId unitId <> "_" <> getModuleName modName <> "." <> sdcName

mkBinderUniqueName :: Bool -> UnitId -> ModuleName -> SBinder -> StgName
mkBinderUniqueName isTopLevel unitId modName SBinder{..}
 | sbinderId == rootMainBinderId
 = "main_:Main.main"

 | otherwise
 = case sbinderScope of
  ModulePublic    -> getUnitId unitId <> "_" <> getModuleName modName <> "." <> sbinderName
  ModulePrivate   -> getUnitId unitId <> "_" <> getModuleName modName <> "." <> sbinderName <> BS8.pack ('_' : show u)
  ClosurePrivate  -> if isTopLevel || True
                      then getUnitId unitId <> "_" <> getModuleName modName <> "." <> sbinderName <> BS8.pack ('_' : show u)
                      else sbinderName <> BS8.pack ('_' : show u)
  where
    BinderId u = sbinderId

rootMainBinderId :: BinderId
rootMainBinderId = BinderId $ Unique '0' 101
-}
public export
data LitNumType
  = LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt8    -- ^ @Int8#@ - exactly 8 bits
  | LitNumInt16   -- ^ @Int16#@ - exactly 16 bits
  | LitNumInt32   -- ^ @Int32#@ - exactly 32 bits
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord8   -- ^ @Word8#@ - exactly 8 bits
  | LitNumWord16  -- ^ @Word16#@ - exactly 16 bits
  | LitNumWord32  -- ^ @Word32#@ - exactly 32 bits
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
--  deriving (Eq, Ord, Generic, Show)

public export
data LabelSpec
  = FunctionLabel (Maybe Int) -- only for stdcall convention
  | DataLabel
--  deriving (Eq, Ord, Generic, Show)

public export
data Lit
  = LitChar     Char
  | LitString   String
  | LitNullAddr
  | LitFloat    Integer Integer --HINT Rational
  | LitDouble   Integer Integer --HINT Rational
  | LitLabel    String LabelSpec
  | LitNumber   LitNumType Integer
  | LitRubbish  StgType
--  deriving (Eq, Ord, Generic, Show)

public export
data AltType' tcOcc
  = PolyAlt
  | MultiValAlt Nat
  | PrimAlt     PrimRep
  | AlgAlt      tcOcc
--  deriving (Eq, Ord, Generic, Show)

public export
data UpdateFlag = ReEntrant | Updatable | SingleEntry
--  deriving (Eq, Ord, Generic, Show)

public export
data Arg' idOcc
  = StgVarArg  idOcc
  | StgLitArg  Lit
--  deriving (Eq, Ord, Generic, Show)

public export
data AltCon' dcOcc
  = AltDataCon  dcOcc
  | AltLit      Lit
  | AltDefault
--  deriving (Eq, Ord, Generic, Show)

public export
data Safety = PlaySafe | PlayInterruptible | PlayRisky
--  deriving (Eq, Ord, Generic, Show)

public export
data CCallConv = MkCCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
--  deriving (Eq, Ord, Generic, Show)

public export
data SourceText
  = MkSourceText    String
  | NoSourceText
--  deriving (Eq, Ord, Generic, Show)

public export
data CCallTarget
  = StaticTarget SourceText String (Maybe UnitId) Bool {- is function -}
  | DynamicTarget
--  deriving (Eq, Ord, Generic, Show)

public export
data ForeignCall =
  MkForeignCall
  {-foreignCTarget  :-} CCallTarget
  {-foreignCConv    :-} CCallConv
  {-foreignCSafety  :-} Safety
--  deriving (Eq, Ord, Generic, Show)

public export
data PrimCall = MkPrimCall String UnitId
--  deriving (Eq, Ord, Generic, Show)

public export
data StgOp
  = StgPrimOp     StgName
  | StgPrimCallOp PrimCall
  | StgFCallOp    ForeignCall
--  deriving (Eq, Ord, Generic, Show)


mutual
  public export
  data Binding' idBnd idOcc dcOcc tcOcc
    = StgNonRec idBnd (Rhs' idBnd idOcc dcOcc tcOcc)
    | StgRec    (List (idBnd, Rhs' idBnd idOcc dcOcc tcOcc))
  --  deriving (Eq, Ord, Generic, Show)

  -- | A top-level binding.
  public export
  data TopBinding' idBnd idOcc dcOcc tcOcc
  -- See Note [CoreSyn top-level string literals]
    = StgTopLifted    (Binding' idBnd idOcc dcOcc tcOcc)
    | StgTopStringLit idBnd String
  --  deriving (Eq, Ord, Generic, Show)

  public export
  data Expr' idBnd idOcc dcOcc tcOcc
    = StgApp
          idOcc         -- function
          (List $ Arg' idOcc)  -- arguments; may be empty

    | StgLit      Lit

          -- StgConApp is vital for returning unboxed tuples or sums
          -- which can't be let-bound first
    | StgConApp   dcOcc         -- DataCon
                  (List $ Arg' idOcc)  -- Saturated
                  (List StgType)        -- types

    | StgOpApp    StgOp         -- Primitive op or foreign call
                  (List $ Arg' idOcc)  -- Saturated.
                  StgType          -- result type
                  (Maybe tcOcc) -- result type name (required for tagToEnum wrapper generator)

    | StgCase
          (Expr' idBnd idOcc dcOcc tcOcc)     -- the thing to examine

          idBnd                               -- binds the result of evaluating the scrutinee
          (AltType' tcOcc)
          (List $ Alt' idBnd idOcc dcOcc tcOcc)      -- The DEFAULT case is always *first*
                                              -- if it is there at all

    | StgLet
          (Binding' idBnd idOcc dcOcc tcOcc)  -- right hand sides (see below)
          (Expr' idBnd idOcc dcOcc tcOcc)     -- body

    | StgLetNoEscape
          (Binding' idBnd idOcc dcOcc tcOcc)  -- right hand sides (see below)
          (Expr' idBnd idOcc dcOcc tcOcc)     -- body

    | StgTick
          Tickish
          (Expr' idBnd idOcc dcOcc tcOcc)     -- sub expression
  --  deriving (Eq, Ord, Generic, Show)

  public export
  data Rhs' idBnd idOcc dcOcc tcOcc
    = StgRhsClosure
          (List idOcc)                   -- non-global free vars
          UpdateFlag               -- ReEntrant | Updatable | SingleEntry
          (List idBnd)                   -- arguments; if empty, then not a function;
                                    -- as above, order is important.
          (Expr' idBnd idOcc dcOcc tcOcc) -- body

    | StgRhsCon
          dcOcc               -- DataCon
          (List $ Arg' idOcc)        -- Args
  --  deriving (Eq, Ord, Generic, Show)

  public export
  data Alt' idBnd idOcc dcOcc tcOcc
    = MkAlt (AltCon' dcOcc) (List idBnd) (Expr' idBnd idOcc dcOcc tcOcc)
  {-
  record Alt' idBnd idOcc dcOcc tcOcc where
    constructor MkAlt
    altCon     : AltCon' dcOcc
    altBinders : List idBnd
    altRHS     : Expr' idBnd idOcc dcOcc tcOcc
  --  deriving (Eq, Ord, Generic, Show)
  -}
-- foreign export stubs
public export
data Header = MkHeader SourceText StgName
--  deriving (Eq, Ord, Generic, Show)

public export
data CImportSpec
  = CLabel    StgName
  | CFunction CCallTarget
  | CWrapper
--  deriving (Eq, Ord, Generic, Show)

public export
data CExportSpec = CExportStatic SourceText StgName CCallConv
--  deriving (Eq, Ord, Generic, Show)

public export
data ForeignImport = CImport CCallConv Safety (Maybe Header) CImportSpec SourceText
--  deriving (Eq, Ord, Generic, Show)

public export
data ForeignExport = CExport CExportSpec SourceText
--  deriving (Eq, Ord, Generic, Show)

public export
data StubImpl
  = StubImplImportCWrapper  StgName (Maybe Int) Bool StgName (List StgName)
  | StubImplImportCApi      StgName (List (Maybe Header, String, Char))
--  deriving (Eq, Ord, Generic, Show)

public export
data StubDecl' idOcc
  = StubDeclImport ForeignImport (Maybe StubImpl)
  | StubDeclExport ForeignExport idOcc String
--  deriving (Eq, Ord, Generic, Show)

public export
data ModuleLabelKind
    = MLK_Initializer       StgName
    | MLK_InitializerArray
    | MLK_Finalizer         StgName
    | MLK_FinalizerArray
    | MLK_IPEBuffer
--  deriving (Eq, Ord, Generic, Show)

public export
data ModuleCLabel
  = MkModuleCLabel UnitId ModuleName ModuleLabelKind
--  deriving (Eq, Ord, Generic, Show)

public export
data ForeignStubs' idOcc
  = NoStubs
  | MkForeignStubs
  {-fsCHeader       :-} String
  {-fsCSource       :-} String
  {-fsInitializers  :-} (List ModuleCLabel)
  {-fsFinalizers    :-} (List ModuleCLabel)
  {-fsDecls         :-} (List $ StubDecl' idOcc)
--  deriving (Eq, Ord, Generic, Show)

-- the whole module

public export
data Module' idBnd idOcc dcOcc tcBnd tcOcc =
  MkModule
  {-modulePhase               :-} String
  {-moduleUnitId              :-} UnitId
  {-moduleName                :-} ModuleName
  {-moduleSourceFilePath      :-} (Maybe StgName) -- HINT: RealSrcSpan's source file refers to this value
  {-moduleForeignStubs        :-} (ForeignStubs' idOcc)
  {-moduleHasForeignExported  :-} Bool
  {-moduleDependency          :-} (List (UnitId, List ModuleName))
  {-moduleExternalTopIds      :-} (List (UnitId, List (ModuleName, List idBnd)))
  {-moduleTyCons              :-} (List (UnitId, List (ModuleName, List tcBnd)))
  {-moduleTopBindings         :-} (List (TopBinding' idBnd idOcc dcOcc tcOcc))
--  deriving (Eq, Ord, Generic, Show)

-- convenience layers: raw and user friendly

-- raw - as it is serialized
SModule        = Module'       SBinder BinderId DataConId STyCon  TyConId
STopBinding    = TopBinding'   SBinder BinderId DataConId TyConId
SBinding       = Binding'      SBinder BinderId DataConId TyConId
SExpr          = Expr'         SBinder BinderId DataConId TyConId
SRhs           = Rhs'          SBinder BinderId DataConId TyConId
SAlt           = Alt'          SBinder BinderId DataConId TyConId
SAltCon        = AltCon'       DataConId
SAltType       = AltType'      TyConId
SArg           = Arg'          BinderId
SStubDecl      = StubDecl'     BinderId
SForeignStubs  = ForeignStubs' BinderId

--------------

public export
record Binder where
  constructor MkBinder
  binderName      : StgName
  binderId        : BinderId
  binderType      : StgType
  binderTypeSig   : StgName
  binderScope     : Scope
  binderDetails   : IdDetails
  binderInfo      : IdInfo
  binderDefLoc    : SrcSpan
  binderUnitId    : UnitId
  binderModule    : ModuleName
  binderTopLevel  : Bool
  -- optimization
  binderUniqueName  : StgName
  binderUNameHash   : Int
--  deriving (Eq, Ord, Generic, Show)

record TyCon

public export
data CutTyCon = MkCutTyCon (Lazy TyCon)
{-
instance Eq CutTyCon where _ == _ = True
instance Ord CutTyCon where compare _ _ = EQ
instance Show CutTyCon where show (CutTyCon tc) = "CutTyCon " ++ (BS8.unpack $ tcUniqueName tc)
-}

-- user friendly data con
public export
record DataCon where
  constructor MkDataCon
  dcName   : StgName
  dcId     : DataConId
  dcUnitId : UnitId
  dcModule : ModuleName
  dcRep    : DataConRep
  dcTyCon  : CutTyCon
  dcWorker : Binder
  dcDefLoc : SrcSpan
  -- optimization
  dcUniqueName  : StgName
  dcUNameHash   : Int
--  deriving (Eq, Ord, Generic, Show)

public export
record TyCon where
  constructor MkTyCon
  tcName      : StgName
  tcId        : TyConId
  tcUnitId    : UnitId
  tcModule    : ModuleName
  tcDataCons  : List DataCon
  tcDefLoc    : SrcSpan
  -- optimization
  tcUniqueName  : StgName
  tcUNameHash   : Int
--  deriving (Eq, Ord, Generic, Show)

-- user friendly - rich information

Module       = Module'       Binder Binder DataCon TyCon TyCon
TopBinding   = TopBinding'   Binder Binder DataCon TyCon
Binding      = Binding'      Binder Binder DataCon TyCon
Expr         = Expr'         Binder Binder DataCon TyCon
Rhs          = Rhs'          Binder Binder DataCon TyCon
Alt          = Alt'          Binder Binder DataCon TyCon
AltCon       = AltCon'       DataCon
AltType      = AltType'      TyCon
Arg          = Arg'          Binder
StubDecl     = StubDecl'     Binder
ForeignStubs = ForeignStubs' Binder

{-
  high level custom types
    done - CutTyCon
    done - DataCon
    done - TyCon
    done - Binder
    Module
-}