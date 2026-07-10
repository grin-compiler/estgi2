module Stg.Reconstruct

--import Data.Hashable
--import qualified Data.HashMap.Lazy as HM
import Data.SortedMap
import Data.String

import Stg.Syntax
import Stg.JSON

{-
instance Hashable BinderId where
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

instance Hashable DataConId where
    hashWithSalt salt (DataConId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

instance Hashable TyConId where
    hashWithSalt salt (TyConId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i
-}
record BinderMap where
  constructor MkBinderMap
  bmUnitId      : UnitId
  bmModule      : ModuleName
  bmIdMap       : SortedMap BinderId Binder
  bmDataConMap  : SortedMap DataConId DataCon
  bmTyConMap    : SortedMap TyConId TyCon

-- Id handling
insertBinder : Binder -> BinderMap -> BinderMap
insertBinder b bm = {bmIdMap $= insert (binderId b) b} bm

insertBinders : List Binder -> BinderMap -> BinderMap
insertBinders bs bm = foldl (flip insertBinder) bm bs

getBinder : BinderMap -> BinderId -> Binder
getBinder bm bid = case lookup bid bm.bmIdMap of
  Just b  => b
  Nothing => assert_total $ idris_crash $ "unknown binder "++ show bid ++ ":\nin scope:\n" ++
              unlines (map (\(bid',b) => show bid' ++ "\t" ++ show b) (kvList bm.bmIdMap))

-- DataCon handling
insertDataCon : DataCon -> BinderMap -> BinderMap
insertDataCon dc bm = {bmDataConMap $= insert (dcId dc) dc} bm

insertDataCons : List DataCon -> BinderMap -> BinderMap
insertDataCons dcs bm = foldl (flip insertDataCon) bm dcs

getDataCon : BinderMap -> DataConId -> DataCon
getDataCon bm bid = case lookup bid bm.bmDataConMap of
  Just b  => b
  Nothing => assert_total $ idris_crash $ "unknown data con "++ show bid ++ ":\nin scope:\n" ++
              unlines (map (\(bid',b) => show bid' ++ "\t" ++ show b) (kvList bm.bmDataConMap))

-- TyCon handling
getTyCon : BinderMap -> TyConId -> TyCon
getTyCon bm i = case lookup i bm.bmTyConMap of
  Just b  => b
  Nothing => assert_total $ idris_crash $ "unknown ty con "++ show i ++ ":\nin scope:\n" ++
              unlines (map (\(i',b) => show i' ++ "\t" ++ show b) (kvList bm.bmTyConMap))

mkTyConUniqueName : UnitId -> ModuleName -> STyCon -> StgName
mkTyConUniqueName (MkUnitId unitId) (MkModuleName modName) (MkSTyCon stcName _ _ _) = unitId ++ "_" ++ modName ++ "." ++ stcName

mkDataConUniqueName : UnitId -> ModuleName -> SDataCon -> StgName
mkDataConUniqueName (MkUnitId unitId) (MkModuleName modName) (MkSDataCon sdcName _ _ _ _) = unitId ++ "_" ++ modName ++ "." ++ sdcName

rootMainBinderId : BinderId
rootMainBinderId = MkBinderId $ MkUnique '0' 101

mkBinderUniqueName : Bool -> UnitId -> ModuleName -> SBinder -> StgName
mkBinderUniqueName isTopLevel (MkUnitId unitId) (MkModuleName modName) (MkSBinder sbinderName sbinderId@(MkBinderId u) sbinderType sbinderTypeSig sbinderScope sbinderDetails sbinderInfo sbinderDefLoc) =
  if sbinderId == rootMainBinderId
    then "main_:Main.main"
    else case sbinderScope of
      ModulePublic    => unitId ++ "_" ++ modName ++ "." ++ sbinderName
      ModulePrivate   => unitId ++ "_" ++ modName ++ "." ++ sbinderName ++ singleton '_' ++ ppUnique u
      ClosurePrivate  => if isTopLevel || True
                          then unitId ++ "_" ++ modName ++ "." ++ sbinderName ++ singleton '_' ++ ppUnique u
                          else sbinderName ++ singleton '_' ++ ppUnique u

-- "recon" == "reconstruct"
reconLocalBinder : BinderMap -> SBinder -> Binder
reconLocalBinder bm b@(MkSBinder sbinderName sbinderId sbinderType sbinderTypeSig sbinderScope sbinderDetails sbinderInfo sbinderDefLoc) = -- HINT: local binders only
  MkBinder
  { binderName        = sbinderName
  , binderId          = sbinderId
  , binderType        = sbinderType
  , binderTypeSig     = sbinderTypeSig
  , binderDetails     = sbinderDetails
  , binderInfo        = sbinderInfo
  , binderDefLoc      = sbinderDefLoc
  , binderUnitId      = bm.bmUnitId
  , binderModule      = bm.bmModule
  , binderScope       = ClosurePrivate
  , binderTopLevel    = False
  , binderUniqueName  = uName
  , binderUNameHash   = 0 -- TODO: hash uName
  } where uName = mkBinderUniqueName False bm.bmUnitId bm.bmModule b

mkTopBinder : UnitId -> ModuleName -> Scope -> SBinder -> Binder

reconDataCon : UnitId -> ModuleName -> Lazy TyCon -> SDataCon -> DataCon
reconDataCon u m tc sdc@(MkSDataCon sdcName sdcId sdcRep sdcWorker@(MkSBinder _ _ _ _ sbinderScope _ _ _) sdcDefLoc) = MkDataCon
  { dcName    = sdcName
  , dcId      = sdcId
  , dcUnitId  = u
  , dcModule  = m
  , dcRep     = sdcRep
  , dcWorker  = mkTopBinder u m sbinderScope sdcWorker
  , dcDefLoc  = sdcDefLoc
  , dcTyCon   = MkCutTyCon tc
  , dcUniqueName  = uName
  , dcUNameHash   = 0 -- TODO: hash uName
  } where uName = mkDataConUniqueName u m sdc

reconTyCon : UnitId -> ModuleName -> STyCon -> TyCon
reconTyCon u m stc@(MkSTyCon stcName stcId stcDataCons stcDefLoc) = tc where
  tc = MkTyCon
    { tcName        = stcName
    , tcId          = stcId
    , tcUnitId      = u
    , tcModule      = m
    , tcDataCons    = map (reconDataCon u m tc) stcDataCons
    , tcDefLoc      = stcDefLoc
    , tcUniqueName  = uName
    , tcUNameHash   = 0 -- TODO: hash uName
    } where uName = mkTyConUniqueName u m stc

mkTopBinder u m scope (MkSBinder sbinderName sbinderId sbinderType sbinderTypeSig sbinderScope sbinderDetails sbinderInfo sbinderDefLoc) =
  MkBinder
  { binderName        = sbinderName
  , binderId          = sbinderId
  , binderType        = sbinderType
  , binderTypeSig     = sbinderTypeSig
  , binderDetails     = sbinderDetails
  , binderInfo        = sbinderInfo
  , binderDefLoc      = sbinderDefLoc
  , binderUnitId      = u
  , binderModule      = m
  , binderScope       = scope
  , binderTopLevel    = True
  , binderUniqueName  = uName
  , binderUNameHash   = 0 -- TODO: hash uName
  } where uName = mkBinderUniqueName True u m (MkSBinder sbinderName sbinderId sbinderType sbinderTypeSig scope sbinderDetails sbinderInfo sbinderDefLoc)

export topBindings : TopBinding' idBnd idOcc dcOcc tcOcc -> List idBnd

reconRhs : BinderMap -> SRhs -> Rhs
reconForeignStubs : BinderMap -> SForeignStubs -> ForeignStubs

export
reconModule : SModule -> IO Module
reconModule (MkModule
              modulePhase
              moduleUnitId
              moduleName
              moduleSourceFilePath
              moduleForeignStubs
              moduleHasForeignExported
              moduleDependency
              moduleExternalTopIds
              moduleTyCons
              moduleTopBindings
            ) = do
  let tyConList : List (UnitId, List (ModuleName, List TyCon))
      tyConList = [(u, [(m, map (reconTyCon u m) l) | (m, l) <- ml]) | (u, ml) <- moduleTyCons]
  --putStrLn "tyConList len: \{show $ length tyConList}"

  let tyCons : List TyCon
      tyCons = concatMap (concatMap snd . snd) tyConList
  --putStrLn "tyCons len: \{show $ length tyCons}"

  let cons : List DataCon
      cons = concatMap tcDataCons tyCons
  --putStrLn "cons len: \{show $ length cons}"

  let exts : List (UnitId, List (ModuleName, List Binder))
      exts = [(u, [(m, map (mkTopBinder u m ModulePublic) l) | (m, l) <- ml]) | (u, ml) <- moduleExternalTopIds]
  --putStrLn "exts len: \{show $ length exts}"

  let tops : List Binder
      tops  = [ mkTopBinder moduleUnitId moduleName sbinderScope b
              | b <- concatMap topBindings moduleTopBindings
              , let (MkSBinder _ _ _ _ sbinderScope _ _ _) = b
              ]
  --putStrLn "tops len: \{show $ length tops}"

  --putStrLn "bm"
  let bm = MkBinderMap
       { bmUnitId      = moduleUnitId
       , bmModule      = moduleName
       , bmIdMap       = fromList [(binderId b, b) | b <- tops ++ concatMap snd (concatMap snd exts)]
       , bmDataConMap  = fromList [(dcId dc, dc) | dc <- cons]
       , bmTyConMap    = fromList [(tcId tc, tc) | tc <- tyCons]
       }

  --putStrLn "stubs"
  let stubs : ForeignStubs
      stubs = reconForeignStubs bm moduleForeignStubs

  --putStrLn "reconTopBinder"
  let reconTopBinder : SBinder -> Binder
      reconTopBinder (MkSBinder _ sbinderId _ _ _ _ _ _) = getBinder bm sbinderId

  --putStrLn "reconTopBinding"
  let reconTopBinding : STopBinding -> TopBinding
      reconTopBinding = \case
        StgTopStringLit b s           => StgTopStringLit (reconTopBinder b) s
        StgTopLifted (StgNonRec b r)  => StgTopLifted $ StgNonRec (reconTopBinder b) (reconRhs bm r)
        StgTopLifted (StgRec bs)      => StgTopLifted $ StgRec [(reconTopBinder b, reconRhs bm r) | (b,r) <- bs]

  --putStrLn "binds"
  let binds : List TopBinding
      binds = map reconTopBinding moduleTopBindings
  --putStrLn "binds len: \{show $ length binds}"

  --putStrLn "DONE"
  pure $ MkModule
          modulePhase
          moduleUnitId
          moduleName
          moduleSourceFilePath
          stubs
          moduleHasForeignExported
          moduleDependency
          exts
          tyConList
          binds

reconStubDecl : BinderMap -> SStubDecl -> StubDecl
reconStubDecl bm = \case
  StubDeclImport f m    => StubDeclImport f m
  StubDeclExport f i s  => StubDeclExport f (getBinder bm i) s

reconForeignStubs bm = \case
  NoStubs                   => NoStubs
  MkForeignStubs h c i f l  => MkForeignStubs h c i f $ map (reconStubDecl bm) l

topBindings = \case
  StgTopLifted (StgNonRec b _)  => [b]
  StgTopLifted (StgRec bs)      => map fst bs
  StgTopStringLit b _           => [b]

reconAltType : BinderMap -> SAltType -> AltType
reconAlt : BinderMap -> SAlt -> Alt
reconArg : BinderMap -> SArg -> Arg
reconBinding : BinderMap -> SBinding -> (BinderMap, Binding)

reconExpr : BinderMap -> SExpr -> Expr
reconExpr bm = \case
  StgLit l              => StgLit l
  StgCase x b at alts   => let b'   = reconLocalBinder bm b
                               bm'  = insertBinder b' bm
                           in StgCase (reconExpr bm x) b' (reconAltType bm at) (map (reconAlt bm') alts)
  StgApp f args         => StgApp (getBinder bm f) (map (reconArg bm) args)
  StgOpApp op args t tc => StgOpApp op (map (reconArg bm) args) t (getTyCon bm <$> tc)
  StgConApp dc args t   => StgConApp (getDataCon bm dc) (map (reconArg bm) args) t
  StgLet b e            => let (bm', b') = reconBinding bm b
                           in StgLet b' (reconExpr bm' e)
  StgLetNoEscape b e    => let (bm', b') = reconBinding bm b
                           in StgLetNoEscape b' (reconExpr bm' e)
  StgTick t e           => StgTick t (reconExpr bm e)

reconBinding bm = \case
  StgNonRec b r => let b'   = reconLocalBinder bm b
                       bm'  = insertBinder b' bm
                   in (bm', StgNonRec b' (reconRhs bm' r))
  StgRec bs     => let bs'  = map (reconLocalBinder bm . fst) bs
                       bm'  = insertBinders bs' bm
                   in (bm', StgRec [(b, reconRhs bm' r) | ((_,r), b) <- zip bs bs'])

reconRhs bm = \case
  StgRhsCon dc vs         => StgRhsCon (getDataCon bm dc) $ map (reconArg bm) vs
  StgRhsClosure fs u bs e => let  fs' = map (getBinder bm) fs
                                  bs' = map (reconLocalBinder bm) bs
                                  bm' = insertBinders bs' bm
                              in StgRhsClosure fs' u bs' (reconExpr bm' e)

reconArg bm = \case
  StgVarArg b => StgVarArg $ getBinder bm b
  StgLitArg l => StgLitArg l

reconAltCon : BinderMap -> SAltCon -> AltCon
reconAltCon bm = \case
  AltDataCon dc => AltDataCon $ getDataCon bm dc
  AltLit l      => AltLit l
  AltDefault    => AltDefault

reconAlt bm (MkAlt con bs rhs) =
    let bs' = map (reconLocalBinder bm) bs
        bm' = insertBinders bs' bm
    in MkAlt (reconAltCon bm con) bs' (reconExpr bm' rhs)

reconAltType bm = \case
  PolyAlt       => PolyAlt
  MultiValAlt i => MultiValAlt i
  PrimAlt r     => PrimAlt r
  AlgAlt tc     => AlgAlt $ getTyCon bm tc
