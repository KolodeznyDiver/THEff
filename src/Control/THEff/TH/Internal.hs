{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE  TemplateHaskell, ViewPatterns, RecordWildCards #-}
module Control.THEff.TH.Internal(mkEff) where

import Language.Haskell.TH
import Data.List
import Data.Either
import Data.Maybe

data DescrEff = DescrEff {  dModule :: String 
                         ,  dName :: String
                         ,  dEffModule :: String 
                         ,  dEffName :: String
                         ,  dType :: Maybe Type 
                         } deriving Show

splitLast:: Char -> String -> (String,String)
splitLast c s = case findIndices (c ==) s of
                    [] -> ("",s)
                    ixs -> splitAt (last ixs +1) s

getModuleAndName :: Name -> (String,String)
getModuleAndName = splitLast '.' . show
                    
parseCon :: Con -> Either String DescrEff
parseCon (NormalC n [(NotStrict, AppT t@(AppT (ConT e) _) _ )]) = 
        if not (null en) && last en == '\'' then
             Right $ DescrEff nm nn' em (init en) (if en == "Lift'" then Nothing else Just t)
        else Left  $ "Incorrect type name : " ++ en
    where (nm,nn) = getModuleAndName n
          (_,nn') = splitLast '_' nn
          (em,en) = getModuleAndName e 
parseCon _ = Left "Incorrect subtype"

mkN_E :: String -> String -> Name
mkN_E prfx eff = mkName $ concat [ prfx, "_", eff]

mkEffName' :: DescrEff -> Name  
mkEffName' DescrEff{..} = mkName $ concat [ dEffModule, dEffName, "'"]

mkPrimName :: String -> Name
mkPrimName = mkName . (++ "'")

mkTypePrim :: String -> [DescrEff] -> Q Dec
mkTypePrim newType lst = do
    m <- newName "m"
    e <- newName "e"
    let kinds = [KindedTV m (AppT (AppT ArrowT StarT) StarT), KindedTV e StarT]
    let n = mkPrimName newType
    let mkc = mkCon (VarT m) (VarT e) newType
    return $ 
        case lst of
            [d] -> NewtypeD [] n kinds (mkc d) []
            _ -> DataD [] n kinds (map mkc lst) []
            
        
mkCon :: Type -> Type -> String -> DescrEff -> Con
mkCon m e newType d@DescrEff{..} = 
    NormalC (mkN_E newType dName) [(NotStrict, AppT 
      ( case dType of
            (Just t) -> t
            _ -> AppT (ConT $ mkEffName' d) m
      ) e )]

mkTypeMain :: String ->  String -> Name -> Name -> Type -> Q Dec 
mkTypeMain thisMdl newType newTypeName effName argT = do
    m <- newName "m"
    a <- newName "a"
    let n = mkName newType
    let t' = mkPrimName $ thisMdl ++ newType
    let u = mkName $ "un" ++ newType
    return $
        NewtypeD [] n [KindedTV m (AppT (AppT ArrowT StarT) StarT),
                                    KindedTV a StarT]
                (RecC n 
                    [(u,NotStrict,
                        AppT (AppT (AppT (AppT 
                          (AppT (ConT effName) (VarT m))
                          (AppT (AppT (ConT newTypeName) (VarT m))
                             (VarT a))
                                          )
                                          (ConT t')
                                   )
                                   argT
                              )
                              (VarT a)
                      )]) []    

lookEff :: (String -> Q (Maybe Name)) -> String -> Q Name
lookEff f n = do
    m <- f n
    case m of
        (Just x) -> return x
        _ -> fail $ concat ["mkEff: ", n, " not found"] 
 
lookEffType :: String -> Q Name
lookEffType = lookEff lookupTypeName

lookEffValue :: String -> Q Name
lookEffValue = lookEff lookupValueName
 
lookEffFn :: String -> String -> Q Name 
lookEffFn mdl eff = lookEffValue $ concat [mdl, "eff", eff]
 
data InstnType = InstnAction | InstnOuter

mkInstn :: String -> String -> Name -> Name -> InstnType -> DescrEff -> Q Dec
mkInstn thisMdl newType newTypeName thisEffName it DescrEff{..} = do
    let thisEff = show thisEffName
    c <- lookEffType (if isJust dType then "EffClass" else "EffClassM") 
    m <- newName "m"
    a <- newName "a"
    x <- newName "x"
    let fullNewType = thisMdl ++ newType
    return $ InstanceD [] (AppT  
        (case dType of
            (Just (AppT effT argT)) -> AppT (AppT (ConT c) effT) argT
            _ -> AppT (ConT c) (VarT m)
        )
        (AppT (AppT (ConT newTypeName) (VarT m)) (VarT a))) 
        [FunD (mkName (if isJust dType then "toEff" else "toEffM")) 
            [Clause [VarP x] (NormalB (AppE (ConE newTypeName) 
                (case it of
                    InstnAction -> AppE (ConE (mkName $ concat [thisEff,"Action"])) 
                                    (VarE x)
                    InstnOuter  -> AppE (ConE (mkName $ concat [thisEff,"Outer"])) 
                                    (AppE (ConE (mkN_E fullNewType dName)) (VarE x))
                ))) []]]

is2ar :: Name -> Q Bool
is2ar t = do
    (TyConI d) <- reify t
    return (case d of
                (TySynD _ [_,_] _) -> True
                (NewtypeD [] _ [_,_] _ _) -> True
                (DataD _ _ [_,_] _ _) -> True
                _ -> False
            )
            
mkRunFun :: String -> String -> Name -> Name -> Type -> Name -> [DescrEff] -> DecsQ
mkRunFun thisMdl newType newTypeName thisEffName argT outerName ds = do
    mf <- lookEffValue ">>="
    eff <- lookEffType "Eff" 
    let (tm,tn) = getModuleAndName thisEffName
    runEffThisEffName <- lookEffValue $ concat [tm,"runEff",tn]
    ri <- reify runEffThisEffName
    let useArg = case ri of
                    (VarI _ (ForallT _ _ 
                        (AppT _ -- arg 1 
                        (AppT  _ -- arg 2
                        (AppT  _ -- arg 3
                        (AppT 
                        (AppT ArrowT _) -- arg 4  (if exist)
                        (AppT -- arg 5 
                            (AppT ArrowT (AppT (AppT (ConT _) (VarT _)) (VarT _))) 
                            (AppT (VarT _) _
                        ))))))) _ _ ) -> True
                    _ -> False
    resTName <- lookEffType $ concat [tm,tn,"ResT"] 
    m <- newName "m"
    a <- newName "a"
    b <- newName "b"
    h <- newName "h"
    twoArgResT <- is2ar resTName
    let fullNewType = thisMdl ++ newType
        fnName = mkName $ "run" ++ newType
        uTypeName = mkName $ concat [thisMdl,"un",newType]
        resT = AppT (ConT resTName) (VarT a)
        resT2 = if twoArgResT then AppT resT argT else resT
        t = AppT (AppT ArrowT (AppT (AppT (ConT eff) (AppT (AppT (ConT newTypeName) 
          (VarT m)) (VarT a))) (VarT a)))
          (case on of
            "Lift"  -> AppT (VarT m) resT2
            "NoEff" -> resT2
            _ -> (AppT (AppT (ConT eff) (AppT (AppT (ConT outerName) (VarT m)) (VarT b))) 
                   resT2)  )
    argsAndResT <- if useArg then do
                        argTName <- lookEffType $ concat [tm,tn,"ArgT"] 
                        twoArgT <- is2ar argTName
                        let a1T = AppT (ConT argTName) argT
                            aT = if twoArgT then AppT a1T (VarT a) else a1T
                        return $ AppT (AppT ArrowT aT) t 
                   else return t
    case ds of
        [] -> do
            let isLift = on == "Lift"
            g <- newName "g"
            e <- lookEffFn om on
            cx <- if isLift then do
                       mnd <- lookEffType "Monad"
                       return [AppT (ConT mnd) (VarT m)]
                  else return []
            let outer = if isLift then
                            (LamE [ConP (mkN_E fullNewType on) [VarP h]] (AppE (VarE mf) 
                                (AppE (VarE e) (VarE h))))
                        else VarE e
                cls = if useArg then [VarP b,VarP g] else [VarP g]
                runeff = AppE (AppE (AppE (VarE runEffThisEffName) outer) (ConE newTypeName))
                            (VarE uTypeName)
                mainBd = if useArg then AppE (AppE runeff (VarE b)) (VarE g)
                         else AppE runeff (VarE g)
            runOuterEffName <- lookEffValue $ concat [om,"run",on]
            return $ [SigD fnName (ForallT [PlainTV m,PlainTV a] cx argsAndResT),
              FunD fnName [Clause cls (NormalB (AppE (VarE runOuterEffName) mainBd)) []]]
        _ -> do
            let mkMatch DescrEff{..} = do
                    g <- newName "g"
                    e <- lookEffFn dEffModule dEffName
                    return $ Match (ConP (mkN_E fullNewType dName) [VarP g]) 
                        (NormalB (AppE (VarE e) (VarE g))) []
            ms <- mapM mkMatch ds
            return [SigD fnName (ForallT [PlainTV m,PlainTV a,PlainTV b] [] argsAndResT ),
                    ValD (VarP fnName) (NormalB (AppE (AppE (AppE (VarE runEffThisEffName) 
                    (LamE [VarP h] (AppE (VarE mf) (CaseE (VarE h) ms)))) (ConE newTypeName)) 
                    (VarE uTypeName))) []]
             
    where (om,on) = getModuleAndName outerName  
             

-- | TH function for building types and functions to ensure the functioning of 
--   the chain enclosed in each other's effects
mkEff :: String -- ^ The name of the new type - the element chain effects. 
                -- Based on this name mkEff will create new names with prefixes and suffixes.
      -> Name   -- ^ The type of effect. e.g. `State' or `Reader'. 
      -> Name   -- ^ The type used in the first argument runEEEE and / or in 
                -- the result of runEEEE. For example, for `State' effect,  of items  this type
                -- used in `get', `put', `modify'.
      -> Name   -- ^ The name of previous (outer) element chain effects.
      -> DecsQ
mkEff newType effName effArg outt = do
    loc <- location
    let thisMdl = loc_module loc ++ "."
        fullNewType = thisMdl ++ newType
        newTypeName = mkName fullNewType
        argT = ConT effArg
    if on `elem` ["Lift","NoEff"] 
        then do
            let isLift = on == "Lift"
                o = DescrEff "" on om on Nothing
            dPrim <- mkTypePrim newType (if isLift then [o] else [])
            dMain <- mkTypeMain thisMdl newType newTypeName effName argT
            dMainInst <- mkInstn thisMdl newType newTypeName effName 
                InstnAction $ mkDescrEff "" "" "" "" effName argT 
            dOuterInst <- mkInstn thisMdl newType newTypeName effName InstnOuter o
            dRun <- mkRunFun thisMdl newType newTypeName effName argT outt [] 
            let r = if isLift then dOuterInst:dRun else dRun
            return $ dPrim:dMain:dMainInst:r
        else do
            oi <- reify outt
            case oi of
                TyConI (NewtypeD [] _ [_,_] (RecC (ocmp -> True)
                    [(_,NotStrict,
                        AppT (AppT (AppT (AppT 
                          (AppT (ConT te) _ ) _
                                          )
                                          (ConT tep)
                                   )
                                   argt
                              )
                              _
                      )]) []) -> do
                        opi <- reify tep
                        let ~(TyConI dec) = opi
                        let lst = case dec of
                                    (NewtypeD [] _ [_,_] c  []) -> [parseCon c]
                                    (DataD    [] _ [_,_] cl []) -> map parseCon cl
                                    _ -> [Left $ "Incorrect type " ++ show tep]
                                  
                        case lefts lst of
                            (e:_) -> fail $ "mkEff: " ++ e
                            [] -> let l = rights lst
                                      (tm,tn) = getModuleAndName te
                                      o = mkDescrEff om on tm tn te argt
                                      l1 = o:l
                                  in do
                                    dPrim <- mkTypePrim newType l1
                                    dMain <- mkTypeMain thisMdl newType newTypeName effName argT
                                    dMainInst <- mkInstn thisMdl newType newTypeName effName 
                                        InstnAction $ mkDescrEff "" "" "" "" effName argT 
                                    dOuterInsts <- mapM 
                                        (mkInstn thisMdl newType newTypeName effName InstnOuter) 
                                        l1
                                    dRun <- mkRunFun thisMdl newType newTypeName effName 
                                        argT outt l1 
                                    return $ dPrim:dMain:dMainInst:(dOuterInsts ++ dRun) 
                _ -> fail "mkEff: Expected newtype in forth argument or ''Lift or ''NoEff"
                
    where (om,on) = getModuleAndName outt
          soutt = show outt
          ocmp = (soutt ==) . show
          mkDescrEff oM oN tM tN tE aT = DescrEff oM oN tM tN (Just $ AppT (ConT 
                                            (mkName $ show tE ++ "'")) aT)

