module Core where

import Data.List (dropWhile)
import Control.Monad (forM_,forM,zipWithM)
import Data.List (foldl,find)

import Lexer2
import Parser2
import Types
import TransformationMonad
import Aux

storeDataInState :: P2Result -> TM ()
storeDataInState (P2Result pdecls) = do
  forM_ (filter isData pdecls)
    $ \(PDataType (L s (Upp name)) pconstrs) -> do
    let x = map
            (\(PConstr ltk pfts)
              -> CConstr ltk (map (toCFTy' . substIntFixed) pfts))
            pconstrs
    addData (L s name, x)

substIntFixed :: PFType -> PFType
substIntFixed pft = case pft of
  PTApp   x    pfts -> PTApp x (map substIntFixed pfts)
  PTArrow pft1 pft2 -> PTArrow
                       (substIntFixed pft1)
                       (substIntFixed pft2)
  PTAExpr (L s (Upp "Int")) -> PTApp (L s (Upp "Int"))
                               [PTAExpr (noLocDec 32)]
  PTAExpr (L s (Upp "Fixed")) -> PTApp (L s (Upp "Fixed"))
                                 [PTAExpr (noLocDec 32)
                                 ,PTAExpr (noLocDec 32)]
  y -> y
  

isData (PDataType _ _) = True
isData  _              = False

gatherFunctions :: P2Result -> TMM [(Name, PFType, [Name], PGuards)]
gatherFunctions (P2Result pds) = do
  let pds' = filter (not . isData) pds
  fs <- glueFunctions pds'
  ret $ map patternsToGuards fs
  where patternsToGuards (n,ft,fd)
          = (n,substIntFixed ft,varsFromPats fd,concatGuards (map destroyPats fd))

varsFromPats :: [([Pat], a)] -> [Name]
varsFromPats xs = map toName [0..(length pats - 1)]
  where (pats,_) = head xs
        toName i = "__i" ++ show i        
          
glueFunctions :: [PDecl] -> TM [(Name, PFType, [([Pat],PGuards)])]
glueFunctions pds = case pds of
  PFType (L _ (Low name)) ft : ps -> do
    let r   = takeFunctionDef name ps
        ps' = dropFunctionDef name ps
    rest <- glueFunctions ps'
    return $ (name, ft, r) : rest
  [] -> return []
  x -> error (show x)

dropFunctionDef :: Name -> [PDecl] -> [PDecl]
dropFunctionDef name = dropWhile (isSameFunc name)

isSameFunc :: Name -> PDecl -> Bool
isSameFunc name p = case p of
  PFunc (L _ (Low n)) pats pgs
    | n == name -> True
  _ -> False
  
takeFunctionDef :: Name -> [PDecl] -> [([Pat], PGuards)]
takeFunctionDef name = map patsNguards . takeWhile (isSameFunc name)
  where patsNguards (PFunc (L _ (Low n)) pats pgs) = (pats, pgs)
        patsNguards _ = error "patsnguards"

destroyPats :: ([Pat],PGuards) -> PGuards
destroyPats (pats, pgs) =
  destroy (zip pats (map makeExpr [0..])) pgs
          
destroy :: [(Pat, PExpr)] -> PGuards -> PGuards
destroy [] pgs = pgs
destroy ((pat,expr):pis) pgs = case pat of
    Pat (L _ (Low  x)) -> destroy pis (replaceBy x expr pgs)
    Pat (L _ (Upp  x)) -> destroy pis (df (x,expr) pgs)
    Pat (L _ (Bin  x)) -> error "bindechex" -- type?
    Pat (L _ (Hex  x)) -> error "bindechex" -- type?
    Pat (L _ (Dec  x)) -> error "bindechex" -- type?
    ConsPat (L _ (Upp x)) pats ->
      let argExprs = zip pats (map
                                (concatExpr x expr)
                                (zip pats [0..]))
          pgs'  = destroy argExprs pgs
          pgs'' = df (x,expr) pgs'
      in destroy pis pgs''
    _ -> error "destroypats"

concatExpr :: Name -> PExpr -> (Pat,Int) -> PExpr
concatExpr x pexpr (p,i)
  = PApp (noLocLow ("__get__" ++ x ++ "__" ++ show i)) [pexpr]

replaceBy :: Name -> PExpr -> PGuards -> PGuards
replaceBy name pexpr pgs =
  let replace :: PExpr -> PExpr
      replace e = case e of
        PAExpr (L _ (Low x))
          | x == name -> pexpr
        PAExpr x -> PAExpr x
        PApp ltn exps -> PApp ltn (map replace exps)
  in case pgs of
    PNoGuards e -> PNoGuards $ replace e
    PGuards ces -> PGuards $ map (\(c,e)
                                   -> (replace c,replace e)) ces

makeExpr i = PAExpr $ noLocLow $ "__i" ++ show i

isPAExpr (PAExpr _) = True
isPAExpr  _ = False

noLoc = L NoLoc
noLocLow = L NoLoc . Low
noLocUpp = L NoLoc . Upp
noLocDec = L NoLoc . Dec

df :: (Name, PExpr) -> PGuards -> PGuards
df (nameCons,pexpr) pgs = case pgs of
  PNoGuards expr -> PGuards [(cond,expr)]
  PGuards ces -> PGuards $ for ces $ \(c,e) ->
    let c' = andExpr cond c
    in (c',e)
  where cond = PApp (noLocLow ("__is__" ++ nameCons)) [pexpr]

andExpr e1 e2 = PApp (noLocLow "and") [e1,e2]

trueCond = PAExpr (noLocLow "otherwise")

concatGuards :: [PGuards] -> PGuards
concatGuards pgs = foldl1 f pgs
  where f :: PGuards -> PGuards -> PGuards
        f (PNoGuards _) (PNoGuards _)
          = error "concatguards"
        f (PNoGuards p1) (PGuards ps2)
          = error "concatguards1"
        f (PGuards ps1) (PNoGuards p2)
          = PGuards (ps1 ++ [(trueCond,p2)])
        f (PGuards ps1) (PGuards ps2)
          = PGuards (ps1 ++ ps2)

toCore :: [(Name, PFType, [Name], PGuards)] -> Core
toCore dcs = Core (map toCore' dcs)
  where toCore' :: (Name, PFType, [Name], PGuards) -> CFunc
        toCore' (n,pfty,vars,pgs)
          = CFunc (noLocLow n) (map noLocLow vars) (toCGS pgs) (toCFTy pfty)
          
        toCGS :: PGuards -> CGuards
        toCGS (PNoGuards x) = CNoGuards x
        toCGS (PGuards xys) = CGuards xys

storeCoreInState :: P2Result -> TMM ()
storeCoreInState expr = do
  mk <- gatherFunctions expr
  cont1 mk $ \k -> do
    setCore $ toCore k
    ret ()
  
toCFTy :: PFType -> [CFType]
toCFTy (PTApp ltok pfts)
  = let cfts = map toCFTy pfts
    in [CTApp ltok (concat cfts)]
toCFTy (PTAExpr x)
  = [CTAExpr x]
toCFTy (PTArrow pft1 pft2)
  = let cfts1 = toCFTy' pft1
        cfts2 = toCFTy pft2
    in [cfts1] ++ cfts2
  
toCFTy' :: PFType -> CFType
toCFTy' (PTApp ltok pfts)
  = let cfts = map toCFTy' pfts
    in CTApp ltok cfts
toCFTy' (PTAExpr x)
  = CTAExpr x
toCFTy' (PTArrow pft1 pft2)
  = let cfts1 = toCFTy' pft1
        cfts2 = toCFTy pft2
    in CTArrow NoLoc ([cfts1] ++ cfts2)

-------- type checking / type inference
  
putDataDefsInState :: TM ()
putDataDefsInState = do
  dds <- getDataDecls
  forM_ dds $ \(L s name, constrs) -> do
    forM_ constrs $ \(CConstr (L _ (Upp datacons)) cfts) -> do
      let typeCons = CTAExpr (L s (Upp name))
          bool = CTAExpr (L s (Upp "Bool"))
      addCFuncType
        ("__is__" ++ datacons, [], [typeCons, bool])
      addCFuncType (datacons, [], cfts ++ [typeCons])
      forM_ (zip cfts [0..]) $ \(cft,i) -> do
        addCFuncType ("__get__" ++ datacons ++ "__" ++ show i
                     , [], [typeCons, cft])

putFunctionTypesInState :: TM ()
putFunctionTypesInState = do
  Core cfs <- getCore
  forM_ cfs $ \(CFunc (L _ (Low name)) _ _ cts) -> do
    addCFuncType (name, [], cts)

funcName (CFunc (L _ (Low name)) _ _ _) = name

typeCheck :: TMM TCore
typeCheck = do
  Core fs <- getCore
  mtcs <- forM fs typeCheckEach
  cont_ mtcs $ \tcs -> do
    ret $ TCore tcs

getName :: LToken -> Name
getName = getN . getVal
  where getN (Low n) = n
        getN (Upp n) = n
        getN _ = error "getn"

typeCheckEach :: CFunc -> TMM TCFunc
typeCheckEach (CFunc lname vars cgs ftype) = do

  mtcgs <- typeCheckGuards cgs
  cont1 mtcgs $ \tcgs -> 
    ret $ TCFunc lname varsWithTypes tcgs retType
  
  where
    varsWithTypes = zip vars ftype
    retType = last ftype
    
    typeCheckGuards :: CGuards -> TMM TCGuards
    typeCheckGuards gs = case gs of
      CNoGuards e -> (TCNoGuards <$>) <$> typeCheckExpr ftype e
      CGuards ces -> do
        let bool = [CTAExpr (noLocUpp "Bool")]
        mces' <- forM ces $ \(c,e) -> do
          debug "************"
          debug $ "COND: " ++ show c
          debug "************"
          mc' <- typeCheckExpr bool c
          debug "************"
          debug $ "EXPR: " ++ show e
          debug "************"
          me' <- typeCheckExpr ftype e
          debug "&&&&&&&&&&"
          debugs "RESULTS"
          debugs (mc', me')
          debug "&&&&&&&&&&"
          cont2 mc' me' $ \c' e' -> ret (c',e')
        cont_ mces' $ ret . TCGuards

    typeCheckExpr :: [CFType] -> PExpr -> TMM TCExpr
    typeCheckExpr ft expr = typeCheckExpr' ft (expr,0)
      
    typeCheckExpr' :: [CFType] -> (PExpr,Int) -> TMM TCExpr
    typeCheckExpr' ft (expr,i)  = case expr of
      PApp ltk args -> do

        debug $ "INSIDE " ++ show (expr,i)
        debug $ "GETTYPE " ++ show (ltk,args)
        mty <- getType ltk
        
        cont1 mty $ \(_,ty) -> do
          
          putTypeCheckState ty
          
          let argsI = zip args [0..]

          --debug $ "@@@ENTERING FUNCTION " ++ getName ltk
          debug $ "ENTERINGARGS: " ++ show argsI
          margs' <- mapM (typeCheckExpr' ty) argsI
          --debug $ "@@@GETTING OUT FUNCTION " ++ getName ltk

          debug "A"
          cont_ margs' $ \args' -> do

            mty' <- getTypeCheckState

            debug "B"
            cont1 mty' $ \ty' -> do

              popTypeCheckState

              mty'' <- getTypeCheckState

              case mty'' of

                Nothing -> do

                  debug $ "HERE1: " ++ show ft
                  match (last ft) (last ty')
                  ret $ TCApp ltk args' (last ty')

                Just ty'' -> do

                  debug $ "HERE2"
                  match (ty'' !! i) (last ty')
                  ret $ TCApp ltk args' (last ty')

      PAExpr ltk -> do

        debug $ "INSIDE " ++ show (expr,i)
        debug $ "GETTYPE expr " ++ show ltk
        mTy <- getType ltk

        debug "C"
        cont1 mTy $ \(_,ty) -> do
          
          mTy' <- getTypeCheckState
          
          case mTy' of
            
            Nothing -> do

              debug $ "HERE3: " ++ show (ft,i)
              mTy'' <- match (last ft) (cTArrow ty)
              debug "D"
              cont1 mTy'' $ \ty'' -> do
                ret $ TCAExpr (ltk, ty'')
                
            Just ty' -> do

              debug "HERE4"
              mTy'' <- match (ty' !! i) (cTArrow ty)
              debug "E"
              cont1 mTy'' $ \ty'' -> do
                ret $ TCAExpr (ltk, ty'')

    cTArrow :: [CFType] -> CFType
    cTArrow [] = error "cTArrow"
    cTArrow [x] = x
    cTArrow xs  = CTArrow (getLoc' (head xs)) xs

    cTArrowInv :: CFType -> [CFType]
    cTArrowInv (CTArrow _ as) = as
    cTArrowInv x = [x]
    
    isInput ltk = elem ltk vars
    tyvar = CTAExpr . L NoLoc . Low

    getType x = do
      hg <- getType' x
      return hg
      
    getType' :: LToken -> TMM ([Constraint],[CFType])
    getType' (L _ (Bin b)) = ret ([("Num","a")],[tyvar "a"])
    getType' (L _ (Hex h)) = ret ([("Num","a")],[tyvar "a"])
    getType' (L _ (Dec d)) = ret ([("Num","a")],[tyvar "a"])
    getType' ltk
      | isInput ltk = do
          let mty = snd <$> find ((==ltk) . fst) varsWithTypes
          cont1 mty $ \ty ->
            ret ([], [ty])
      | otherwise = do
          mm <- searchCFuncType (getName ltk)
          cont1 mm $ \(_,constraints,cfts) -> do
            ret (constraints,cfts)

    match x y = do
      debug $ "MATCH OPEN " ++ show (x , y)
      m <- match' x y
      debug $ "MATCH CLOSE " ++ show m
      return m
      
    match' :: CFType -> CFType -> TMM CFType
    match' (CTArrow l vs1) (CTArrow _ vs2) = do
      mArgs' <- zipWithM match' vs1 vs2
      x <- cont_ mArgs' $ \args' ->
        ret $ CTArrow l args' -- ???
      return x
    match' (CTApp f args) (CTApp f' args')
      | f == f' = do
          mArgs' <- zipWithM match' args args'
          x <- cont_ mArgs' $ \args' ->
            ret $ CTApp f args'
          return x
      | otherwise = typeMatchErr (getLoc f)
    match' (CTAExpr (L _ (Low v))) (CTAExpr (L _ (Dec n))) = do
      modifyTypeCheckState (substDec v n)
      debug $ "***SUBST " ++ v ++ " BY " ++ show n
      debugShowTypeCheckState
      ret $ CTAExpr (noLocDec n)
    match' (CTAExpr (L _ (Dec n))) (CTAExpr (L _ (Low v))) = do
      modifyTypeCheckState (substDec v n)
      debug $ "***SUBST " ++ v ++ " BY " ++ show n
      debugShowTypeCheckState
      ret $ CTAExpr (noLocDec n)
    match' (CTAExpr (L _ (Low v))) (CTAExpr (L _ (Upp c))) = do
      modifyTypeCheckState (substCons v c)
      debug $ "***SUBST " ++ v ++ " BY " ++ c
      debugShowTypeCheckState
      ret $ CTAExpr (noLocUpp c)
    match' (CTAExpr (L _ (Upp c))) (CTAExpr (L _ (Low v))) = do
      modifyTypeCheckState (substCons v c)
      debug $ "***SUBST " ++ v ++ " BY " ++ c
      debugShowTypeCheckState
      ret $ CTAExpr (noLocUpp c)
    match' (CTAExpr (L _ (Low v))) ctapp@(CTApp _ _) = do
      modifyTypeCheckState (substCTApp v ctapp)
      debug $ "***SUBST " ++ v ++ " BY " ++ show ctapp
      debugShowTypeCheckState
      ret ctapp
    match' ctapp@(CTApp _ _) (CTAExpr (L _ (Low v))) = do
      modifyTypeCheckState (substCTApp v ctapp)
      debug $ "***SUBST " ++ v ++ " BY " ++ show ctapp
      debugShowTypeCheckState
      ret ctapp
    match' (CTAExpr (L _ (Low v1))) (CTAExpr (L _ (Low v2)))
      | v1 == v2 = ret $ CTAExpr (noLocLow v1)
      | otherwise = do
          modifyTypeCheckState (substVar v2 v1)
          debug $ "SUBST " ++ v2 ++ " BY " ++ v1
          debugShowTypeCheckState
          ret $ CTAExpr (noLocLow v1)
    match' fa1 fa2
      | fa1 == fa2 = ret fa1
      | otherwise = typeMatchErr (getLoc' fa1)

    debugShowTypeCheckState = do
      tstate <- getIt
      debug $ "IT :" ++ show tstate
      
    getLoc' :: CFType -> SrcLoc
    getLoc' (CTApp   (L l _) _) = l
    getLoc' (CTArrow  l      _) = l
    getLoc' (CTAExpr (L l _)  ) = l
    
    typeMatchErr :: SrcLoc -> TMM a
    typeMatchErr l = do
      throw (TErr
             CantMatchTypes
             (Just ("In function " ++ getName lname))
             "Can't match types"
             l)
      noRet

    substDec v n = cTArrowInv . substDec' v n . cTArrow
    
    substDec' :: Name -> Int -> CFType -> CFType
    substDec' v n = go
      where
        go ft = case ft of
          CTArrow l fts -> CTArrow l (map go fts)
          CTApp f fts -> CTApp f (map go fts)
          CTAExpr (L _ (Low v')) -> if v' == v
                                    then CTAExpr (noLocDec  n)
                                    else CTAExpr (noLocLow v')
          x -> x

    substCons v n = cTArrowInv . substCons' v n . cTArrow
    
    substCons' :: Name -> Name -> CFType -> CFType
    substCons' v c = go
      where
        go ft = case ft of
          CTArrow l fts -> CTArrow l (map go fts)
          CTApp f fts   -> CTApp f (map go fts)
          CTAExpr (L _ (Low v')) -> if v' == v
                                    then CTAExpr (noLocUpp  c)
                                    else CTAExpr (noLocLow v')
          x -> x

    substVar v n = cTArrowInv . substVar' v n . cTArrow
    
    substVar' :: Name -> Name -> CFType -> CFType
    substVar' v1 v2 = go
      where
        go ft = case ft of
          CTArrow l fts -> CTArrow l (map go fts)
          CTApp f fts -> CTApp f (map go fts)
          CTAExpr (L _ (Low v))
            | v == v1   -> CTAExpr (noLocLow v2)
            | otherwise -> CTAExpr (noLocLow  v)
          x -> x

    substCTApp v n = cTArrowInv . substCTApp' v n . cTArrow
    
    substCTApp' :: Name -> CFType -> CFType -> CFType
    substCTApp' v1 ctapp = go
      where
        go ft = case ft of
          CTArrow l fts -> CTArrow l (map go fts)
          CTApp f fts   -> CTApp f (map go fts)
          CTAExpr (L _ (Low v))
            | v == v1   -> ctapp
            | otherwise -> CTAExpr (noLocLow  v)
          x -> x
