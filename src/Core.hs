module Core where

import Data.List (dropWhile)
import Control.Monad (forM_,forM,zipWithM)
import Data.List (foldl,find,nub)

import Lexer
import Parser
import Types
import TransformationMonad
import Aux

storeDataInState :: P2Result -> TM ()
storeDataInState (P2Result pdecls) = do
  forM_ (filter isData pdecls)
    $ \(PDataType (L s (Upp name)) pconstrs) -> do
    let cconstrs
          = map
            (\(PConstr ltk pfts)
              -> CConstr ltk (map (toCFTy' . substIntFixed) pfts))
            pconstrs
        isRec = isRecursiveData name cconstrs
    addData (L s name, cconstrs, isRec, False)

isRecursiveData :: Name -> [CConstr] -> Bool
isRecursiveData name cconstrs = or $ map isRec cconstrs
  where isRec :: CConstr -> Bool
        isRec (CConstr _ cfts) = or $ map isSameType cfts
        isSameType :: CFType -> Bool
        isSameType t = case t of
          CTAExpr (L _ (Upp x))
            | x == name -> True
          _ -> False

substIntFixed :: PFType -> PFType
substIntFixed pft = case pft of
  PTApp   x    pfts -> PTApp x (map substIntFixed pfts)
  PTArrow pft1 pft2 -> PTArrow
                       (substIntFixed pft1)
                       (substIntFixed pft2)
  PTAExpr (L s (Upp "Int")) -> PTApp (L s (Upp "Int"))
                               [PTAExpr (noLocDec 32)]
  PTAExpr (L s (Upp "Fixed")) -> PTApp (L s (Upp "Fixed"))
                                 [PTAExpr (noLocDec 32)]
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
  destroy (zip3 pats (map makeExpr [0..]) [0..]) pgs
          
destroy :: [(Pat, PExpr,Int)] -> PGuards -> PGuards
destroy [] pgs = pgs
destroy ((pat,expr,i):pis) pgs = case pat of
    Pat (L _ (Low  x)) -> destroy pis (replaceBy x (expr,i) pgs)
    Pat (L _ (Upp  x)) -> destroy pis (df (x,expr) pgs)
    Pat (L _ (Bin  x)) -> error "bindechex" -- type?
    Pat (L _ (Hex  x)) -> error "bindechex" -- type?
    Pat (L _ (Dec  x)) -> error "bindechex" -- type?
    ConsPat (L _ (Upp x)) pats ->
      let argExprs = zip3 pats (map
                                (concatExpr x expr)
                                (zip pats [0..])) [0..]
          pgs'  = destroy argExprs pgs
          pgs'' = df (x,expr) pgs'
      in destroy pis pgs''
    _ -> error "destroypats"

concatExpr :: Name -> PExpr -> (Pat,Int) -> PExpr
concatExpr x pexpr (p,i)
  = PApp (noLocLow ("__get__" ++ x ++ "__" ++ show i)) [pexpr]

replaceBy :: Name -> (PExpr,Int) -> PGuards -> PGuards
replaceBy name (pexpr,i) pgs =
  let replace :: PExpr -> PExpr
      replace e = case e of
        PAExpr (L _ (Low x))
          | x == name -> pexpr
        PAExpr x -> PAExpr x
        PApp (L l (Low x)) exps
          | x == name
            -> PApp (L l (Low ("__i" ++ show i))) (map replace exps)
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
  forM_ dds $ \(L s name, constrs, isRec, _) -> do
    forM_ constrs $ \(CConstr (L _ (Upp datacons)) cfts) -> do
      let typeCons = CTAExpr (L s (Upp name))
          bool = CTAExpr (L s (Upp "Bool"))
      addCFuncType ("__is__" ++ datacons, [], [typeCons, bool])
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

fixedFunctions :: TCore -> TCore
fixedFunctions (TCore tcfs)
  = TCore $ map substFunctions tcfs

substFunctions :: TCFunc -> TCFunc
substFunctions (TCFunc name vars tcgs fty)
  = TCFunc name vars (substGuards tcgs) fty
  where substGuards gs = case gs of
          TCNoGuards e -> TCNoGuards $ substExpr e
          TCGuards ces -> TCGuards $ map (both substExpr) ces

isNumFunc f = elem f ["add","sub","mul"]

isFixed :: CFType -> Bool
isFixed ty = case ty of
  CTApp (L _ (Upp "Fixed")) _ -> True
  _ -> False

substExpr :: TCExpr -> TCExpr
substExpr tcexpr = case tcexpr of
  TCApp (L l (Low name)) args ty
    | isNumFunc name && isFixed ty
      -> TCApp (L l (Low (name ++ "F"))) args ty
  x -> x

typeCheck :: TMM ()
typeCheck = do
  mtcore <- typeCheck'
  cont1 mtcore $ \tcore -> do
    putTCore tcore
    ret ()

typeCheck' :: TMM TCore
typeCheck' = do
  Core fs <- getCore
  mtcs <- forM fs typeCheckEach
  cont_ mtcs $ \tcs -> do
    ret $ fixedFunctions $ TCore tcs

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
          mc' <- typeCheckExpr bool c
          me' <- typeCheckExpr ftype e
          cont2 mc' me' $ \c' e' -> ret (c',e')
        cont_ mces' $ ret . TCGuards

    typeCheckExpr :: [CFType] -> PExpr -> TMM TCExpr
    typeCheckExpr ft expr = typeCheckExpr' ft (expr,0)
      
    typeCheckExpr' :: [CFType] -> (PExpr,Int) -> TMM TCExpr
    typeCheckExpr' ft (expr,i) = case expr of
      PApp ltk args -> do
        mty <- getType ltk
        
        cont1 mty $ \(cs,_ty) -> do

          let ty = cTArrowInv' _ty
          putTypeCheckState (cs,ty)
          
          let argsI = zip args [0..]

          _ <- mapM (typeCheckExpr' ty) argsI
          margs' <- mapM (typeCheckExpr' ty) argsI

          cont_ margs' $ \args' -> do

            mty' <- getTypeCheckState

            cont1 mty' $ \(cs',ty') -> do

              popTypeCheckState

              mty'' <- getTypeCheckState

              case mty'' of

                Nothing -> do

                  mr <- match (last ft) (last ty')
                  cont1 mr $ \r -> do
                    ok <- matchConstraint r (nub (cs ++ cs'))
                    cont [ok] $ ret $ TCApp ltk args' (last ty')

                Just (cs'',ty'') -> do

                  mr <- match (ty'' !! i) (last ty')
                  cont1 mr $ \r -> do
                    ok <- matchConstraint r (nub (cs ++ cs' ++ cs''))
                    cont [ok] $ ret $ TCApp ltk args' (last ty')

      PAExpr ltk -> do

        mTy <- getType ltk

        cont1 mTy $ \(cs,ty) -> do
          
          mTy' <- getTypeCheckState
          
          case mTy' of
            
            Nothing -> do

              mTy'' <- match (last ft) (cTArrow ty)
              cont1 mTy'' $ \ty'' -> do
                ok <- matchConstraint ty'' cs
                cont [ok] $ ret $ TCAExpr (ltk, ty'')
                
            Just (cs',ty') -> do

              mTy'' <- match (ty' !! i) (cTArrow ty)
              cont1 mTy'' $ \ty'' -> do
                ok <- matchConstraint ty'' (nub (cs' ++ cs))
                cont [ok] $ ret $ TCAExpr (ltk, ty'')

    matchConstraint :: CFType -> [Constraint] -> TMM ()
    matchConstraint cft cs = do
      checkeds <- mapM check cs
      cont checkeds mok
      where check :: Constraint -> TMM ()
            check cons = case cons of
              ("NotRec",var) -> do
                isIt <- isRecursiveType cft
                case isIt of
                  False -> mok
                  True -> mok
              ("Num",var)
                | isNumType cft -> mok
                | otherwise -> mok
              _ -> mok

    isNumType :: CFType -> Bool
    isNumType cft = case cft of
      CTApp (L _ (Upp "Fixed")) _ -> True
      CTApp (L _ (Upp   "Int")) _ -> True
      --CTAExpr (L _ (Upp   "Int")) -> True
      _ -> False

    isRecursiveType :: CFType -> TM Bool
    isRecursiveType cft = case cft of
      CTApp (L _ (Upp name)) _ -> isTypeRecursive name
      CTAExpr (L _ (Upp name)) -> isTypeRecursive name
      x -> return False
      --error $ "isRecursiveType error " ++ show x
    
    cTArrow :: [CFType] -> CFType
    cTArrow  [] = error "cTArrow"
    cTArrow [x] = x
    cTArrow xs  = CTArrow (getLoc' (head xs)) xs

    cTArrowInv :: CFType -> [CFType]
    cTArrowInv (CTArrow _ as) = as
    cTArrowInv x = [x]

    cTArrowInvLast :: CFType -> CFType
    cTArrowInvLast = last . cTArrowInv

    cTArrowInvHead :: CFType -> CFType
    cTArrowInvHead (CTArrow _ as) = head as
    cTArrowInvHead c = c

    fromListTy c = case c of
      CTApp (L _ (Upp "List")) [x] -> x
      x -> x

    cTArrowInv' :: [CFType] -> [CFType]
    cTArrowInv' [CTArrow _ as] = as
    cTArrowInv' xs = xs

    arrowLast :: [CFType] -> Int -> CFType
    arrowLast xs i = case xs !! i of
      CTArrow _ as' -> last as'
      x -> x
    
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
      | ltk == L NoLoc (Upp "Cons") = do

          mm <- searchCFuncType (getName ltk) (fromListTy (cTArrowInvHead (head ftype)))
          cont1 mm $ \(_,constraints,cfts) -> do
            ret (constraints,cfts)
      | otherwise = do

          mm <- searchCFuncType (getName ltk) (cTArrowInvHead (head ftype))
          cont1 mm $ \(_,constraints,cfts) -> do
            ret (constraints,cfts)

    match x y = do
      m <- case (x,y) of
        (CTArrow _ _,CTArrow _ _) -> match' x y
        (CTArrow _ _,_) -> match' (cTArrowInvLast x) y
        (_,CTArrow _ _) -> match' x (cTArrowInvLast y)
        _ -> match' x y
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
      ret $ CTAExpr (noLocDec n)
    match' (CTAExpr (L _ (Dec n))) (CTAExpr (L _ (Low v))) = do
      modifyTypeCheckState (substDec v n)
      ret $ CTAExpr (noLocDec n)
    match' (CTAExpr (L _ (Low v))) (CTAExpr (L _ (Upp c))) = do
      modifyTypeCheckState (substCons v c)
      ret $ CTAExpr (noLocUpp c)
    match' (CTAExpr (L _ (Upp c))) (CTAExpr (L _ (Low v))) = do
      modifyTypeCheckState (substCons v c)
      ret $ CTAExpr (noLocUpp c)
    match' (CTAExpr (L _ (Low v))) ctapp@(CTApp _ _) = do
      modifyTypeCheckState (substCTApp v ctapp)
      ret ctapp
    match' ctapp@(CTApp _ _) (CTAExpr (L _ (Low v))) = do
      modifyTypeCheckState (substCTApp v ctapp)
      ret ctapp
    match' (CTAExpr (L _ (Low v1))) (CTAExpr (L _ (Low v2)))
      | v1 == v2 = ret $ CTAExpr (noLocLow v1)
      | otherwise = do
          modifyTypeCheckState (substVar v2 v1)
          ret $ CTAExpr (noLocLow v1)
    match' fa1 fa2
      | fa1 == fa2 = ret fa1
      | otherwise = typeMatchErr (getLoc' fa1)

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
