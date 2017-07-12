module Function where

--------------------- External Imports

import Prelude hiding (log)
import Control.Monad (forM, forM_)
import Control.Monad.Trans (lift)
import Data.List (elemIndex, intercalate)

--------------------- Internal Imports

import Lexer
import Types
import TransformationMonad
import Aux
  
--------------------- Transforming PResult to TFuncs and adding them to state

getParsedFunctions_TransformToF_AddToState :: TM ()
getParsedFunctions_TransformToF_AddToState = do
  newStage TInterpretationStage
  TCore fs <- getTCore
  mapM_ fromParsedFunctionToF_AddToState fs

fromParsedFunctionToF_AddToState :: TCFunc -> TMM ()
fromParsedFunctionToF_AddToState (TCFunc name varsNtypes guards typeExpr) = do
  
  let fname  = fmap fromLowOrUpp name
      srcLoc = getLoc name
      vars   = map fst varsNtypes
      types  = map snd varsNtypes

  mfts <- mapM fromParsedTypeExprToFunctionType types

  cont mfts $ do

    let fts = map just mfts
        vars' = map toFVar vars
        args = zip vars' fts

    case guards of

      TCNoGuards body -> do
        
        mFExpr <- getFExpr body
  
        cont1 mFExpr $ \fExpr -> do
      
          mft <- fromParsedTypeExprToFunctionType typeExpr
      
          cont1 mft $ \ft -> do
        
            addFunc (funcName, srcLoc, F args (NoFGuards fExpr) ft, length args, (classifyRecursion funcName (NoFGuards fExpr), classifyTypes fts ft, isConsExpr (NoFGuards fExpr)), isHighOrder fts)
            ret ()

      TCGuards guards -> do

        mfguards <- forM guards $ \(condExpr,expr) -> do

          mc <- getFExpr condExpr
          me <- getFExpr expr

          cont2 mc me $ \c e -> ret (c,e)

        cont mfguards $ do

          let fguards = map just mfguards

          mft <- fromParsedTypeExprToFunctionType typeExpr
      
          cont1 mft $ \ft -> do
        
            addFunc (funcName, srcLoc, F args (FGuards fguards) ft, length args, (classifyRecursion funcName (FGuards fguards), classifyTypes fts ft, isConsExpr (FGuards fguards)), isHighOrder fts)
            ret ()
    
  where

    funcName = fromLowOrUpp $ getVal name

    fromLowOrUpp (Upp n) = n
    fromLowOrUpp (Low n) = n

    makeFunctionId :: Name -> [FExpr] -> FType -> TM Id
    makeFunctionId name fexs ft = do
      let ftypes = map getTypeFromFExpr fexs ++ [ft]
      id <- getFunctionId name ftypes
      debug $ "Name: " ++ name ++ " [" ++ show id ++ "]"
      addFunctionId (name, id, ftypes)
      return id

    makeFunctionId' :: Name -> FType -> TM Id
    makeFunctionId' name ft = do
      id <- getFunctionId name [ft]
      debug $ "Name: " ++ name ++ " [" ++ show id ++ "]"
      addFunctionId (name, id, [ft])
      return id
    
    -- odot transforms pexpr to fexpr
    -- naming: fromParsedExpressionToFunctionExpression
    getFExpr :: TCExpr -> TMM FExpr
    getFExpr expr = case expr of
      
      TCApp (L src (Low s)) es ty -> do
        mes' <- mapM getFExpr es
        cont mes' $ do
          let es' = map just mes'
          mft <- fromParsedTypeExprToFunctionType ty
          cont1 mft $ \ft -> do
            id <- makeFunctionId s es' ft
            ret $ FApp (L src s, id) es' ft
        
      TCAExpr (L s (Low v), t)
        | v == "_'_" -> do
            mft <- fromParsedTypeExprToFunctionType t
            cont1 mft $ \ft -> do
              id <- makeFunctionId' v ft
              ret $ FAExpr (FCons FForeverWait, id, ft)
        | otherwise -> do
            mft <- fromParsedTypeExprToFunctionType t
            cont1 mft $ \ft -> do
              id <- makeFunctionId' v ft
              ret $ FAExpr (FVar (L s v), id, ft)
              
      TCApp (L src (Upp s)) es ty -> do
        mes' <- mapM getFExpr es
        cont mes' $ do
          let es' = map just mes'
          mft <- fromParsedTypeExprToFunctionType ty
          cont1 mft $ \ft -> do
            id <- makeFunctionId s es' ft
            ret $ FApp (L src s, id) es' ft
      TCAExpr (L s (Upp v), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft -> do
          id <- makeFunctionId' v ft
          ret $ FAExpr (FVar (L s v), id, ft)
              
      TCAExpr (L s (Bin i), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft -> do
          id <- makeFunctionId' ("const_bin_" ++ i) ft
          ret $ FAExpr (FCons(FBin (L s i)), id, ft)
      TCAExpr (L s (Hex i), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft -> do
          id <- makeFunctionId' ("const_hex_" ++ i) ft
          ret $ FAExpr (FCons(FHex (L s i)), id, ft)
      TCAExpr (L s (Dec i), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft -> do
          id <- makeFunctionId' ("const_dec_" ++ show i) ft
          ret $ FAExpr (FCons(FDec (L s i)), id, ft)

      x -> throw (TErr
                  ExpressionConstructionErr
                  (Just ("In function " ++ funcName))
                  ("Expression construction error: " ++ show x)
                  NoLoc) >> noRet

fromParsedTypeExprToFunctionType :: CFType -> TMM FType
fromParsedTypeExprToFunctionType cft = case cft of
  CTArrow s _ -> ret $ Function s
  CTApp (L s (Upp "Stream")) xs -> do
    mfts <- mapM fromParsedTypeExprToFunctionType xs
    cont_ mfts $ \fts -> do
      ret $ Stream $ head fts
  CTApp (L s (Upp "Vec")) [CTAExpr (L _ (Dec n))]
    -> ret $ BitVec s n
  CTAExpr (L s (Upp "Bit")) -> ret $ Bit s
  CTAExpr (L s (Upp "Function")) -> ret $ Function s
  CTApp (L s (Upp "Nat")) [CTAExpr (L _ (Dec n))]
    -> ret $ Nat s n
  x -> throw (TErr
              TypeNotPermitted
              Nothing
              ("Type '" ++ show x ++ "' not permitted.")
              NoLoc) >> noRet

--------------------- Checking for errors

checkForArityErrs :: TM ()
checkForArityErrs = do
  log "Checking for declarations and arity errors"
  fs <- getFunctions
  mapM_ checkFunc fs
  where
    ok = return ()
    
    checkFunc :: TFunc -> TM ()
    checkFunc (_, _, SpecialF, _,_,_) = ok
    checkFunc (name, _loc, F vars fg _, _,_,_) = case fg of

      NoFGuards body -> check body
      FGuards guards ->
        forM_ guards $ \(c,e) -> (check c >> check e)
        
      where
        check :: FExpr -> TM ()
        check (FAExpr _) = ok
        check (FApp (L _ "consR",_) _ _) = ok
        check (FApp (lname,_) fexprs _) = do
          let arity = length fexprs
          mf <- searchFunction (getVal lname)
          case mf of
            Nothing
              | elem lname (map fst vars) -> ok
              | otherwise -> throw (TErr
                                    FunctionNotDeclared
                                    (Just ("In function " ++ name))
                                    ("Function " ++ (getVal lname) ++ " is not declared.")
                                    (getLoc lname))
            Just (_,_,_,arity',_,_)
              | arity == arity' -> ok
              | otherwise -> throw (TErr
                                    ArityMismatch
                                    (Just ("In function " ++ name))
                                    ("Arity mismatch applying "
                                     ++ (getVal lname)
                                     ++ " ("
                                     ++ show arity'
                                     ++ " needed, "
                                     ++ show arity
                                     ++ " given)")
                                    (getLoc lname))
          mapM_ check fexprs

applyHighOrder :: TM ()
applyHighOrder = do
  fs <- getFunctions
  mapM_ high fs
  where
    high :: TFunc -> TMM ()
    high (_, _, SpecialF, _,_,_) = mok
    high (_,_,_,_,_,_:_) = mok
    high (name,loc,F vars fg _,_,_,[]) = case fg of
      NoFGuards body -> do
        body' <- change body
        changeFunction name (NoFGuards body')
      FGuards guards -> do
        guards' <- forM guards $ \(c,e) -> do
          c' <- change c
          e' <- change e
          return (c',e')
        changeFunction name (FGuards guards')
      where
        change :: FExpr -> TM FExpr
        change id@(FAExpr _) = return id
        change id@(FApp (L s fname,i) fexprs ft)
          | fname == name = do
              fexprs' <- mapM change fexprs
              return (FApp (L s fname,i) fexprs' ft)
          | otherwise = do
              isIt <- isFunctionHighOrder fname
              case isIt of
                True  -> do
                  as <- highOrderArgs fname
                  let namesNindexes = zip (map toName (indexes as fexprs)) as
                      fnameNew = fname ++ restOfName namesNindexes
                  addNewFunction fname (map toName (indexes as fexprs)) as
                  fexprs' <- mapM change fexprs
                  return $ FApp (L s fnameNew,i) (takeOut as fexprs') ft
                False -> do
                  fexprs' <- mapM change fexprs
                  return (FApp (L s fname,i) fexprs' ft)

addNewFunction :: Name -> [Name] -> [Int] -> TMM ()
addNewFunction fname substs as = do
  let namesNinds = zip substs as
      newFname = fname ++ restOfName namesNinds
  mf <- searchFunction fname
  cont1 mf $ \(_,loc,F vars fgs ft, a, fc,_) -> do
    let substInps = map (getVal . fst) $ indexes as vars
    fgs' <- newBody (fname, newFname) substInps substs as fgs
    addFunc (newFname, loc, F (takeOut as vars) fgs' ft, a, fc, [])
    removeFunction fname
    ret ()

newBody :: (Name,Name) -> [Name] -> [Name] -> [Int] -> FGuards -> TM FGuards
newBody (fname,newFname) sInps substs as fg = case fg of
  NoFGuards e -> do
    e' <- newExpr e
    return $ NoFGuards e'
  FGuards    fgs -> do
    fgs' <- forM fgs $ \(c,e) -> do
      c' <- newExpr c
      e' <- newExpr e
      return (c',e')
    return $ FGuards fgs'
  where
    newExpr :: FExpr -> TM FExpr
    newExpr fex = case fex of
      FApp (L s n,id) fexs ft -> do
        isIt <- isFunctionHighOrder n
        case isIt of
          True
            | n == fname -> do
                debug $ "hey " ++ n
                fexs' <- mapM newExpr fexs
                return $ FApp (L s newFname,id) (takeOut as fexs') ft
            | otherwise -> do
                debug $ "HOHGGH " ++ n
                as' <- highOrderArgs n
                fexs' <- mapM newExpr fexs
                let namesNindexes = zip (map toName (indexes as' fexs')) as'
                    nnew = n ++ restOfName namesNindexes
                addNewFunction n (map toName (indexes as' fexs')) as'
                return $ FApp (L s nnew,id) (takeOut as' fexs') ft
          False
            | n == fname -> do
                debug $ "hey " ++ n
                fexs' <- mapM newExpr fexs
                return $ FApp (L s newFname,id) (takeOut as fexs') ft
            | elem n sInps -> do
                debug n
                debug $ sub n
                fexs' <- mapM newExpr fexs
                return $ FApp (L s (sub n),id) fexs' ft
            | otherwise    -> do
                debug $ "nope " ++ n
                fexs' <- mapM newExpr fexs
                return $ FApp (L s n,id) fexs' ft
      FAExpr (FVar (L s v), id, ft)
        | elem v sInps -> return $ FAExpr (FVar (L s (sub v)), id, ft)
      x -> return x

    sub :: Name -> Name
    sub x = case elemIndex x sInps of
      Nothing -> error "sub"
      Just i  -> substs !! i

restOfName :: [(Name,Int)] -> String
restOfName = concat . map (\(n,i) -> "__" ++ show i ++ n)

takeOut :: [Int] -> [a] -> [a]
takeOut inds = map just . filter isJust . mapIndex f
  where f i x
          | elem i inds = Nothing
          | otherwise   = Just x
          
toName :: FExpr -> Name
toName (FAExpr (FVar (L _ x), _, Function _)) = x
toName x = error $ "toname " ++ show x

--------------------- Classifying Function

isHighOrder :: [FType] -> [Int]
isHighOrder = map just . filter isJust . mapIndex isFunctionType
  where isFunctionType i (Function _) = Just i
        isFunctionType i  _           = Nothing

isConsExpr :: FGuards -> IsConsExpr
isConsExpr gs = case gs of
  NoFGuards    fExpr -> isConsExpr' fExpr
  FGuards condNexprs -> or (map (isConsExpr' . fst) condNexprs)
  where isConsExpr' fex = case fex of
          FApp (L _ "cons", _) _ _ -> True
          _ -> False
  
classifyTypes :: [FType] -> FType -> TypeClassification
classifyTypes argTypes returnType =
  case (or (map isStream argTypes), isStream returnType) of
    (True , True) -> OutputInputRecursive
    (False, True) -> OutputRecursive
    (True ,False) -> InputRecursive
    (False,False) -> NoRecursiveTypes
  where isStream (Stream _) = True
        isStream _          = False

classifyRecursion :: Name -> FGuards -> RecursionClassification
classifyRecursion name guards = case guards of
  FGuards condNexprs -> decideAcrossGuards $ map (\(_,f) -> classifyFExpr name f) condNexprs
  NoFGuards fexpr    -> classifyFExpr name fexpr

classifyFExpr :: Name -> FExpr -> RecursionClassification
classifyFExpr name fexpr = case fexpr of
  FApp (L _ x,_) fexprs _
    | x == name -> decide $ LeftRecursive : map (classifyFExpr' name) fexprs
    | otherwise -> decide $ NonRecursive  : map (classifyFExpr' name) fexprs
  FAExpr (FVar (L _ x), _, _)
    | x == name -> NonTerminatingRecursion
    | otherwise -> NonRecursive
  _ -> NonRecursive

classifyFExpr' :: Name -> FExpr -> RecursionClassification
classifyFExpr' name fexpr = case fexpr of
  FApp (L _ x,_) fexprs _
    | x == name -> decide $ RightRecursive : map (classifyFExpr' name) fexprs
    | otherwise -> decide $ NonRecursive  : map (classifyFExpr' name) fexprs
  FAExpr (FVar (L _ x), _, _)
    | x == name -> NonTerminatingRecursion
    | otherwise -> NonRecursive
  _ -> NonRecursive

decide :: [RecursionClassification] -> RecursionClassification
decide = foldl decide' NonRecursive
  where decide' LeftRecursive  NonRecursive   = LeftRecursive
        decide' LeftRecursive  RightRecursive = MultipleRecursive
        decide' LeftRecursive  LeftRecursive  = MultipleRecursive
        decide' LeftRecursive  x              = x
        decide' RightRecursive NonRecursive   = RightRecursive
        decide' RightRecursive LeftRecursive  = MultipleRecursive
        decide' RightRecursive RightRecursive = MultipleRecursive
        decide' RightRecursive x              = x
        decide' MultipleRecursive x           = MultipleRecursive
        decide' x MultipleRecursive           = MultipleRecursive
        decide' NonTerminatingRecursion x     = NonTerminatingRecursion
        decide' x NonTerminatingRecursion     = NonTerminatingRecursion
        decide' NonRecursive x                = x

decideAcrossGuards :: [RecursionClassification] -> RecursionClassification
decideAcrossGuards = foldl decide' NonRecursive
  where decide' LeftRecursive  NonRecursive   = LeftRecursive
        decide' LeftRecursive  RightRecursive = MultipleRecursive
        decide' LeftRecursive  LeftRecursive  = LeftRecursive
        decide' LeftRecursive  x              = x
        decide' RightRecursive NonRecursive   = RightRecursive
        decide' RightRecursive LeftRecursive  = MultipleRecursive
        decide' RightRecursive RightRecursive = MultipleRecursive
        decide' RightRecursive x              = x
        decide' MultipleRecursive x           = MultipleRecursive
        decide' x MultipleRecursive           = MultipleRecursive
        decide' NonTerminatingRecursion x     = NonTerminatingRecursion
        decide' x NonTerminatingRecursion     = NonTerminatingRecursion
        decide' NonRecursive x                = x


--------------------- Aux

-- odot : fromLocatedTokenToFunctionVariable
toFVar :: LToken -> FVar
toFVar (L src (Low s)) = L src s

showFuncs :: [TFunc] -> String
showFuncs = intercalate "\n\n" . map showFunc

showFunc :: TFunc -> String
showFunc (name, _, f, _, _, _)
  = name ++ " " ++ showF f

showF :: F -> String
showF SpecialF = "*special func*"
showF (F inpsNTypes fgs fty)
 = showVars inpsNTypes ++ " : " ++ showTy fty ++ " = " ++ showFGS fgs

showTy :: FType -> String
showTy t = case t of
  BitVec _ i -> "Vec " ++ show i
  Bit _      -> "Bit"
  Function _ -> "Function"
  Nat _ i    -> "Nat " ++ show i
  Stream ft  -> "Stream " ++ showTy ft

showVars = intercalate " " . map showVar
showVar (L _ v, t) = "(" ++ v ++ " :: " ++ showTy t ++ ")"

showFGS fgs = case fgs of
  NoFGuards e -> showEx e
  FGuards ces -> "\n" ++ unlines (map (\(c,e) -> "  | " ++ showEx c ++ " = " ++ showEx e) ces)

showEx :: FExpr -> String
showEx fexpr = case fexpr of
  FApp (L _ name, id) fexprs ft -> name ++ "[" ++ show id ++"] " ++ intercalate " " (map showEx' fexprs) ++ " :: " ++ showTy ft
  FAExpr (fvarcons, id, ft) -> showfvarcons fvarcons ++ "[" ++ show id ++ "] :: " ++ showTy ft

showEx' x = "(" ++ showEx x ++ ")"

showfvarcons fvc = case fvc of
  FVar (L _ n) -> n
  FCons (FBin (L _ b)) -> "0b" ++ b
  FCons (FHex (L _ s)) -> "0x" ++ s
  FCons (FDec (L _ i)) -> show i
  FCons FForeverWait -> "_'_"

rightToLeft :: TM ()
rightToLeft = do
  fs <- getFunctions
  forM_ fs $ \(f@(name,l,def,a,(rc,tc,ice),ho)) -> case rc of
    RightRecursive -> case def of
      F _ (NoFGuards _) _ -> error "noconds"
      F fvars (FGuards ces) fty -> do
        let name'      = changeName name
            endingExpr = findEndingExpr name ces
            fvars'     = fvars ++ [(L NoLoc "__acc", fty)]
        ces' <- forM ces $ \(c,e) -> do
          case isRecursive name e of
            False -> return (c, accExpr fty)
            True  -> return (c, leftRecExpr name e)
        removeFunction name
        addFunc (name, l
                ,F fvars (NoFGuards (initialCallExpr
                                      (changeName name)
                                      fvars
                                      fty
                                      endingExpr)) fty
                ,a, (NonRecursive, tc, False), ho)
        addFunc (name', NoLoc, F fvars' (FGuards ces') fty
                ,a+1, (LeftRecursive, tc, ice), ho)
    _ -> ok

initialCallExpr :: Name -> [(FVar,FType)] -> FType -> FExpr -> FExpr
initialCallExpr name fvars fty eexpr
  = FApp
    (L NoLoc name,1)
    (map fvarToExpr fvars ++ [eexpr])
    fty
  where fvarToExpr :: (FVar,FType) -> FExpr
        fvarToExpr (fv,t) = FAExpr (FVar fv,1,t)

changeName :: Name -> Name
changeName = (++ "__left")
                     
leftRecExpr :: Name -> FExpr -> FExpr
leftRecExpr name e = extendBWithA b a
  where a = replaceConsR (replaceAcc e)
        b = changeName' (getB e)

        extendBWithA :: FExpr -> FExpr -> FExpr
        extendBWithA b a = case b of
          FApp lnameid args ty -> FApp lnameid (args ++ [a]) ty
          _ -> error "extendBWithA"

        replaceConsR :: FExpr -> FExpr
        replaceConsR fex = case fex of
          FApp (L s "cons",id) args ty
            -> FApp (L s "consR",id) args ty
          x -> x

        replaceAcc :: FExpr -> FExpr
        replaceAcc fex = case fex of
          FApp (L s n, id) args ty
            | n == name -> FAExpr (FVar (L NoLoc "__acc"), 1, ty)
            | otherwise -> FApp (L s n, id) (map replaceAcc args) ty
          FAExpr _ -> fex

        getB :: FExpr -> FExpr
        getB = onlyOne . getB'
          where onlyOne [x] = x
                onlyOne  xs = error "onlyOne error"
          
        getB' :: FExpr -> [FExpr]
        getB' fex = case fex of
          FApp (L _ n, id) args ty
            | n == name -> [fex]
            | otherwise -> concat (map getB' args)
          FAExpr _ -> []

        changeName' :: FExpr -> FExpr
        changeName' fex = case fex of
          FApp (L s name,id) args ty
            -> FApp (L s (changeName name),id) args ty
          _ -> error "changename'"

isRecursive :: Name -> FExpr -> Bool
isRecursive name fexpr = case classifyFExpr name fexpr of
  LeftRecursive  -> True
  RightRecursive -> True
  _ -> False

accExpr :: FType -> FExpr
accExpr ty = FAExpr (FVar (L NoLoc "__acc"), 1, ty)

findEndingExpr :: Name -> [(FExpr,FExpr)] -> FExpr
findEndingExpr name =
  snd . headErr . filter (not . isRecursive name . snd)
  where
    headErr [] = error "head err"
    headErr xs = head xs

