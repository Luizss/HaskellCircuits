module Function where

--------------------- External Imports

import Prelude hiding (log)
import Control.Monad (forM, forM_)
import Control.Monad.Trans (lift)
import Data.List (elemIndex)

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
      addFunctionId (name, id, ftypes)
      return id

    makeFunctionId' :: Name -> FType -> TM Id
    makeFunctionId' name ft = do
      id <- getFunctionId name [ft]
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

