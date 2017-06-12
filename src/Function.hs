module Function where

--------------------- External Imports

import Prelude hiding (log)
import Control.Monad (forM, forM_)
import Control.Monad.Trans (lift)

--------------------- Internal Imports

import ParserCore
import LexerCore
import Types
import TransformationMonad
import Aux

--------------------- Transforming PResult to TFuncs and adding them to state

getParsedFunctions_TransformToF_AddToState :: TM ()
getParsedFunctions_TransformToF_AddToState = do
  newStage TInterpretationStage
  PResult fs <- getParsedResult
  mapM_ fromParsedFunctionToF_AddToState fs

fromParsedFunctionToF_AddToState :: PFunc -> TMM ()
fromParsedFunctionToF_AddToState (PFunc name varsNtypes guards typeExpr) = do
  
  let fname  = fmap fromLow name
      srcLoc = getLoc name
      vars   = map fst varsNtypes
      types  = map snd varsNtypes

  mfts <- mapM fromParsedTypeExprToFunctionType types

  cont mfts $ do

    let fts = map just mfts
        vars' = map toFVar vars
        args = zip vars' fts

    case guards of

      PNoGuards body -> do
        
        mFExpr <- getFExpr body
  
        cont1 mFExpr $ \fExpr -> do
      
          mft <- fromParsedTypeExprToFunctionType typeExpr
      
          cont1 mft $ \ft -> do
        
            addFunc (funcName, srcLoc, F args (NoFGuards fExpr) ft, length args, classifyFunction funcName (NoFGuards fExpr))
            ret ()

      PGuards guards -> do

        mfguards <- forM guards $ \(condExpr,expr) -> do

          mc <- getFExpr condExpr
          me <- getFExpr expr

          cont2 mc me $ \c e -> ret (c,e)

        cont mfguards $ do

          let fguards = map just mfguards

          mft <- fromParsedTypeExprToFunctionType typeExpr
      
          cont1 mft $ \ft -> do
        
            addFunc (funcName, srcLoc, F args (FGuards fguards) ft, length args, classifyFunction funcName (FGuards fguards))
            ret ()
    
  where

    funcName = fromLow $ getVal name
    
    fromLow (Low n) = n

    -- odot transforms pexpr to fexpr
    -- naming: fromParsedExpressionToFunctionExpression
    getFExpr :: PExpr -> TMM FExpr
    getFExpr expr = case expr of
      
      PApp (L src (Low s)) es ty -> do
        mes' <- mapM getFExpr es
        cont mes' $ do
          let es' = map just mes'
          mft <- fromParsedTypeExprToFunctionType ty
          cont1 mft $ \ft ->
            ret $ FApp (L src s) es' ft
        
      PAExpr (L s (Low v), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FVar (L s v), ft)
      PAExpr (L s (Bin i), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FCons(FBin (L s i)), ft)
      PAExpr (L s (Hex i), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FCons(FHex (L s i)), ft)
      PAExpr (L s (Dec i), t) -> do
        mft <- fromParsedTypeExprToFunctionType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FCons(FDec (L s i)), ft)

      _ -> throw (TErr
                  ExpressionConstructionErr
                  (Just ("In function " ++ funcName))
                  "Expression construction error"
                  NoLoc) >> noRet

fromParsedTypeExprToFunctionType :: PTypeExpr -> TMM FType
fromParsedTypeExprToFunctionType texpr = case texpr of
  PTApp (PTAExpr (L s (Upp "Vec"))) (PTAExpr (L _ (Dec n)))
    -> ret $ BitVec s n
  PTAExpr (L s (Upp "Bit")) -> ret $ Bit s
  PTApp (PTAExpr (L s (Upp "Nat"))) (PTAExpr (L _ (Dec n)))
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
    checkFunc (_, _, SpecialF, _,_) = ok
    checkFunc (name, _loc, F vars fg _, _,_) = case fg of

      NoFGuards body -> check body
      FGuards guards ->
        forM_ guards $ \(c,e) -> (check c >> check e)
        
      where
        check :: FExpr -> TM ()
        check (FAExpr _) = ok
        check (FApp lname fexprs _) = do
          let arity = length fexprs
          mf <- searchFunction (getVal lname)
          case mf of
            Nothing -> throw (TErr
                              FunctionNotDeclared
                              (Just ("In function " ++ name))
                              ("Function " ++ (getVal lname) ++ " is not declared.")
                              (getLoc lname))
            Just (_,_,_,arity',_)
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


--------------------- Classifying Function

                
classifyFunction :: Name -> FGuards -> FunctionClassification
classifyFunction name guards = case guards of
  FGuards condNexprs -> decideAcrossGuards $ map (\(_,f) -> classifyFExpr name f) condNexprs
  NoFGuards fexpr    -> classifyFExpr name fexpr

classifyFExpr :: Name -> FExpr -> FunctionClassification
classifyFExpr name fexpr = case fexpr of
  FApp (L _ x) fexprs _
    | x == name -> decide $ LeftRecursive : map (classifyFExpr' name) fexprs
    | otherwise -> decide $ NonRecursive  : map (classifyFExpr' name) fexprs
  FAExpr (FVar (L _ x), _)
    | x == name -> NonTerminatingRecursion
    | otherwise -> NonRecursive
  _ -> NonRecursive

classifyFExpr' :: Name -> FExpr -> FunctionClassification
classifyFExpr' name fexpr = case fexpr of
  FApp (L _ x) fexprs _
    | x == name -> decide $ RightRecursive : map (classifyFExpr' name) fexprs
    | otherwise -> decide $ NonRecursive  : map (classifyFExpr' name) fexprs
  FAExpr (FVar (L _ x), _)
    | x == name -> NonTerminatingRecursion
    | otherwise -> NonRecursive
  _ -> NonRecursive

decide :: [FunctionClassification] -> FunctionClassification
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

decideAcrossGuards :: [FunctionClassification] -> FunctionClassification
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
