module Function where

import Prelude hiding (log)
import ParserCore
import LexerCore
import Types
import TransformationMonad
import Aux

import Control.Monad (forM, forM_)
import Control.Monad.Trans (lift)

-- interpret must be more complex for functions
-- with type signatures and pattern matching definitions
interpret :: TM ()
interpret = do
  newStage InterpretationStage
  Program fs <- getProgram
  mapM_ interpretEach fs

interpretEach :: Func -> TMM ()
interpretEach (Func name varsNtypes guards typeExpr) = do
  
  let fname = fmap fromLow name
      srcLoc = getLoc name
      vars  = map fst varsNtypes
      types = map snd varsNtypes

  mfts <- mapM toFType types

  cont mfts $ do

    let fts = map just mfts
        vars' = map toFVar vars
        args = zip vars' fts

    case guards of

      NoGuards body -> do
        
        mFExpr <- getFExpr body
  
        cont1 mFExpr $ \fExpr -> do
      
          mft <- toFType typeExpr
      
          cont1 mft $ \ft -> do
        
            addFunc (funcName, srcLoc, F args (NoFGuards fExpr) ft, length args)
            ret ()

      Guards guards -> do

        mfguards <- forM guards $ \(condExpr,expr) -> do

          mc <- getFExpr condExpr
          me <- getFExpr expr

          cont2 mc me $ \c e -> ret (c,e)

        cont mfguards $ do

          let fguards = map just mfguards

          mft <- toFType typeExpr
      
          cont1 mft $ \ft -> do
        
            addFunc (funcName, srcLoc, F args (FGuards fguards) ft, length args)
            ret ()
    
  where

    funcName = fromLow $ getVal name
    
    fromLow (Low n) = n

    getFExpr :: Expr -> TMM FExpr
    getFExpr expr = case expr of
      
      App (L src (Low s)) es ty -> do
        mes' <- mapM getFExpr es
        cont mes' $ do
          let es' = map just mes'
          mft <- toFType ty
          cont1 mft $ \ft ->
            ret $ FApp (L src s) es' ft
        
      AExpr (L s (Low v), t) -> do
        mft <- toFType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FVar (L s v), ft)
      AExpr (L s (Bin i), t) -> do
        mft <- toFType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FCons(FBin (L s i)), ft)
      AExpr (L s (Hex i), t) -> do
        mft <- toFType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FCons(FHex (L s i)), ft)
      AExpr (L s (Dec i), t) -> do
        mft <- toFType t
        cont1 mft $ \ft ->
          ret $ FAExpr (FCons(FDec (L s i)), ft)

      _ -> throw (TErr
                  ExpressionConstructionErr
                  (Just ("In function " ++ funcName))
                  "Expression construction error"
                  NoLoc) >> noRet

    {-joinApps :: FExpr -> FExpr -> TMM FExpr
    joinApps (FApp f args) expr
      = ret $ FApp f (args ++ [expr])
    joinApps (FAExpr (FCons c)) _
      = throw (TErr
               ErrConstantAsFunction
               (Just ("In function " ++ funcName))
               "Constants are not functions"
               (getLocFromCons c)) >> noRet
    joinApps (FAVar v) expr
      = ret $ FApp v [expr]
    joinApps _ _
      = throw (TErr
               ExpressionConstructionErr
               (Just ("In function " ++ funcName))
               "Expression construction error"
               NoLoc) >> noRet

toName :: String -> String
toName sym = case sym of
  "+" -> "add"
  "-" -> "sub"
  "*" -> "mul"-}

checkForArityErrs :: TM ()
checkForArityErrs = do
  log "Checking for declarations and arity errors"
  fs <- getFunctions
  mapM_ checkFunc fs
  where
    ok = return ()
    
    checkFunc :: TFunc -> TM ()
    checkFunc (_, _, SpecialF, _) = ok
    checkFunc (name, _loc, F vars fg _, _) = case fg of

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
            Just (_,_,_,arity')
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

toFType :: TypeExpr -> TMM FType
toFType texpr = case texpr of
  TApp (TAExpr (L s (Upp "Vec"))) (TAExpr (L _ (Dec n)))
    -> ret $ BitVec s n
  TAExpr (L s (Upp "Bit")) -> ret $ Bit s
  TApp (TAExpr (L s (Upp "Nat"))) (TAExpr (L _ (Dec n)))
    -> ret $ Nat s n
  x -> throw (TErr
              TypeNotPermitted
              Nothing
              ("Type '" ++ show x ++ "' not permitted.")
              NoLoc) >> noRet

toFVar :: LToken -> FVar
toFVar (L src (Low s)) = L src s
