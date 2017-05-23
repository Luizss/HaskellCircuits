module Function where

import Prelude hiding (log)
import ParserCore
import LexerCore
import Types
import TransformationMonad

import Control.Monad.Trans (lift)

-- interpret must be more complex for functions
-- with type signatures and pattern matching definitions
interpret :: TM ()
interpret = do
  newStage InterpretationStage
  Program fs <- getProgram
  mapM_ interpretEach fs

interpretEach :: Func -> TMM ()
interpretEach (Func name vars body) = do
  
  let fname = fmap fromLow name
      srcLoc = getLoc name
      args  = map (fmap fromLow) vars
      
  mFExpr <- getFExpr body
  
  cont1 mFExpr $ \fExpr -> do
    addFunc (funcName, srcLoc, F args fExpr, length args)
    ret ()
    
  where

    funcName = fromLow $ getVal name
    
    fromLow (Low n) = n

    getFExpr :: Expr -> TMM FExpr
    getFExpr expr = case expr of
      
      App e1 e2 -> do
        me1'  <- getFExpr e1
        me2'  <- getFExpr e2
        cont2 me1' me2' joinApps
        
      Binop ltok e1 e2 -> do
        me1'  <- getFExpr e1
        me2'  <- getFExpr e2
        let binop = FAExpr (FVar (fmap (\(Sym s) -> toName s) ltok))
        cont2 me1' me2' $ \e1' e2' -> do
          me1'' <- joinApps binop e1'
          cont1 me1'' $ \e1'' -> joinApps e1'' e2'
          
      AExpr (L s (Low v)) -> ret $ FAExpr (FVar (L s v))
      AExpr (L s (Int i)) -> ret $ FAExpr (FCons (L s i))

    joinApps :: FExpr -> FExpr -> TMM FExpr
    joinApps (FApp f args) expr
      = ret $ FApp f (args ++ [expr])
    joinApps (FAExpr (FCons c)) _
      = throw (TErr
               ErrConstantAsFunction
               (Just ("In function " ++ funcName))
               "Constants are not functions"
               (getLoc c)) >> noRet
    joinApps (FAExpr (FVar v)) expr
      = ret $ FApp v [expr]

toName :: String -> String
toName sym = case sym of
  "+" -> "add"
  "-" -> "sub"
  "*" -> "mul"

checkForArityErrs :: TM ()
checkForArityErrs = do
  log "Checking for declarations and arity errors"
  fs <- getFunctions
  mapM_ checkFunc fs
  where
    ok = return ()
    
    checkFunc :: TFunc -> TM ()
    checkFunc (_, _, SpecialF, _) = ok
    checkFunc (name, _loc, F vars body, _) = check body
      where
        check :: FExpr -> TM ()
        check (FAExpr _) = ok
        check (FApp lname fexprs) = do
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