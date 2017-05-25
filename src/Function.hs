module Function where

import Prelude hiding (log)
import ParserCore
import LexerCore
import Types
import Aux
import TransformationMonad

-- interpret must be more complex for functions
-- with type signatures and pattern matching definitions
interpret :: TM ()
interpret = do
  newStage InterpretationStage
  Program decls <- getProgram
  --zipTypesWithBodies decls
  mapM_ interpretEach decls

-- -- checks if every function has a type followed by a body
-- zipTypesWithBodies :: [Decl] -> TMM ()
-- zipTypesWithBodies [] = mok
-- zipTypesWithBodies [FuncType nameLTok _] = do
--   let name = fromLow $ getVal nameLTok
--       loc = getLoc nameLTok
--   throw (TErr
--          FunctionWithoutDefinition
--          Nothing
--          ("Function " ++ name ++ " has no definition")
--          loc)
--   noRet
-- zipTypesWithBodies [Func nameLTok _ _] = do
--   let name = fromLow $ getVal nameLTok
--       loc = getLoc nameLTok
--   throw (TErr
--          FunctionWithoutType
--          Nothing
--          ("Function " ++ name ++ " has no type")
--          loc)
--   noRet
-- zipTypesWithBodies (FuncType fname1 _ : Func fname2 _ _ : ds)
--   | getVal fname1 == getVal fname2 = zipTypesWithBodies ds
--   | otherwise = do
--       let name1 = fromLow $ getVal fname1
--           loc1 = getLoc fname1
--           name2 = fromLow $ getVal fname2
--           loc2 = getLoc fname2
--       throw (TErr
--              FunctionWithoutDefinition
--              Nothing
--              ("Function " ++ name1 ++ " has no definition")
--              loc1)
--       throw (TErr
--              FunctionWithoutType
--              Nothing
--              ("Function " ++ name2 ++ " has no type")
--              loc2)
--       noRet
-- zipTypesWithBodies _ = throw (TErr
--                               FunctionDeclarationsError
--                               Nothing
--                               "Error in declarations of functions (every function must have a type followed by a body)"
--                               NoLoc) >> noRet

interpretEach :: Decl -> TMM ()
interpretEach decl = case decl of
  Func _ _ _ -> interpretFunction decl
  FuncType _ _ -> interpretFunctionType decl

interpretFunctionType :: Decl -> TMM ()
interpretFunctionType (FuncType name typeExpr) = do
  
  mRes <- getFTExpr typeExpr

  cont1 mRes $ \ ftexpr -> do
    let arity = countArity ftexpr
    addFuncType (funcName, srcLoc, ftexpr, arity)
    ret ()

  where

    countArity :: FT -> Int
    countArity ftexpr = case ftexpr of
      FTArrow _ args -> length args - 1
      _              -> 0
      
    srcLoc = getLoc name
    funcName = fromLow $ getVal name
    
    -- parei aqui
    getFTExpr :: TypeExpr -> TMM FT
    getFTExpr typeExpr = case typeExpr of
      
      TArrow e1 e2 -> do
        fe1 <- getFTExpr e1
        fe2 <- getFTExpr e2
        cont2 fe1 fe2 joinArrows
        
      TApp e1 e2 -> do
        fe1 <- getFTExpr e1
        fe2 <- getFTExpr e2
        cont2 fe1 fe2 joinApps
        
      TAExpr (L s (Upp c)) -> ret $ FTAExpr (FTCons (L s c))
      
      TAExpr (L s (Low v)) -> do
        throw (TErr
               TypeVariablesNotPermited
               (Just ("In function " ++ funcName))
               ("Type variable " ++ v ++ " not permited")
               s)
        noRet
      
      TAExpr (L s (Int n)) -> ret $ FTAExpr (FTNat (L s n))

    joinApps :: FT -> FT -> TMM FT
    joinApps expr' expr = case expr' of
      FTApp f args -> ret $ FTApp f (args ++ [expr])
      FTAExpr (FTCons c) -> ret $ FTApp c [expr]
      FTAExpr (FTVar v) -> do
        throw (TErr
               ErrVariableAsTypeFunction
               (Just ("In function " ++ funcName))
               "Variables cannot be used as type functions"
               (getLoc v))
        noRet
      FTAExpr (FTNat n) -> do
        throw (TErr
               ErrNaturalAsTypeFunction
               (Just ("In function " ++ funcName))
               "Naturals cannot be used as type functions"
               (getLoc n))
        noRet
      FTArrow loc args -> do
        throw (TErr
               ErrArrowAsTypeFunction
               (Just ("In function " ++ funcName))
               "Type arrows cannot be used as type functions"
               loc)
        noRet

    joinArrows :: FT -> FT -> TMM FT
    joinArrows (FTArrow l args) (FTArrow _ args') = ret $ FTArrow l (FTArrow l args : args')
    joinArrows fte (FTArrow _ args') = ret $ FTArrow (loc fte) (fte : args')
    joinArrows fte1 fte2 = ret $ FTArrow (loc fte1) [fte1, fte2]

    loc :: FT -> SrcLoc
    loc (FTArrow   l _) = l
    loc (FTApp lname _) = getLoc lname
    loc (FTAExpr (FTVar  lname)) = getLoc lname
    loc (FTAExpr (FTCons lname)) = getLoc lname
    loc (FTAExpr (FTNat  lname)) = getLoc lname
    
interpretFunction :: Decl -> TMM ()
interpretFunction (Func name vars body) = do
  
  let fname = fmap fromLow name
      srcLoc = getLoc name
      args  = map (fmap fromLow) vars
      
  mFExpr <- getFExpr body
  
  cont1 mFExpr $ \fExpr -> do
    addFunc (funcName, srcLoc, F (map noTy args) fExpr, length args)
    ret ()
    
  where

    funcName = fromLow $ getVal name
    
    getFExpr :: Expr -> TMM FExpr
    getFExpr expr = case expr of
      
      App e1 e2 -> do
        me1'  <- getFExpr e1
        me2'  <- getFExpr e2
        cont2 me1' me2' joinApps
        
      Binop ltok e1 e2 -> do
        me1'  <- getFExpr e1
        me2'  <- getFExpr e2
        let binop = FAExpr (FVar (noTy (fmap (\(Sym s) -> toName s) ltok)))
        cont2 me1' me2' $ \e1' e2' -> do
          me1'' <- joinApps binop e1'
          cont1 me1'' $ \e1'' -> joinApps e1'' e2'
          
      AExpr (L s (Low v)) -> ret $ FAExpr (FVar (noTy (L s v)))
      AExpr (L s (Int i)) -> ret $ FAExpr (FCons (noTy (L s i)))

    joinApps :: FExpr -> FExpr -> TMM FExpr
    joinApps expr' expr = case expr' of
      FApp f args -> ret $ FApp f (args ++ [expr])
      FAExpr (FVar v) -> ret $ FApp v [expr]
      FAExpr (FCons c) -> do
        throw (TErr
               ErrConstantAsFunction
               (Just ("In function " ++ funcName))
               "Constants are not functions"
               (getLocL c))
        noRet

toName :: String -> String
toName sym = case sym of
  "+" -> "add"
  "-" -> "sub"
  "*" -> "mul"

-- obsolete?
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
          mf <- searchFunction (getTermL lname)
          case mf of
            Nothing -> throw (TErr
                              FunctionNotDeclared
                              (Just ("In function " ++ name))
                              ("Function " ++ (getTermL lname) ++ " is not declared.")
                              (getLocL lname))
            Just (_,_,_,arity')
              | arity == arity' -> ok
              | otherwise -> throw (TErr
                                    ArityMismatch
                                    (Just ("In function " ++ name))
                                    ("Arity mismatch applying "
                                     ++ (getTermL lname)
                                     ++ " ("
                                     ++ show arity'
                                     ++ " needed, "
                                     ++ show arity
                                     ++ " given)")
                                    (getLocL lname))
          mapM_ check fexprs
