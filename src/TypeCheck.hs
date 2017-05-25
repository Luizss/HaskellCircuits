module TypeCheck where

import Prelude hiding (log)
import ParserCore
import LexerCore
import Types
import Aux
import TransformationMonad

import Data.List (sortBy)
import Control.Monad (zipWithM, forM, when)

createContext :: TM ()
createContext = do
  fs  <- getFunctions
  fts <- getFunctionTypes
  debugs $ length fs
  debugs $ length fts
  let sortByName = sortBy (\(n1,_,_,_) (n2,_,_,_) -> compare n1 n2)
      fsS = sortByName fs
      ftsS = sortByName fts
  debugs $ map (\(n1,_,_,_) -> n1) fsS
  debugs $ map (\(n1,_,_,_) -> n1) ftsS
  zipWithM attachTypeAndBody fsS ftsS
  return ()

attachTypeAndBody :: TFunc -> TFuncType -> TMM ()
attachTypeAndBody (n1,l,f,a1) (n2,_,ft,a2)
  | n1 /= n2  = do
      throw (TErr
             NamesNotTheSame
             Nothing
             ("Names " ++ n1 ++ " and " ++ n2 ++ " not the same")
             l)
      noRet
  | a1 /= a2  = do
      throw (TErr
             NumberOfArgumentsDoesntMatch
             Nothing
             ("Function type and definition do not have same number of arguments (definition: "
              ++ show a1 ++ ", type: " ++ show a2 ++ ")")
             l)
      noRet
  | otherwise = do
      addContext (n1, l, n1, ft)
      attachFunctionToType n1 f ft

attachFunctionToType :: Name -> F -> FT -> TMM ()
attachFunctionToType fname f ft = case ft of
  FTArrow l  [] -> do
      throw (TErr
             EmptyListInTArrow
             Nothing
             "Empty list in type arrow"
             l)
      noRet
  FTArrow l [x] -> do
      throw (TErr
             SingletonListInTArrow
             Nothing
             "Singleton list in tyxpe arrow"
             l)
      noRet
  FTArrow l xs -> attachTypes f xs -- varios
  ftexpr       -> attachTypes f [ftexpr] -- apenas 1
  where
    attachTypes :: F -> [FT] -> TMM ()
    attachTypes SpecialF _ = ret ()
    attachTypes (F vars body) ftexprs = do
      zipWithM attachTypeToVariable vars ftexprs
      ret ()

    attachTypeToVariable var ftexpr = do
      addContext (fname, getLocL var, getTermL var, ftexpr)
      ret ()

typeCheck :: TMM ()
typeCheck = do
  fs <- getFunctions
  newTFuncs <- forM fs typeCheckEach
  cont newTFuncs $ do
    debug "isso"
    let a = map just newTFuncs
    putFunctions a
    ret ()

typeCheckEach :: TFunc -> TMM TFunc
typeCheckEach id@(_, _, SpecialF, _) = ret id
typeCheckEach (fname, loc, F vars fex, a) = do

  debug $ "\nname: " ++ fname ++ "\n"
  
  mType <- searchFunctionType fname
  mts <- mapM (searchContext fname) (map getTermL vars)
         
  cont1 mType $ \(_, _, _, ftex) -> do
    
    mfex' <- typeCheckExpr ftex (fex,0)
    
    cont1 mfex' $ \fex' -> do
      
      cont mts $ do
        
        let ts = map ((\(_,_,_,ft) -> ft) . just) mts
            vars' = zipWith putType ts vars
              
        ret (fname, loc, F vars' fex', a)

  where

    isInput x = elem (getTerm x) (map getTerm vars)
    
    typeCheckExpr :: FT -> (FExpr,Int) -> TMM FExpr
    typeCheckExpr ft (fex, i) = case fex of
      
      FApp tyLName fexs -> do
        
        mTy <- case isInput tyLName of
          True  -> searchContext fname $ getTermL tyLName
          False -> searchFunctionType $ getTermL tyLName

        cont1 mTy $ \(_,_,_,ty) -> do
          
          putTypeCheckState ty
          let fexI = zip fexs [0..]
          mfexs' <- mapM (typeCheckExpr ty) fexI

          cont mfexs' $ do
            
            let fexs' = map just mfexs'
            mty' <- getTypeCheckState

            cont1 mty' $ \ty' -> do
              _r <- match (getReturnType ft) (getReturnType ty')

              cont [_r] $ do
                mty'' <- getTypeCheckState
                
                cont1 mty'' $ \ty'' -> do
                  
                  popTypeCheckState

                  mTy''' <- getTypeCheckState

                  case mTy''' of

                    Nothing -> do
                  
                      ret $ FApp (putType ty'' tyLName) fexs'

                    Just ty''' -> do

                      match (indexTCS i ty''') (getReturnType ty'')
                      ret $ FApp (putType ty'' tyLName) fexs'
    
      FAExpr (FVar v)   -> do
        
        mTy <- case isInput v of
          True  -> searchContext fname $ getTermL v
          False -> searchFunctionType $ getTermL v
          
        cont1 mTy $ \(_,_,_,ty) -> do
          mTy' <- getTypeCheckState
          case mTy' of
            Nothing -> do
              mTy'' <- match ft ty
              cont1 mTy'' $ \ty'' -> do
                ret $ FAExpr $ FVar $ putType ty'' v
            Just ty' -> do
              mTy'' <- match (indexTCS i ty') ty
              cont1 mTy'' $ \ty'' -> do
                ret $ FAExpr $ FVar $ putType ty'' v
          
      FAExpr (FCons c)
        | getTermL c == 0  || getTermL c == 1 -> do
            let typeBitOrVec
                  = FTApp (noLoc "Vec") [FTAExpr (FTVar (noLoc "m"))]
            mTy' <- getTypeCheckState
            case mTy' of
              Nothing -> do
                mTy'' <- match ft typeBitOrVec
                cont1 mTy'' $ \ty'' ->
                  ret $ FAExpr $ FCons $ putType ty'' c
              Just ty' -> do
                mTy'' <- match (indexTCS i ty') typeBitOrVec
                cont1 mTy'' $ \ty'' ->
                  ret $ FAExpr $ FCons $ putType ty'' c
            
        | getTermL c > 1 -> do
            let typeBitOrVec
                  = FTApp (noLoc "Vec") [FTAExpr (FTVar (noLoc "m"))]
            mTy' <- getTypeCheckState
            case mTy' of
              Nothing -> do
                mTy'' <- match ft typeBitOrVec
                cont1 mTy'' $ \ty'' ->
                  ret $ FAExpr $ FCons $ putType ty'' c
              Just ty' -> do
                mTy'' <- match (indexTCS i ty') typeBitOrVec
                cont1 mTy'' $ \ty'' ->
                  ret $ FAExpr $ FCons $ putType ty'' c

      where
        match :: FT -> FT -> TMM FT
        match (FTArrow l _) (FTArrow _ _) = typeMatchErr l
        match (FTApp f args) (FTApp f' args')
          | f == f' = do
              mArgs'' <- zipWithM match args args'
              cont mArgs'' $ do
                let args'' = map just mArgs''
                ret $ FTApp f args''
          | otherwise = typeMatchErr (getLoc f)
        match (FTAExpr (FTVar v)) (FTAExpr (FTNat n)) = do
          debug "varnat"
          debugs v
          debugs n
          a <- getTypeCheckState
          modifyTypeCheckState (subst v n)
          b <- getTypeCheckState
          debugs a
          debugs b
          ret $ FTAExpr (FTNat n)
        match (FTAExpr (FTNat n)) (FTAExpr (FTVar v)) = do
          debug "natvar"
          debugs v
          debugs n
          a <- getTypeCheckState
          modifyTypeCheckState (subst v n)
          b <- getTypeCheckState
          debugs a
          debugs b
          ret $ FTAExpr (FTNat n)
        match (FTAExpr (FTVar v1)) (FTAExpr (FTVar v2))
          | v1 == v2 = ret $ FTAExpr (FTVar v1)
          | otherwise = do
              debugs (v1,v2)
              a<- getTypeCheckState
              modifyTypeCheckState (substVar v2 v1)
              b<- getTypeCheckState
              debugs a
              debugs b
              
              ret $ FTAExpr (FTVar v1)
        match (FTAExpr fa1) (FTAExpr fa2)
          | fa1 == fa2 = do debugs fa1 ;ret (FTAExpr fa1)
          | otherwise = typeMatchErr (getLoc' fa1)

        typeMatchErr :: SrcLoc -> TMM a
        typeMatchErr l = do
          throw (TErr
                 CantMatchTypes
                 (Just ("In function " ++ fname))
                 "Can't match types"
                 l)
          noRet

getLoc' (FTVar  v) = getLoc v
getLoc' (FTCons c) = getLoc c
getLoc' (FTNat  n) = getLoc n

subst :: L Name -> L Int -> FT -> FT
subst v n = go
  where
    go ft = case ft of
      FTArrow l fts -> FTArrow l (map go fts)
      FTApp f fts -> FTApp f (map go fts)
      FTAExpr (FTVar v') -> if v' == v
                            then FTAExpr (FTNat  n)
                            else FTAExpr (FTVar v')
      x -> x

substVar :: L Name -> L Name -> FT -> FT
substVar v1 v2 = go
  where
    go ft = case ft of
      FTArrow l fts -> FTArrow l (map go fts)
      FTApp f fts -> FTApp f (map go fts)
      FTAExpr (FTVar v)
        | v == v1   -> FTAExpr (FTVar v2)
        | otherwise -> FTAExpr (FTVar v)
      x -> x

  
indexTCS :: Int -> FT -> FT
indexTCS i ft = case ft of
  FTArrow _ fts -> fts !! i
  _ -> error "gasgasgase"
  

{-     maybeFexpr' <- typeCheckFExpr (getReturnType funcType, fexpr)
     mts <- mapM (searchContext fname) (map getTermL vars)
     
     cont1 maybeFexpr' $ \fexpr' -> do
       cont mts $ do
         let ts = map ((\(_,_,_,ft) -> ft) . just) mts
             vars' = zipWith putType ts vars
         ret (fname, loc, F vars' fexpr', ari)

  where 

    typeCheckFExpr :: (FT,FExpr) -> TMM FExpr
    typeCheckFExpr (returnType, fex) = go

      where 
        go :: TMM FExpr
        go = case fex of
      
          FApp tyLName params -> do
        
            mFuncType <- searchFunctionType (getTermL tyLName)
        
            cont1 mFuncType $ \(_,_,_,FT tyVars fte) ->
              
              case fte of
                
                FTArrow _ fxs -> forM (zip params fxs) $ \(p, fx) -> do

                  p
                    undefined
                  
                  mParams' <- typeCheckApp ft params

                  cont mParams' $ do
                    let params' = map just mParams'
                    mIsEq <- match (getReturnType ft) returnType
                    cont1 mIsEq $ \isEq -> case isEq of
                      True  -> ret (FApp (putType ft tyLName) params')
                      False -> debug "bbbbbbbbb" >> noRet
                      
                _ -> error "zzzzzzzzzz"
          
            -- checar parametros e tipo de retorno tambem
            
          FAExpr (FVar tyLName) -> do

            let isInput x = elem (getTerm x) (map getTerm vars)
            
            m <- searchContext fname $ case isInput tyLName of
              True  -> getTermL tyLName
              False -> fname

            cont1 m $ \(_,_,_,varType) -> do
              mIsEq <- match returnType varType
              cont1 mIsEq $ \isEq -> case isEq of
                True  -> ret $ FAExpr $ FVar $ putType varType tyLName
                False -> debug ("aaaaaaaaaaa" ++ getTermL tyLName) >> noRet
            
          FAExpr (FCons tyLInt) -> ret $ FAExpr $ FCons tyLInt

        match :: FT -> FT -> TMM Bool
        match (FT ts e) (FT ts' e') = case (e,e') of

          (FTArrow _ _, _) -> error "impossibru"
          
          (FTApp cons exps, FTApp cons' exps') -> do
            let consEqual = cons == cons'
                toFT t = map (FT t)
            zipWithM match (toFT ts exps) (toFT ts' exps')
            ret True
            
          FTAExpr (FTVar (L _ v))
            | elem v tvs -> undefined
            | otherwise -> error ".........."
            
          (a,b) -> ret $ a == b  
          

        getFTArrowFromFT e = case e of
          FTArrow loc ftexprs -> ret e
          _ -> debug "xxxx" >> noRet
          
        typeCheckApp (FT tyVars texpr) params = case texpr of
          FTArrow loc ftexprs -> do
            let args = zip (map (FT tyVars) ftexprs) params
            mapM typeCheckFExpr args
          _ -> debug "xxxx" >> (\x->[x]) <$> noRet
          


-}

getReturnType :: FT -> FT
getReturnType fexpr = case fexpr of
  FTArrow _ args -> last args
  x -> x
