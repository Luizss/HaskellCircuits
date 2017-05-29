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
  let sortByName = sortBy (\(n1,_,_,_) (n2,_,_,_) -> compare n1 n2)
      fsS = sortByName fs
      ftsS = sortByName fts
  zipWithM attachTypeAndBody fsS ftsS
  return ()

attachTypeAndBody :: TFunc -> TFuncType -> TMM ()
attachTypeAndBody (n1,l,f,a1) (n2,_,ft,a2)
  | n1 /= n2  = do
      throw (TErr
             NamesNotTheSame
             Nothing
             ("Names '" ++ n1 ++ "' and '" ++ n2 ++ "' not the same")
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
  newTFuncs <- mapM typeCheckEach fs
  cont newTFuncs $ do
    let a = map just newTFuncs
    putFunctions a
    ret ()

typeCheckEach :: TFunc -> TMM TFunc
typeCheckEach id@(_, _, SpecialF, _) = ret id
typeCheckEach (fname, loc, F vars fex, a) = do

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
      
      FApp tyLName fexs
        | getTermL tyLName == "slice"
          -> typeCheckSlice tyLName fexs i
        | otherwise -> do
        
            mTy <- case isInput tyLName of
              True  -> searchContext fname $ getTermL tyLName
              False -> searchFunctionType $ getTermL tyLName

            cont1 mTy $ \(_,_,_,ty) -> do

              putTypeCheckState ty
              let fexI = zip fexs [0..]
              -- duas passadas
              _ <- mapM (typeCheckExpr ty) fexI
              mfexs' <- mapM (typeCheckExpr ty) fexI

              cont mfexs' $ do
            
                let fexs' = map just mfexs'
                
                mty'' <- getTypeCheckState
                
                cont1 mty'' $ \ty'' -> do

                  popTypeCheckState
  
                  mTy''' <- getTypeCheckState

                  case mTy''' of

                    Nothing -> do

                      match (getReturnType ft) (getReturnType ty'')
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

        typeCheckSlice :: TyL Name -> [FExpr] -> Int -> TMM FExpr
        typeCheckSlice tyLName lst i = case lst of
      
          [FAExpr (FCons i1), FAExpr (FCons i2), l] -> do
        
            mty <- searchFunctionType "slice"
        
            cont1 mty $ \(_,_,_,ty) -> do
          
              putTypeCheckState ty

              -- duas passadas
              _ <- typeCheckExpr ty (l,0)
              ml' <- typeCheckExpr ty (l,0)
          
              cont1 ml' $ \l' -> do
                
                mty' <- getTypeCheckState
            
                cont1 mty' $ \ty' -> do
              
                  popTypeCheckState

                  let vecType = indexTCS 0 ty'

                  case vecType of

                    tyVec@(FTApp
                           (L _ "Vec")
                           [FTAExpr (FTNat (L _ n))]) -> do

                      mfpqo <- sliceFunction
                              (getTermL i1)
                              (getTermL i2)
                              n

                      cont1 mfpqo $ \fpqo -> do
                        
                        let retType = FTApp
                                      (noLoc "Vec")
                                      [FTAExpr (FTNat (noLoc fpqo))]
                            tySlice = FTArrow
                                      NoLoc
                                      [FTApp
                                       (noLoc "Vec")
                                       [FTAExpr (FTNat (noLoc n))]
                                      , retType]

                        mty'' <- getTypeCheckState

                        cont1 mty'' $ \ty'' -> do

                          match (indexTCS i ty'') (retType)
                          ret $ FApp (putType tySlice tyLName) [FAExpr (FCons i1), FAExpr (FCons i2), l']
                      
                    _ -> do throw (TErr
                                   WrongSliceApplication
                                   (Just ("In function " ++ fname))
                                   ("Slice has 3 arguments:"
                                    ++ "2 constants and a vector")
                                   NoLoc)
                            noRet
              
          _ -> do throw (TErr
                         WrongSliceApplication
                         (Just ("In function " ++ fname))
                         ("Slice has 3 arguments:"
                          ++"2 constants and a vector")
                         NoLoc)
                  noRet

        sliceFunction :: Int -> Int -> Int -> TMM Int
        sliceFunction x y z
          | y - x + 1 > z = do
            throw (TErr
                   SliceBiggerThanVector
                   (Just ("In function " ++ fname))
                   "Slice bigger than actual vector"
                   NoLoc)
            noRet
          | y < x = do
            throw (TErr
                   ImpossibleSlice
                   (Just ("In function " ++ fname))
                   ("Impossible slice from "
                    ++ show y
                    ++ " to "
                    ++ show x)
                   NoLoc)
            noRet
          | otherwise = ret $ y - x + 1
        
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
          modifyTypeCheckState (subst v n)
          ret $ FTAExpr (FTNat n)
        match (FTAExpr (FTNat n)) (FTAExpr (FTVar v)) = do
          modifyTypeCheckState (subst v n)
          ret $ FTAExpr (FTNat n)
        match (FTAExpr (FTVar v1)) (FTAExpr (FTVar v2))
          | v1 == v2 = ret $ FTAExpr (FTVar v1)
          | otherwise = do
              modifyTypeCheckState (substVar v2 v1)
              ret $ FTAExpr (FTVar v1)
        match (FTAExpr (FTCons (L _ "Bit"))) (FTApp (L _ "Vec") [FTAExpr (FTNat (L _ 1))]) = ret $ FTAExpr (FTCons (noLoc "Bit"))
        match (FTApp (L _ "Vec") [FTAExpr (FTNat (L _ 1))]) (FTAExpr (FTCons (L _ "Bit"))) = ret $ FTAExpr (FTCons (noLoc "Bit"))
        match fa1 fa2
          | fa1 == fa2 = ret fa1
          | otherwise = typeMatchErr (getLoc'' fa1)

        typeMatchErr :: SrcLoc -> TMM a
        typeMatchErr l = do
          throw (TErr
                 CantMatchTypes
                 (Just ("In function " ++ fname))
                 "Can't match types"
                 l)
          noRet
      
getLoc'' (FTArrow l _) = l
getLoc'' (FTApp  ln _) = getLoc ln
getLoc'' (FTAExpr   x) = getLoc' x

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
  _ -> error "indexTCS" 

getReturnType :: FT -> FT
getReturnType fexpr = case fexpr of
  FTArrow _ args -> last args
  x -> x
