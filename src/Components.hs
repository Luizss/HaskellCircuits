module Components where

import Prelude hiding (lookup)
import Function
import LexerCore (L(..))
import Control.Monad.State
import Data.Map hiding ((\\), map, findIndex)
import Data.List ((\\), findIndex)
import Control.Monad

type Input  = (String,String)
type Output = (String,String)
type Conns = [((String,String),(String,String))]
data Proc = Get String Proc
          | Put String String Proc
          | EndProc
          deriving Show
type Id = String
data Instance = Instance String{-modulename-} Id [Input] [Output]
              | ConsInstance Int Id Input
              | SpecialInstance String Id [Input] [Output]
              | Fifo Input Output
              | DummyInstance
              deriving Show

data C = C FName F [Instance] [Input] [Output] Conns Proc
       | Dummy -- in order to get more errors 
       deriving Show

type Errors = [String]

type A a = State (Map (L FName) F, Map FName C, Errors) a

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y
instance Ord a => Ord (L a) where
  compare (L _ x) (L _ y) = compare x y

toComponents :: [(L FName, F)] -> Either String [(FName, C)]
toComponents fs
  = case execState toC (fromList fs, fromList [], []) of
      (_,cs,[])
        -> Right (toList cs)
      (_,cs,es)
        -> Left (Prelude.foldl1 (\a b -> a ++ "\n" ++ b) es)

toC :: A ()
toC = go "main"
  where go name
          | special name = do
              removeFunction name
          | otherwise = do
              maybeF <- findFunction name
              case maybeF of
                Just f -> do
                  removeFunction name
                  c <- synth name f
                  addComponent name c
                Nothing -> do
                  maybeC <- findComponent name
                  case maybeC of
                    Just c -> return ()
                    Nothing -> do        
                      addError ("Variable " ++ name ++ " not in declared.")
  
        synth :: FName -> F -> A C
        synth name f = do
          let deps = getDependencies f
          esBefore <- getErrors
          mapM_ go deps
          esAfter <- getErrors
          if esBefore == esAfter
            then analyse name f
            else return Dummy

        analyse :: FName -> F -> A C
        analyse name f@(F _ fexpr) = do
          let inps = getInputsFromF f
              deps = getDependencies f
              cons = getConstantsFromF f
          depInsts <- mapM makeInstancesFromDep deps
          consInsts <- mapM makeInstancesFromCons cons
          let insts = depInsts ++ consInsts
              inps' = makeInputs name inps
              outs' = makeOutput name
              (conns,insts'') = makeConns name insts inps inps' outs' fexpr
          return $
            C
            name
            f
            (insts ++ insts'')
            inps'
            outs'
            conns
            EndProc

        makeInputs name = zipWith (\x y -> (spec name, "in_" ++ (show x))) [0..]

        makeOutput name = [(spec name,"out_0")]
        
        makeInstancesFromCons cons = do
          return $ ConsInstance cons ("constant_" ++ show cons) (("constant_" ++ show cons,"out_0"))
                
        makeInstancesFromDep dep = do
          maybeC <- findComponent dep
          case maybeC of
            Nothing    ->
              if special dep
              then makeSpecial dep
              else addError "wat" >> return DummyInstance
            Just Dummy -> return DummyInstance
            Just (C n _ _ inps outs _ _) -> do
              return $
                Instance
                n
                n
                inps
                outs
              
        makeSpecial s = return $
                        SpecialInstance
                        s
                        ("special_" ++ s)
                        [("special_" ++ s, "in_0")
                        ,("special_" ++ s, "in_1")]
                        [("special_" ++ s, "out_0")]

        makeConns
          :: String
          -> [Instance]
          -> [FName]
          -> [Input]
          -> [Output]
          -> FExpr
          -> (Conns,[Instance])
        makeConns name insts inps inps' [outp] f = go f [] insts
          where
            go
              :: FExpr
              -> Conns
              -> [Instance]
              -> (Conns,[Instance])
            go fexpr cs ins  = case fexpr of
              FApp namef@(L _ n) args
                -> join $
                   ([((spec n,"out_0"), specTup outp)]
                   ,[]) : mapIndex (go' namef cs ins) args
              FAExpr (FVar v@(L _ vs))
                -> let maybei = findIndex (== v) (map anyL inps)
                   in case maybei of
                        Nothing -> error "Undefined?"
                        Just i -> ([(specTup (inps' !! i), specTup outp)],[])
              FAExpr (FCons (L _ v))
                -> ([(("constant_" ++ show v, "out_0"), specTup outp)]
                   ,[]) --conn c com output
            go'
              :: L FName
              -> Conns
              -> [Instance]
              -> Int
              -> FExpr
              -> (Conns,[Instance])
            go' (L _ namef) cs ins i fe = case fe of
              FApp (L _ nameg) args
                -> let fifo = Fifo ("fifo","in_0") ("fifo","out_0")
                   in join $
                      ([((spec nameg, "out_0"), ("fifo","in_0"))
                       ,(("fifo", "out_0"), (spec namef,"in_" ++ show i))]
                      ,[fifo]) : mapIndex (go' (anyL nameg) cs ins) args
              FAExpr (FVar v@(L _ vs))
                -> let maybei = findIndex (== v) (map anyL inps)
                   in case maybei of
                        Nothing -> error "Undefined?"
                        Just j -> ([(inps' !! j, (spec namef,"in_" ++ show i))],[])
              FAExpr (FCons (L _ c))
                -> ([(("constant_" ++ show c, "out_0"), (spec namef, "in_" ++ show i))], [])
              
          --error $ (show a) ++ (show b) ++ (show c) ++ (show f)

            join :: [(Conns,[Instance])] -> (Conns, [Instance])
            join xs = (cs, ins) where cs = concat (map fst xs)
                                      ins = concat (map snd xs)

mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex = go 0
  where go _ _ [] = []
        go i f (x:xs) = f i x : go (i+1) f xs

spec :: FName -> FName
spec name
  | special name = "special_" ++ name
  | otherwise    = name

specTup :: (FName,a) -> (FName,a)
specTup (n,x) = (spec n,x)

special :: FName -> Bool
special n
  = n `elem` ["add","sub","mul"]

specialSym :: FName -> Bool
specialSym n
  = n `elem` ["+","-","*"]

getVariablesFromF :: F -> [FName]
getVariablesFromF (F fvars expr) = go expr
  where go :: FExpr -> [FName]
        go (FApp (L _ n) es) = [n] ++ concat (map go es)
        go (FAExpr (FVar x)) = [getVal x]
        go (FAExpr _) = []

getConstantsFromF :: F -> [Int]
getConstantsFromF (F fvars expr) = go expr
  where go :: FExpr -> [Int]
        go (FApp _ es) = concat (map go es)
        go (FAExpr (FCons c)) = [getVal c]
        go (FAExpr _) = []

getInputsFromF :: F -> [FName]
getInputsFromF (F fvars expr) = fmap getVal fvars

getDependencies :: F -> [FName]
getDependencies f = getVariablesFromF f \\ getInputsFromF f -- less constants less inputs

{-addMaybeComponent :: FName -> Maybe C -> A ()
addMaybeComponent _ Nothing = return ()
addMaybeComponent n (Just c) = addComponent n c-}

throwErrors :: A ()
throwErrors = do
  errs <- getErrors
  case errs of
    [] -> return ()
    _  -> error (Prelude.foldl1 (\a b -> a ++ "\n\n" ++ b) errs)
  
anyL x = L undefined x

getFunctions :: A (Map (L FName) F)
getFunctions = do
  (fs,_,_) <- get
  return fs

putFunctions :: Map (L FName) F -> A ()
putFunctions fs = do
  (_,cs,es) <- get
  put (fs,cs,es)

getComponents :: A (Map FName C)
getComponents = do
  (_,cs,_) <- get
  return cs

putComponents :: Map FName C -> A ()
putComponents cs = do
  (fs,_,es) <- get
  put (fs,cs,es)

addComponent :: FName -> C -> A ()
addComponent name c = do
  cs <- getComponents
  putComponents (insert name c cs)

getErrors :: A Errors
getErrors = do
  (_,_,errs) <- get
  return errs

putErrors :: Errors -> A ()
putErrors errs = do
  (fs,cs,_) <- get
  put (fs,cs,errs)

addError :: String -> A ()
addError err = do
  errs <- getErrors
  putErrors (errs ++ [err])
  
findFunction :: FName -> A (Maybe F)
findFunction name = do
  fs <- getFunctions
  return (lookup (anyL name) fs)

removeFunction :: FName -> A ()
removeFunction name = do
  fs <- getFunctions
  putFunctions (delete (anyL name) fs)

findComponent :: FName -> A (Maybe C)
findComponent name = do
  cs <- getComponents
  return (lookup name cs)
