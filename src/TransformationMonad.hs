{-# LANGUAGE RecordWildCards #-}

module TransformationMonad where

--- Imports

import Parser2
import ParserCore
import Lexer2
import LexerCore hiding (L(..), SrcLoc(..))
import Aux
import Types
import Control.Monad.State
import Control.Monad.Trans (lift)

import Prelude hiding (log)
import Data.List (find)

--- Running TM machine (only thing useful is state)

runTM :: TM a -> TState
runTM tm = execState tm initialTState

outTM :: TM a -> a
outTM tm = evalState tm initialTState

--- Useful functions

-- getting stage
getStage :: TM TStage
getStage = do
  state <- get
  return (actualStage state)

-- setting stage
newStage :: TStage -> TM ()
newStage stg = do
  state <- get
  put (state { actualStage = stg })
  log $ "Entering stage " ++ show stg

-- logging information
log :: Msg -> TM ()
log msg = do
  stg <- getStage
  state <- get
  put (state { tLogs = TLog msg stg : tLogs state })

-- logging errors
throw :: TErr -> TM ()
throw err = do
  stg <- getStage
  state <- get
  put (state { tLogs = TLogErr err stg : tLogs state })
  
-- logging debug information
debug :: Msg -> TM ()
debug msg = do
  stg <- getStage
  state <- get
  put (state { tLogs = TLogDebug msg stg : tLogs state })

debugs :: Show a => a -> TM ()
debugs = debug . show

--- Returning and not returning

-- used to retorn in the context TMM
ret :: a -> TMM a
ret = return . Just

-- used to not return anything in the TMM context
-- use this function to return when an error occurs
-- and the error does not make the output possible
noRet :: TMM a
noRet = return Nothing

--- Playing with errors and continuations

mayThrow :: Maybe a -> TErr -> TM ()
mayThrow val err
  | isNothing val = throw err >> return ()
  | otherwise = return ()

throwIf :: Bool -> TErr -> TM ()
throwIf bool err
  | bool = throw err >> return ()
  | otherwise = return ()

cont :: [Maybe a] -> TMM b -> TMM b
cont maybes cont
  | and (map isJust maybes) = cont
  | otherwise = noRet

contIf :: Bool -> TMM b -> TMM b
contIf bool cont
  | bool = cont
  | otherwise = noRet

cont1 :: Maybe a -> (a -> TMM b) -> TMM b
cont1 maybe cont
  | isJust maybe = cont (just maybe)
  | otherwise = noRet

cont2 :: Maybe a -> Maybe b -> (a -> b -> TMM c) -> TMM c
cont2 m1 m2 cont
  | isJust m1 && isJust m2 = cont (just m1) (just m2)
  | otherwise = noRet

cont3 :: Maybe a -> Maybe b -> Maybe c -> (a -> b -> c -> TMM d) -> TMM d
cont3 m1 m2 m3 cont
  | isJust m1 && isJust m2 && isJust m3 = cont (just m1) (just m2) (just m3)
  | otherwise = noRet

----- ??

getSourceLine :: Int -> TM String
getSourceLine n = do
  source <- getSourceCode
  return $ (lines source) !! n

getSourceCode :: TM SourceCode
getSourceCode = do
  st <- get
  return (sourceCode st)
  
putSourceCode :: SourceCode -> TM ()
putSourceCode s = do
  state <- get
  put (state { sourceCode = s })

-- start function: put program that comes from parser
putParsedResult :: PResult -> TM ()
putParsedResult p = do
  state <- get
  put (state { parsedResult = p })

-- getting the parsedResult
getParsedResult :: TM PResult
getParsedResult = do
  st <- get
  return (parsedResult st)

addFunc :: TFunc -> TM ()
addFunc x = do
  state <- get
  put (state { tFuncs =  x : tFuncs state })

addComp :: TComp -> TM ()
addComp x = do
  state <- get
  put (state { components =  x : components state })

getFunctions :: TM [TFunc]
getFunctions = do
  st <- get
  return (tFuncs st)

putFunctions :: [TFunc] -> TM ()
putFunctions fs = do
  st <- get
  put (st { tFuncs = fs })

removeFunction :: Name -> TM ()
removeFunction name = do
  fs <- getFunctions
  let fs' = filter (\(n,_,_,_,_,_) -> n /= name) fs
  putFunctions fs'

isFunctionHighOrder :: Name -> TM Bool
isFunctionHighOrder name = do
  mf <- searchFunction name
  return $ case mf of
    Nothing -> False
    Just (_,_,_,_,_,[]) -> False
    Just _ -> True

highOrderArgs :: Name -> TM [Int]
highOrderArgs name = do
  mf <- searchFunction name
  return $ case mf of
    Nothing -> []
    Just (_,_,_,_,_,xs) -> xs

getComponents :: TM [TComp]
getComponents = do
  st <- get
  return (components st)

putComponents :: [TComp] -> TM ()
putComponents comps = do
  st <- get
  put (st { components = comps })

searchFunction :: Name -> TMM TFunc
searchFunction name = do
  fs <- getFunctions
  return (find (\(n,_,_,_,_,_) -> n == name) fs)

searchComponent :: Name -> TMM TComp
searchComponent name = do
  cs <- getComponents
  return (find (\(n,_) -> n == name) cs)

changeFunction :: Name -> FGuards -> TMM ()
changeFunction name fgs = do
  mf <- searchFunction name
  removeFunction name
  cont1 mf $ \(n, s, F vars _ ft, a, fc, ho) -> do
    addFunc (n, s, F vars fgs ft, a, fc, ho)
    ret ()

{-changeMainName :: Name -> Name -> TM ()
changeMainName = do
  cs <- getComponents
  putComponents $ map ifMainChange cs
  where ifMainChange ("main",main) = ("mainFunc",main)
        ifMainChange x = x-}

addData :: (L Name, [PConstr]) -> TM ()
addData x = do
  st <- get
  put (st { dataDecls = x : dataDecls st })

getInstances :: TM [TInst]
getInstances = do
  st <- get
  return (instances st)

putInstances :: [TInst] -> TM ()
putInstances insts = do
  st <- get
  put (st { instances = insts })

addInstance :: TInst -> TM ()
addInstance inst = do
  st <- get
  put (st { instances = inst : instances st })

isInstanceAdded :: TInst -> TM Bool
isInstanceAdded inst = do
  insts <- getInstances
  return $ elem inst insts 

searchInstances :: CompName -> Name -> TM [TInst]
searchInstances comp name = do
  insts <- getInstances
  let f (c,_,NameId n _,_,_) = n == name && c == comp
  return (filter f insts)

getIdForInstance :: CompName -> Name -> TM Int
getIdForInstance comp name = do
  insts <- searchInstances comp name
  case insts of
    [] -> return 1
    _  -> return (maximum (map getId insts))
  where
    -- forkid changes id only when its used X times
    -- X being the number of forks
    getId (_, _, _, ForkI _ _  o, False) = 1
    getId (_, _, NameId _ id, ForkI _ _  o, True) = id + 1
    getId (_, _, NameId _ id, _ , _) = id + 1

getInstancesFromComponent :: CompName -> TM [TInst]
getInstancesFromComponent comp = do
  insts <- getInstances
  let f (c,_,_,_,_) = c == comp
  return (filter f insts)

getUniqueInstance :: CompName -> NameId -> TMM TInst
getUniqueInstance comp nameId = do
  insts <- getInstances
  case find (\(comp',_,nameId',_,_) -> nameId' == nameId
                                     && comp' == comp) insts of
    Nothing
      -> throw (TErr
                UniqueInstanceNotFound
                Nothing
                ("Unique instance "++ show nameId ++" not found.")
                NoLoc) >> noRet
    Just i -> ret i

modifyUniqueInstance
  :: CompName -> NameId -> (TInst -> TInst) -> TMM ()
modifyUniqueInstance comp nid f = do
  maybeIns <- getUniqueInstance comp nid
  mayThrow maybeIns (TErr
                     ModifyUnexistingInstance
                     Nothing
                     "Tried to modify instance that does not exist"
                     NoLoc)
  cont1 maybeIns $ \_ -> do
    insts <- getInstances
    let replace i@(_,_,nid',_,_)
          | nid == nid' = f i
          | otherwise   = i
    putInstances (map replace insts)
    ret ()

setInstanceUsed :: CompName -> NameId -> TMM ()
setInstanceUsed comp nid = do
  modifyUniqueInstance comp nid (\(cn,id,nid',i,_) -> (cn,id,nid',i,True))

getNextInstance :: CompName -> Name -> TMM TInst
getNextInstance comp name = do
  insts <- searchInstances comp name
  let allUsed = and (map (\(_,_,_,_,u) -> u) insts)
  throwIf allUsed
    (TErr
     AllInstancesUsed
     Nothing
     ("All instances of " ++ name ++ " in " ++ comp)
      NoLoc)
  contIf (not allUsed) $ do
    let whatMatters (_,_,NameId _ id,_,used) = (id,used)
        ius = map whatMatters insts
        id = fst (foldl1 chooseId ius)
        chooseId (id1, True) (id2, True) = (id1,True)
        chooseId (id1, True) (id2,False) = (id2,False)
        chooseId (id1,False) (id2, True) = (id1,False)
        chooseId (id1,False) (id2,False) =
          (if id1 < id2 then id1 else id2, False)
    getUniqueInstance comp (NameId name id)

getConnections :: CompName -> TM [CConn]
getConnections compname = do
  cs <- connections <$> get
  let isCompName (cname,_,_) = cname == compname 
  return (filter isCompName cs)

addConnection :: CConn -> TM ()
addConnection conn = do
  st <- get
  put (st { connections =  conn : connections st })

getLogicalConnections :: TM [Name]
getLogicalConnections = do
  st <- get
  return $ logicalConnections st

addLogicalConnection :: Name -> TM ()
addLogicalConnection conn = do
  st <- get
  put (st { logicalConnections =  conn : logicalConnections st })

doesLogicalConnectionExist :: Name -> TM Bool
doesLogicalConnectionExist conn = do
  st <- get
  return (elem conn (logicalConnections st))

getLogicalOutputs :: TM [(Name,Name,FType,Maybe Int)]
getLogicalOutputs = do
  st <- get
  return $ logicalOutputs st
  
addLogicalOutput :: (Name,Name,FType,Maybe Int) -> TM ()
addLogicalOutput x = do
  st <- get
  put (st { logicalOutputs = x : logicalOutputs st })

doesLogicalOutputExist :: Name -> TM Bool
doesLogicalOutputExist x = do
  st <- get
  if elem x (map fst4 (logicalOutputs st))
    then debug x
    else debug ("Nope " ++ x)
  return (elem x (map fst4 (logicalOutputs st)))

getLogicalOutput :: Name -> TM (Name, FType, Maybe Int)
getLogicalOutput x = do
  st <- get
  return $ (\(a,b,f,c) -> (b,f,c)) $ just $ find ((==x) . fst4) (logicalOutputs st)

addSystemCFile :: File -> TM ()
addSystemCFile file = do
  st <- get
  put (st { systemC =  file : systemC st })

getTimesForked :: TM [(CompName, String, Int)]
getTimesForked = do
  st <- get
  return (timesForked st)

putTimesForked :: [(CompName, String, Int)] -> TM ()
putTimesForked tf = do
  st <- get
  put (st {timesForked = tf })

addForkedIndex :: (CompName, String, Int) -> TM ()
addForkedIndex x = do
  st <- get
  put (st { timesForked = x : timesForked st })

getForkedIndex :: CompName -> String -> TM Int
getForkedIndex comp inp = do
  tf <- getTimesForked
  case find (\(c,i,_) -> c == comp && i == inp) tf of
    Nothing      -> do
      addForkedIndex (comp, inp, 1)
      return 1
    Just (_,_,c) -> return c

putForkedIndex :: CompName -> String -> Int -> TM ()
putForkedIndex comp inp index = do
  tf <- getTimesForked
  case find (\(c,i,_) -> c == comp && i == inp) tf of
    Nothing -> return ()
    Just _ -> putTimesForked (map replace tf)
  where replace (c,i,ind)
          | c == comp
            && i == inp = (c,i,index)
          | otherwise = (c,i,ind)
  
incrementForkedIndex :: CompName -> String -> TM ()
incrementForkedIndex comp v = do
  i <- getForkedIndex comp v
  putForkedIndex comp v (i + 1)

addFuncType :: TFuncType -> TM ()
addFuncType x = do
  state <- get
  put (state { tTypes =  x : tTypes state })

getFunctionId :: Name -> [FType] -> TM Id
getFunctionId name ftypes = do
  fun <- searchFunctionByName name
  case fun of
    [] -> return 1
    xs -> case find (\(_,_,ft) -> and (zipWith equalFType ft ftypes)) xs of
      Nothing      -> return $ (+1) $ maximum $ map takeId xs
      Just (_,i,_) -> return i
  where takeId (_,id,_) = id

searchFunctionByName :: Name -> TM [(Name,Id,[FType])]
searchFunctionByName name = do
  cs <- getFunctionIds
  return (filter (\(n,_,_) -> n == name) cs)

getFunctionIds :: TM [(Name, Id, [FType])]
getFunctionIds = functionIds <$> get

addFunctionId :: (Name, Id, [FType]) -> TM ()
addFunctionId x = do
  st <- get
  put (st { functionIds =  x : functionIds st })

--- Example of the use of TM

testF1 :: TM (Maybe Int)
testF1 = do
  log "At f"
  mayThrow Nothing (TErr ErrConstantAsFunction Nothing "aaaaaaaaaaaaa" NoLoc)
  noRet
  
testF2 :: TM (Maybe Int)
testF2 = do
  -- testF2 gets results from two other processes
  a <- ret 5
  b <- testF1
  -- if any of them retorns nothing (noRet = return Nothing)
  -- then those errors are going to be logged
  mayThrow a (TErr ErrConstantAsFunction Nothing (show a) NoLoc)
  mayThrow b (TErr ErrConstantAsFunction Nothing (show b) NoLoc)
  -- cont function checks if both outputs are ok
  -- if both are it runs the second argument,
  -- if not it returns noRet
  cont [a,b] $ may $ do
    a' <- a
    b' <- b
    return $ a' + b'
