{-# LANGUAGE RecordWildCards #-}

module TransformationMonad where

--- Imports

import ParserCore
import LexerCore
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
getStage :: TM Stage
getStage = do
  state <- get
  return (actualStage state)

-- setting stage
newStage :: Stage -> TM ()
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
putProgram :: Program -> TM ()
putProgram p = do
  state <- get
  put (state { program = p })

-- getting the program
getProgram :: TM Program
getProgram = do
  st <- get
  return (program st)

addFunc :: TFunc -> TM ()
addFunc x = do
  state <- get
  put (state { tFuncs =  x : tFuncs state })

putFunctions :: [TFunc] -> TM ()
putFunctions xs = do
  state <- get
  put (state { tFuncs =  xs })

addFuncType :: TFuncType -> TM ()
addFuncType x = do
  state <- get
  put (state { tFuncTypes =  x : tFuncTypes state })

addComp :: TComp -> TM ()
addComp x = do
  state <- get
  put (state { components =  x : components state })

getFunctions :: TM [TFunc]
getFunctions = do
  st <- get
  return (tFuncs st)

getFunctionTypes :: TM [TFuncType]
getFunctionTypes = do
  st <- get
  return (tFuncTypes st)

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
  return (find (\(n,_,_,_) -> n == name) fs)

searchComponent :: Name -> TMM TComp
searchComponent name = do
  cs <- getComponents
  return (find (\(n,_) -> n == name) cs)

{-changeMainName :: Name -> Name -> TM ()
changeMainName = do
  cs <- getComponents
  putComponents $ map ifMainChange cs
  where ifMainChange ("main",main) = ("mainFunc",main)
        ifMainChange x = x-}
        
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

searchInstances :: CompName -> Name -> TM [TInst]
searchInstances comp name = do
  insts <- getInstances
  let f (c,NameId n _,_,_) = n == name && c == comp
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
    getId (_, _, ForkI _ _  o, False) = 1
    getId (_, NameId _ id, ForkI _ _  o, True) = id + 1
    getId (_,NameId _ id, _ , _) = id + 1

getInstancesFromComponent :: CompName -> TM [TInst]
getInstancesFromComponent comp = do
  insts <- getInstances
  let f (c,_,_,_) = c == comp
  return (filter f insts)

getUniqueInstance :: CompName -> NameId -> TMM TInst
getUniqueInstance comp nameId = do
  insts <- getInstances
  case find (\(comp',nameId',_,_) -> nameId' == nameId
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
    let replace i@(_,nid',_,_)
          | nid == nid' = f i
          | otherwise   = i
    putInstances (map replace insts)
    ret ()

setInstanceUsed :: CompName -> NameId -> TMM ()
setInstanceUsed comp nid = do
  modifyUniqueInstance comp nid (\(cn,nid',i,_) -> (cn,nid',i,True))

getNextInstance :: CompName -> Name -> TMM TInst
getNextInstance comp name = do
  insts <- searchInstances comp name
  let allUsed = and (map (\(_,_,_,u) -> u) insts)
  throwIf allUsed
    (TErr
     AllInstancesUsed
     Nothing
     ("All instances of " ++ name ++ " in " ++ comp)
      NoLoc)
  contIf (not allUsed) $ do
    let whatMatters (_,NameId _ id,_,used) = (id,used)
        ius = map whatMatters insts
        id = fst (foldl1 chooseId ius)
        chooseId (id1, True) (id2, True) = (id1,True)
        chooseId (id1, True) (id2,False) = (id2,False)
        chooseId (id1,False) (id2, True) = (id1,False)
        chooseId (id1,False) (id2,False) =
          (if id1 < id2 then id1 else id2, False)
    getUniqueInstance comp (NameId name id)

getConnections :: CompName -> TM [TConn]
getConnections compname = do
  cs <- connections <$> get
  let isCompName (cname,_,_) = cname == compname 
  return (filter isCompName cs)

addConnection :: TConn -> TM ()
addConnection conn = do
  st <- get
  put (st { connections =  conn : connections st })

addSystemCFile :: File -> TM ()
addSystemCFile file = do
  st <- get
  put (st { systemC =  file : systemC st })

getContext :: TM Context
getContext = do
  st <- get
  return (context st)
  
addContext :: ContextUnit -> TM ()
addContext cUnit = do
  st <- get
  put (st { context =  cUnit : context st })

searchContext :: FuncName -> Name -> TMM ContextUnit
searchContext func var = do
  ctxt <- getContext
  let g (f,_,v,_) = f == func && v == var
  return (find g ctxt)

changeContext cUnit@(f,_,v,_) = do
  st <- get
  let cs = context st
  put (st { context = map replace cs })
  where replace id@(f',_,v',_)
          | f == f' && v == v' = cUnit
          | otherwise = id
  
searchFunctionType :: FuncName -> TMM ContextUnit
searchFunctionType func = do
  ctxt <- getContext
  let g (f,_,v,_) = f == func && v == func
  return (find g ctxt)

getTimesForked :: TM [(CompName, Input, Int)]
getTimesForked = do
  st <- get
  return (timesForked st)

putTimesForked :: [(CompName, Input, Int)] -> TM ()
putTimesForked tf = do
  st <- get
  put (st {timesForked = tf })

addForkedIndex :: (CompName, Input, Int) -> TM ()
addForkedIndex x = do
  st <- get
  put (st { timesForked = x : timesForked st })

getForkedIndex :: CompName -> Input -> TM Int
getForkedIndex comp inp = do
  tf <- getTimesForked
  case find (\(c,i,_) -> c == comp && i == inp) tf of
    Nothing      -> do
      addForkedIndex (comp, inp, 1)
      return 1
    Just (_,_,c) -> return c

putForkedIndex :: CompName -> Input -> Int -> TM ()
putForkedIndex comp inp index = do
  tf <- getTimesForked
  case find (\(c,i,_) -> c == comp && i == inp) tf of
    Nothing -> return ()
    Just _ -> putTimesForked (map replace tf)
  where replace (c,i,ind)
          | c == comp
            && i == inp = (c,i,index)
          | otherwise = (c,i,ind)
  
incrementForkedIndex :: CompName -> Input -> TM ()
incrementForkedIndex comp v = do
  i <- getForkedIndex comp v
  putForkedIndex comp v (i + 1)

getTypeCheckState :: TMM FT
getTypeCheckState = do
  st <- get
  let tcs = typeCheckState st
  case tcs of
    []   -> noRet
    x:xs -> ret x

putTypeCheckState :: FT -> TM ()
putTypeCheckState ft = do
  st <- get
  let tcs = typeCheckState st
  put ( st { typeCheckState = ft : tcs } )

popTypeCheckState :: TM ()
popTypeCheckState = do
  st <- get
  let tcs = typeCheckState st
  case tcs of
    [] -> error "yyyyy1"
    x:xs -> put ( st { typeCheckState = xs } )

modifyTypeCheckState :: (FT -> FT) -> TM ()
modifyTypeCheckState func = do
  st <- get
  let tcs = typeCheckState st
  debugs tcs
  case tcs of
    []   -> return ()
    x:xs -> debug "tudo" >> put ( st { typeCheckState = func x : xs })

isEmptyTypeCheckState :: TM Bool
isEmptyTypeCheckState = do
  x <- getTypeCheckState
  case x of
    Nothing -> return True
    Just x  -> return False
    
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
