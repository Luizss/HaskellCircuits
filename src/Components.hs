module Components where

import Prelude hiding (lookup,log)

import Function
import LexerCore
import TransformationMonad
import Types
import Aux

import Control.Monad.State
import Data.List ((\\), findIndex, nub, nubBy, elem)
import Control.Monad

toComponents :: TM ()
toComponents = do
  log "Entering toComponents"
  toC "main"
  return ()

-- This logic may change with recursive functions
toC :: Name -> TMM ()
toC name
  | special name = mok
  | otherwise = do
      maybeC <- searchComponent name
      case maybeC of
        Just c -> mok
        Nothing -> do
          maybeF <- searchFunction name
          case maybeF of
            Nothing -> throw (TErr
                              FunctionNotDeclared
                              Nothing
                              ("Function "
                               ++ name
                               ++ " is not declared.")
                              NoLoc) >> noRet
            Just f -> do
              mC <- synth f
              cont1 mC $ \c -> do
                addComp (name, c)
                ret ()
            
synth :: TFunc -> TMM C
synth tfunc@(name,_,f,_) = do
  let deps   = getDependencies f
      consts = getConstantsFromF f
  cs <- mapM toC (nub deps)
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM_ (makeInstancesFromConst name) consts
    -- throw err with okdeps
    cont okDeps $ do
      connect tfunc
      insts <- getInstancesFromComponent name
      conns <- getConnections name
      let c = C
              f
              insts
              (getInputsFromF f)
              "out"
              conns
              EndProc
      ret c

makeInstancesFromConst :: CompName -> Int -> TM ()
makeInstancesFromConst compName const = do
  let nameInst = "const" ++ show const
  id <- getIdForInstance compName nameInst
  let inst = (compName
             , NameId nameInst id
             , ConstI
               const
               "out"
             , False)
  addInstance inst

makeInstancesFromDep :: CompName -> String -> TMM ()
makeInstancesFromDep compName dep
  | special dep = makeSpecialInstance compName dep
  | otherwise = do
      maybeC <- searchComponent dep
      case maybeC of
        Nothing ->
          throw (TErr
                 ComponentNotDone
                 Nothing
                 ("Component "
                   ++ dep
                   ++ " not created")
                 NoLoc) >> noRet
        Just (n, C _ _ inps out _ _) -> do
          id <- getIdForInstance compName n
          let inst = (compName, NameId n id, I inps out, False)
          addInstance inst
          ret ()

makeSpecialInstance :: CompName -> Name -> TMM ()
makeSpecialInstance compName s = do
  id <- getIdForInstance compName s
  let inst = (compName
             , NameId s id
             , SpecialI ["in1","in2"] "out"
             , False)
  addInstance inst
  ret ()
      
connect :: TFunc -> TMM ()
connect (comp,_,F inps fexpr,arity) =
  connectOutter fexpr
  --go f [] insts
  where

    getOut :: I -> Output
    getOut i = case i of
      I        _ out -> out
      ConstI   _ out -> out
      SpecialI _ out -> out
      FifoI    _ out -> out

    getInput :: Int -> I -> TMM Input
    getInput i inst = case inst of
      I inps _
        | i < length inps -> ret (inps !! i)
        | otherwise -> error'
      ConstI inp _
        | otherwise -> error''
      SpecialI inps _
        | i < length inps -> ret (inps !! i)
        | otherwise -> error'
      FifoI inp _
        | i == 0 -> ret inp
        | otherwise -> error'
      where error'  = throw (TErr
                            WrongInstanceNumberInput
                            Nothing
                            "Wrong number os inputs of instance"
                            NoLoc) >> noRet
            error'' = throw (TErr
                            ConstantsHaveNoInputs
                            Nothing
                            "Constants have no input"
                            NoLoc) >> noRet
      
    connectOutter :: FExpr -> TMM ()
    connectOutter fexp = do
      case fexp of
        FApp (L _ instName) args -> do
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, "out"))
            setInstanceUsed comp nid
            mapM_ (connectInner inst) (zip args [0..])
            ret ()
        FAExpr (FVar v@(L _ vs))
          | elem v inps -> 
              addConnection (comp
                            ,(NameId comp 1, vs)
                            ,(NameId comp 1, "out")) >> ret ()
          | otherwise -> do
              minst <- getNextInstance comp vs
              mayThrow minst (TErr
                              CouldntGetNextInstance
                              Nothing
                              ("Couldn't get next instance for "
                               ++ vs)
                              NoLoc)
              cont1 minst $ \inst@(_,nid',ins',_) -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(NameId comp 1, "out"))
                setInstanceUsed comp nid'
                ret ()
            
        FAExpr (FCons (L _ c)) -> do
          let instName = "const" ++ show c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, "out"))
            setInstanceUsed comp nid

    connectInner :: TInst -> (FExpr,Int) -> TMM ()
    connectInner inst@(_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ instName) args' -> do
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst'@(_,nid',ins',_) -> do
            mayInp <- getInput n ins
            cont1 mayInp $ \inp -> do
              let fifoName = "__fifo__"
              fifoId <- getIdForInstance comp fifoName
              let fifo = FifoI "in" "out"
                  fifoNId = NameId fifoName fifoId
              addInstance (comp, fifoNId, fifo, True)
              addConnection (comp
                            ,(nid', getOut ins')
                            ,(fifoNId, "in"))
              addConnection (comp
                            ,(fifoNId, "out")
                            ,(nid, inp))
              setInstanceUsed comp nid'
              mapM_ (connectInner inst') (zip args' [0..])
              ret ()
              
      FAExpr (FVar v@(L _ vs))
        | elem v inps -> do

            let c = count v (getInputsInBody inps fexpr)

            case c > 1 of

              True -> do

                i <- getForkedIndex comp vs

                let forkName = "__fork" ++ show c ++ "__"
                forkId <- getIdForInstance comp forkName
                let forkOuts = map (("out"++). show) [1..c]
                    fork     = ForkI c "in" forkOuts
                    forkNId  = NameId forkName forkId
                    
                when (i == 1) $ do
                  addConnection (comp
                                ,(NameId comp 1, vs)
                                ,(forkNId, "in"))
                  addInstance (comp, forkNId, fork, False)

                when (i == c) $ do
                  setInstanceUsed comp forkNId
                  return ()
                  
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  addConnection (comp
                                ,(forkNId, forkOuts !! (i-1))
                                ,(nid, inp))
                  incrementForkedIndex comp vs
                  ret ()
                
              False -> do
                
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  addConnection (comp
                                ,(NameId comp 1, vs)
                                ,(nid, inp))
                  ret ()
                
        | otherwise -> do
            minst <- getNextInstance comp vs
            mayThrow minst (TErr
                            CouldntGetNextInstance
                            Nothing
                            ("Couldn't get next instance for "
                             ++ vs)
                            NoLoc)
            cont1 minst $ \inst@(_,nid',ins',_) -> do
              mayInp <- getInput n ins
              cont1 mayInp $ \inp -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(nid, inp))
                setInstanceUsed comp nid'
              ret ()
            
      FAExpr (FCons (L _ c)) -> do
        let instName = "const" ++ show c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()

special :: Name -> Bool
special n
  = n `elem` ["add","sub","mul"]

getVariablesFromF :: F -> [Name]
getVariablesFromF (F fvars expr) = go expr
  where go :: FExpr -> [Name]
        go (FApp (L _ n) es) = [n] ++ concat (map go es)
        go (FAExpr (FVar x)) = [getVal x]
        go (FAExpr _) = []

getConstantsFromF :: F -> [Int]
getConstantsFromF (F fvars expr) = go expr
  where go :: FExpr -> [Int]
        go (FApp _ es) = concat (map go es)
        go (FAExpr (FCons c)) = [getVal c]
        go (FAExpr _) = []

getInputsFromF :: F -> [Name]
getInputsFromF (F fvars expr) = fmap getVal fvars

getDependencies :: F -> [Name]
getDependencies f =
  let inputs = getInputsFromF f
      vars = getVariablesFromF f
      isInput x = elem x inputs
      inputsOut x
        | isInput x = False
        | otherwise = True
  in filter inputsOut vars

getInputsInBody :: [L Input] -> FExpr -> [L Name]
getInputsInBody inps expr = go expr
  where
    go exp = case exp of
      FApp _ es -> concat (map go es)
      FAExpr (FVar v)
        | elem v inps -> [v]
        | otherwise -> []
      FAExpr (FCons c) -> []
