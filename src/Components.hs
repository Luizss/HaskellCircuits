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
synth tfunc@(name,_,f@(F _ _ returnType),_) = do
  let deps   = getDependencies f
      consts = getConstantsFromF f
  cs <- mapM toC (map fst (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM_ (makeInstancesFromConst name) consts
    -- throw err with okdeps
    cont okDeps $ do
      connect tfunc
      insts <- getInstancesFromComponent name
      conns <- getConnections name
      proced <- procedure tfunc
      let c = C
              f
              insts
              (getInputsFromF f)
              ("out", returnType)
              conns
              proced
      ret c

for l f = map f l

procedure :: TFunc -> TM Proc
procedure tfunc@(name,_,f@(F inps fguards rType),_) = case fguards of

  NoFGuards expr -> return []

  FGuards fgs -> do
    
    let n = length fgs
        getInputs = map (\(inp,t) -> GETINPUT (getVal inp, t)) inps
        fifoName i inp = "__fifo__"
                         ++ name
                         ++ "__cond__"
                         ++ show i
                         ++ "__in__"
                         ++ inp
        fifoOutName i = "__fifo__"
                        ++ name
                        ++ "__cond__"
                        ++ show i
                        ++ "__out"
    putConds' <- forM [1..n] $ \i -> do
      forM inps $ \(inp',t) -> do
        let inp = getVal inp'
        yn <- doesLogicalConnectionExist (fifoName i inp)
        case yn of
          False -> return Nothing
          True  -> return $ Just $ PUT (fifoName i inp, t) inp
    let putConds = concat $ map (map just . filter isJust) putConds'
        getConds = for [1..n] $ \i -> GET (fifoOutName i, Bit NoLoc)
        cond = COND n name
        ifElseIfElse n i
          | i == 1    = IF     i <$> insideCond i
          | n == i    = ELSE     <$> insideCond i
          | otherwise = ELSEIF i <$> insideCond i
        insideCond i = do
          let fifoOut inp = "__fifo__"
                            ++ name
                            ++ "__expr__"
                            ++ show i
                            ++ "__in__"
                            ++ inp
              fifoOutExpr = "__fifo__"
                            ++ name
                            ++ "__expr__"
                            ++ show i
                            ++ "__out"
          putInputs' <- forM inps $ \(inp',t) -> do
            let inp = getVal inp'
            yn <- doesLogicalConnectionExist (fifoOut inp)
            case yn of
              False -> return Nothing
              True  -> return $ Just $ PUT (fifoOut inp,t) inp
          let putInputs = map just $ filter isJust putInputs'
              getOutput = GET (fifoOutExpr, rType)
              putOutput = PUTOUTPUT "out" fifoOutExpr
          return $ putInputs ++ [getOutput,putOutput]
    ifelses <- forM [1..n] (ifElseIfElse n)
    return $ getInputs ++ putConds ++ getConds ++ [cond] ++ ifelses

makeInstancesFromConst :: CompName -> (FCons, FType) -> TM ()
makeInstancesFromConst compName (const, Nat _ _) = return ()
makeInstancesFromConst compName (const, ftype) = case const of
  FBin (L _ bin) -> do
    let nameInst = "const_bin_" ++ bin
    id <- getIdForInstance compName nameInst
    let inst = (compName
               , NameId nameInst id
               , ConstBinI
                 bin
                 ("out", ftype)
               , False)
    addInstance inst
  FHex (L _ hex) -> do
    let nameInst = "const_hex_" ++ hex
    id <- getIdForInstance compName nameInst
    let inst = (compName
               , NameId nameInst id
               , ConstHexI
                 hex
                 ("out", ftype)
               , False)
    addInstance inst
  FDec (L _ dec) -> do
    let nameInst = "const_dec_" ++ show dec
    id <- getIdForInstance compName nameInst
    let inst = (compName
               , NameId nameInst id
               , ConstDecI
                 dec
                 ("out", ftype)
               , False)
    addInstance inst

makeInstancesFromDep :: CompName -> (String, [FType]) -> TMM ()
makeInstancesFromDep compName (dep, ts)
  | special dep = makeSpecialInstance compName (dep, ts)
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

takeNats :: [FExpr] -> [FType]
takeNats fexs = map toNat (takeWhile isNat fexs)
  where toNat (FAExpr (_,t)) = t
        isNat (FAExpr (_,Nat _ _)) = True
        isNat _ = False
             
makeSpecialInstance :: CompName -> (Name, [FType]) -> TMM ()
makeSpecialInstance compName (s,ts) = do
  id <- getIdForInstance compName (put_ s)
  let len = length $ takeWhile isNat ts
      isNat (Nat _ _) = True
      isNat _ = False
      ins = map (("in"++) .show) [1..]
      args = take len ts
      inst = (compName
             , NameId ((put_ s) ++ appendNats args) id
             , SpecialI (zip ins (drop len (init ts))) ("out", last ts) (map toInt args)
             , False)
  addInstance inst
  ret ()

put_ "and" = "and_"
put_ "or"  = "or_"
put_ "not" = "not_"
put_ x = x

appendNats :: [FType] -> String
appendNats [] = []
appendNats xs = (++"_") $ concat $ map (("_"++) . show. toInt) xs

toInt (Nat _ i) = i

connect :: TFunc -> TMM ()
connect (comp, _, F inps fguards ftype, arity) = case fguards of
  
  FGuards fguards -> do
    m <- forM (zip [1..] fguards) $ \(i, (fcond, fexpr)) -> do
      ma <- connectOutterCond "cond" i fcond
      mb <- connectOutterCond "expr" i fexpr
      cont [ma, mb] $ ret ()
    cont m $ ret ()
    
  NoFGuards fexpr -> connectOutter fexpr
  
  where

    getOut :: I -> Output
    getOut i = case i of
      I         _ out   -> out
      ConstBinI _ out   -> out
      ConstHexI _ out   -> out
      ConstDecI _ out   -> out
      SpecialI  _ out _ -> out
      FifoI     _ out   -> out

    getInput :: Int -> I -> TMM Input
    getInput i inst = case inst of
      I inps _
        | i < length inps -> ret (inps !! i)
        | otherwise -> error'
      ConstBinI inp _
        | otherwise -> error''
      ConstHexI inp _
        | otherwise -> error''
      ConstDecI inp _
        | otherwise -> error''
      SpecialI inps _ _
        | i < length inps -> ret $ inps !! i
        | otherwise -> error'
      FifoI inp _
        | i == 0 -> ret inp
        | otherwise -> error'
      where error'  = throw (TErr
                            WrongInstanceNumberInput
                            Nothing
                            "Wrong number of inputs of instance"
                            NoLoc) >> noRet
            error'' = throw (TErr
                            ConstantsHaveNoInputs
                            Nothing
                            "Constants have no input"
                            NoLoc) >> noRet
      
    connectOutter :: FExpr -> TMM ()
    connectOutter fexp = do
      case fexp of
        FApp (L _ instName) args ftype -> do
          let nats = takeNats args
              pos = appendNats nats
          minst <- getNextInstance comp (put_ instName ++ pos)
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("16Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", ftype)))
            setInstanceUsed comp nid
            mapM_ (connectInner fexp inst) (zip (filter (not . isFExprNat) args) [0..])
            ret ()
        FAExpr (FVar v@(L _ vs), t)
          | elem v (map fst inps) -> 
              addConnection (comp
                            ,(NameId comp 1, (vs,t))
                            ,(NameId comp 1, ("out",t))) >> ret ()
          | otherwise -> do
              minst <- getNextInstance comp vs
              mayThrow minst (TErr
                              CouldntGetNextInstance
                              Nothing
                              ("17Couldn't get next instance for "
                               ++ vs)
                              NoLoc)
              cont1 minst $ \inst@(_,nid',ins',_) -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(NameId comp 1, ("out", t)))
                setInstanceUsed comp nid'
                ret ()
            
        FAExpr (FCons (FBin (L _ c)), Nat _ _) -> mok
        FAExpr (FCons (FBin (L _ c)), t) -> do
          let instName = "const_bin_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("18Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FHex (L _ c)), Nat _ _) -> mok
        FAExpr (FCons (FHex (L _ c)), t) -> do
          let instName = "const_hex_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("19Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FDec (L _ c)), Nat _ _) -> mok
        FAExpr (FCons (FDec (L _ c)), t) -> do
          let instName = "const_dec_" ++ show c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("20Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", t)))
            setInstanceUsed comp nid

    connectInner :: FExpr -> TInst -> (FExpr,Int) -> TMM ()
    connectInner fexpr inst@(_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ instName) args' ftype -> do
        let nats = takeNats args'
            pos = appendNats nats
        minst <- getNextInstance comp (put_ instName ++ pos)
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("1Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst'@(_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            let fifoName = "__fifo__"
                t = snd inp
            fifoId <- getIdForInstance comp fifoName
            let fifo = FifoI ("in", t) ("out", t)
                fifoNId = NameId fifoName fifoId
            addInstance (comp, fifoNId, fifo, True)
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(fifoNId, ("in", t)))
            addConnection (comp
                          ,(fifoNId, ("out", t))
                          ,(nid, inp))
            setInstanceUsed comp nid'
            mapM_ (connectInner fexpr inst') (zip (filter (not . isFExprNat) args') [0..])
            ret ()
              
      FAExpr (FVar v@(L _ vs), t)
        | elem v (map fst inps) -> do

            let c = count v (getInputsInBody inps fexpr)

            case c > 1 of

              True -> do

                i' <- getForkedIndex comp vs

                let forkName = "__fork" ++ show c ++ "__"
                forkId <- getIdForInstance comp forkName
                let forkOuts = map ((\x -> (x,t)) . ("out"++) . show) [1..c]
                    fork     = ForkI c ("in", t) forkOuts
                    forkNId  = NameId forkName forkId
                    
                when (i' == 1) $ do
                  addConnection (comp
                                ,(NameId comp 1, (vs,t))
                                ,(forkNId, ("in",t)))
                  addInstance (comp, forkNId, fork, False)

                when (i' == c) $ do
                  setInstanceUsed comp forkNId
                  return ()
                  
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  addConnection (comp
                                ,(forkNId, forkOuts !! (i'-1))
                                ,(nid, inp))
                  incrementForkedIndex comp vs
                  ret ()
                
              False -> do
                
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  addConnection (comp
                                ,(NameId comp 1, (vs,t))
                                ,(nid, inp))
                  ret ()
                
        | otherwise -> do
            minst <- getNextInstance comp vs
            mayThrow minst (TErr
                            CouldntGetNextInstance
                            Nothing
                            ("2Couldn't get next instance for "
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
            
      FAExpr (FCons (FBin (L _ c)), Nat _ _) -> mok
      FAExpr (FCons (FBin (L _ c)), t) -> do
        let instName = "const_bin_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("3Couldn't get next instance for "
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
      FAExpr (FCons (FHex (L _ c)), Nat _ _) -> mok
      FAExpr (FCons (FHex (L _ c)), t) -> do
        let instName = "const_hex_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("4Couldn't get next instance for "
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

      FAExpr (FCons (FDec (L _ c)), Nat _ _) -> mok
      FAExpr (FCons (FDec (L _ c)), t) -> do
        let instName = "const_dec_" ++ show c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("5Couldn't get next instance for "
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

    connectOutterCond :: String -> Int -> FExpr -> TMM ()
    connectOutterCond txt i fexpr = do
      let fifoName = "__fifo__"
                     ++ comp
                     ++ "__" ++ txt ++"__"
                     ++ show i
                     ++ "__out"
          t = getTypeFromFExpr fexpr --Bit NoLoc
          fifo = FifoI ("in", t) ("out", t)
          fifoNId = NameId fifoName 1
      addInstance (comp, fifoNId, fifo, True)
      case fexpr of
        FApp (L _ instName) args ftype -> do
          let nats = takeNats args
              pos = appendNats nats
          minst <- getNextInstance comp (put_ instName ++ pos)
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("6Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid
            mapM_ (connectInnerCond txt i fexpr inst) (zip (filter (not . isFExprNat) args) [0..])
            ret ()
        FAExpr (FVar v@(L _ vs), t')
          | elem v (map fst inps) -> do
              let fifoName' = "__fifo__"
                             ++ comp
                             ++ "__" ++ txt ++ "__"
                             ++ show i
                             ++ "__in__"
                             ++ vs
                  fifo' = FifoI ("in", t') ("out", t')
                  fifoNId' = NameId fifoName' 1
              ----------
              let newInst = (comp, fifoNId', fifo', True)
              isIt <- isInstanceAdded newInst
              case isIt of
                True -> ok
                False -> do
                  addInstance (comp, fifoNId', fifo', True)
              addLogicalConnection fifoName'
              ----------                                   
              addConnection (comp
                            ,(fifoNId', ("out",t'))
                            ,(fifoNId, ("in",t)))
              ret ()
          | otherwise -> do
              minst <- getNextInstance comp vs
              mayThrow minst (TErr
                              CouldntGetNextInstance
                              Nothing
                              ("7Couldn't get next instance for "
                               ++ vs)
                              NoLoc)
              cont1 minst $ \inst@(_,nid',ins',_) -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(fifoNId, ("in", t)))
                setInstanceUsed comp nid'
                ret ()
            
        FAExpr (FCons (FBin (L _ c)), Nat _ _) -> mok
        FAExpr (FCons (FBin (L _ c)), t) -> do
          let instName = "const_bin_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("8Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FHex (L _ c)), Nat _ _) -> mok
        FAExpr (FCons (FHex (L _ c)), t) -> do
          let instName = "const_hex_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("9Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FDec (L _ c)), Nat _ _) -> mok
        FAExpr (FCons (FDec (L _ c)), t) -> do
          let instName = "const_dec_" ++ show c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("10Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid
            
    connectInnerCond
      :: String -> Int -> FExpr -> TInst -> (FExpr,Int) -> TMM ()
    connectInnerCond
      txt i fexpr inst@(_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ instName) args' ftype -> do
        let nats = takeNats args'
            pos = appendNats nats
        minst <- getNextInstance comp (put_ instName ++ pos)
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("11Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst'@(_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            let fifoName = "__fifo__"
                t = snd inp
            fifoId <- getIdForInstance comp fifoName
            let fifo = FifoI ("in", t) ("out", t)
                fifoNId = NameId fifoName fifoId
            addInstance (comp, fifoNId, fifo, True)
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(fifoNId, ("in", t)))
            addConnection (comp
                          ,(fifoNId, ("out", t))
                          ,(nid, inp))
            setInstanceUsed comp nid'
            mapM_ (connectInnerCond txt i fexpr inst') (zip (filter (not . isFExprNat) args') [0..])
            ret ()
              
      FAExpr (FVar v@(L _ vs), t)
        | elem v (map fst inps) -> do

            let c = count v (getInputsInBody inps fexpr)

            case c > 1 of

              True -> do

                i' <- getForkedIndex comp vs

                let forkName = "__fork" ++ show c ++ "__"
                forkId <- getIdForInstance comp forkName
                let forkOuts = map ((\x -> (x,t)) . ("out"++) . show) [1..c]
                    fork     = ForkI c ("in", t) forkOuts
                    forkNId  = NameId forkName forkId
                    
                when (i' == 1) $ do

                  let fifoName' = "__fifo__"
                                ++ comp
                                ++ "__" ++ txt ++ "__"
                                ++ show i
                                ++ "__in__"
                                ++ vs
                      fifo' = FifoI ("in", t) ("out", t)
                      fifoNId' = NameId fifoName' 1
                  -----------
                  let newInst = (comp, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(forkNId, ("in",t)))
                  addInstance (comp, forkNId, fork, False)

                when (i' == c) $ do
                  setInstanceUsed comp forkNId
                  return ()
                  
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  addConnection (comp
                                ,(forkNId, forkOuts !! (i'-1))
                                ,(nid, inp))
                  incrementForkedIndex comp vs
                  ret ()
                
              False -> do

                let fifoName' = "__fifo__"
                                ++ comp
                                ++ "__" ++ txt ++ "__"
                                ++ show i
                                ++ "__in__"
                                ++ vs
                    fifo' = FifoI ("in", t) ("out", t)
                    fifoNId' = NameId fifoName' 1
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  -----------
                  let newInst = (comp, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(nid, inp))
                  ret ()
                
        | otherwise -> do
            minst <- getNextInstance comp vs
            mayThrow minst (TErr
                            CouldntGetNextInstance
                            Nothing
                            ("12Couldn't get next instance for "
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
            
      FAExpr (FCons (FBin (L _ c)), Nat _ _) -> mok
      FAExpr (FCons (FBin (L _ c)), t) -> do
        let instName = "const_bin_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("13Couldn't get next instance for "
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
      FAExpr (FCons (FHex (L _ c)), Nat _ _) -> mok
      FAExpr (FCons (FHex (L _ c)), t) -> do
        let instName = "const_hex_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("14Couldn't get next instance for "
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

      FAExpr (FCons (FDec (L _ c)), Nat _ _) -> mok
      FAExpr (FCons (FDec (L _ c)), t) -> do
        let instName = "const_dec_" ++ show c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("15Couldn't get next instance for "
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

isFExprNat :: FExpr -> Bool
isFExprNat (FAExpr (_,Nat _ _)) = True
isFExprNat _ = False

special :: Name -> Bool
special n
  = n `elem` ["add","sub","mul","and","or","not","equ","sli","rep","cat"]

getTypeFromFExpr :: FExpr -> FType
getTypeFromFExpr (FApp _ _ t) = t
getTypeFromFExpr (FAExpr (_, t)) = t

getVariablesFromF :: F -> [(Name, [FType])]
getVariablesFromF (F fvars fguards ftype) = case fguards of
  NoFGuards expr -> go expr
  FGuards condsExprs
    -> concat $ map (\(a,b) -> go a ++ go b) condsExprs
  where go (FApp (L _ n) es t) =
          [(n, map getTypeFromFExpr es ++ [t])] ++ concat (map go es)
        go (FAExpr (FVar x, t)) = [(getVal x, [t])]
        go (FAExpr _) = []

getConstantsFromF :: F -> [(FCons, FType)]
getConstantsFromF (F fvars fguards ftype) = case fguards of
  NoFGuards expr -> go expr
  FGuards condsExprs
    -> concat $ map (\(a,b) -> go a ++ go b) condsExprs
  where go (FApp _ es t) = concat (map go es)
        go (FAExpr (FCons c, t)) = [(c, t)]
        go (FAExpr _) = []

getInputsFromF :: F -> [Input]
getInputsFromF (F fvars expr ftype) = map (\(x,y) -> (getVal x,y)) fvars

getDependencies :: F -> [(Name, [FType])]
getDependencies f =
  let inputs = getInputsFromF f
      vars   = getVariablesFromF f
      isInput x = elem (fst x) (map fst inputs)
      inputsOut x
        | isInput x = False
        | otherwise = True
  in filter inputsOut vars

getInputsInBody :: [(FVar, FType)] -> FExpr -> [L Name]
getInputsInBody inps expr = go expr
  where
    go exp = case exp of
      FApp _ es _ -> concat (map go es)
      FAExpr (FVar v, _)
        | elem v (map fst inps) -> [v]
        | otherwise   -> []
      FAExpr (FCons c, _) -> []
