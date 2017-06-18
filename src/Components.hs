module Components where

----------------------- External Imports

import Prelude hiding (lookup,log)
import Control.Monad.State
import Data.List ((\\), findIndex, nub, nubBy, elem, delete, deleteBy)
import Control.Monad

----------------------- Internal Imports

import Function
import LexerCore
import TransformationMonad
import Types
import Aux

for l f = map f l

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
synth tfunc@(name,_,_,_,clss@(rc,_,_)) = case rc of
  LeftRecursive           -> synthLeftRecursion tfunc clss
  RightRecursive          -> synthRightRecursion tfunc
  MultipleRecursive       -> cannotSynthErr
  NonTerminatingRecursion -> cannotSynthErr
  NonRecursive            -> synthNonRecursiveFunction tfunc
  where cannotSynthErr = throw (TErr
                                CannotSynth
                                Nothing
                                ("Function "
                                  ++ name
                                  ++ " cannot be synthesized.")
                                 NoLoc) >> noRet
  

synthNonRecursiveFunction :: TFunc -> TMM C
synthNonRecursiveFunction tfunc@(name,_,f@(F _ _ returnType),_,_) = do
  let deps   = getDependencies f
      consts = getConstantsFromF f
  cs <- mapM toC (map fst3 (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM (makeInstancesFromConst name) consts
    -- throw err with okdeps
    cont okDeps $ do
      connect tfunc
      insts <- getInstancesFromComponent name
      conns <- getConnections name
      mproced <- procedure tfunc
      cont1 mproced $ \proced -> do
        let c = C
                f
                insts
                (getInputsFromF f)
                ("out", returnType)
                conns
                proced
        ret c

synthLeftRecursion :: TFunc -> FunctionClassification -> TMM C
synthLeftRecursion tfunc clss@(_,tc,_) = case tc of
  InputRecursive       -> synthLeftRecursionWithInputStream tfunc
  OutputRecursive      -> undefined
  OutputInputRecursive -> undefined
  NoRecursiveTypes     -> synthLeftRecursionWithNormalTypes tfunc

synthLeftRecursionWithInputStream :: TFunc -> TMM C
synthLeftRecursionWithInputStream tfunc@(name,_,f@(F _ _ returnType),_,_) = do
  let deps   = filter ((/=name) . fst3) (getDependencies f)
      consts = getConstantsFromF f
  cs <- mapM toC (map fst3 (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM_ (makeInstancesFromConst name) consts
    -- throw err with okdeps
    cont okDeps $ do
      connect tfunc
      insts <- getInstancesFromComponent name
      conns <- getConnections name
      mproced <- procedure tfunc
      cont1 mproced$ \proced-> do
        let c = C
                f
                insts
                (getInputsFromF f)
                ("out", returnType)
                conns
                proced
        ret c

synthLeftRecursionWithNormalTypes :: TFunc -> TMM C
synthLeftRecursionWithNormalTypes tfunc@(name,_,f@(F _ _ returnType),_,_) = do
  let deps   = filter ((/=name) . fst3) (getDependencies f)
      consts = getConstantsFromF f
  cs <- mapM toC (map fst3 (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM_ (makeInstancesFromConst name) consts
    -- throw err with okdeps
    cont okDeps $ do
      connect tfunc
      insts <- getInstancesFromComponent name
      conns <- getConnections name
      mproced <- procedure tfunc
      cont1 mproced$ \proced-> do
        let c = C
                f
                insts
                (getInputsFromF f)
                ("out", returnType)
                conns
                proced
        ret c

synthRightRecursion :: TFunc -> TMM C
synthRightRecursion (_,_,f,_,_) = do
  debug "righttt"
  ret $ C f [] [] ("aaa",Bit NoLoc) [] []

procedure :: TFunc -> TMM CProc
procedure tfunc@(name,_,_,_,(classification,_,_)) = case classification of
  LeftRecursive  -> procedureLeftRecursive tfunc
  RightRecursive -> noRet
  MultipleRecursive -> noRet
  NonTerminatingRecursion -> noRet
  NonRecursive -> procedureNonRecursive tfunc

procedureNonRecursive :: TFunc -> TMM CProc
procedureNonRecursive tfunc@(name,_,f@(F inps fguards rType),_,_) = case fguards of

  NoFGuards expr -> ret []

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
          | i == 1    = IF     (i-1) <$> insideCond i
          | n == i    = ELSE         <$> insideCond i
          | otherwise = ELSEIF (i-1) <$> insideCond i
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
    ret $ getInputs ++ putConds ++ getConds ++ [cond] ++ ifelses

procedureLeftRecursive :: TFunc -> TMM CProc
procedureLeftRecursive tfunc@(_,_,_,_,(_,tc,_)) = case tc of
  InputRecursive       -> procedureLeftRecursiveWithStreamInput tfunc
  OutputRecursive      -> undefined
  OutputInputRecursive -> undefined
  NoRecursiveTypes     -> procedureLeftRecursiveNormal tfunc

takeStreamInps :: [(FVar, FType)] -> [(FVar, FType)]
takeStreamInps = filter isStream

takeNonStreamInps :: [(FVar, FType)] -> [(FVar, FType)]
takeNonStreamInps = filter (not . isStream)

isStream :: (FVar, FType) -> Bool
isStream (_,Stream _) = True
isStream _ = False

isStreamT :: FType -> Bool
isStreamT (Stream _) = True
isStreamT _ = False

numberOfGets :: [(FExpr,FExpr)] -> Name -> Int
numberOfGets ces name = maximum (map (\(c,e) -> max (go 0 c) (go 0 e)) ces)
  where go :: Int -> FExpr -> Int
        go c fex = case fex of
          FApp (L _ "now", _) [FAExpr (_,_,Nat _ nat), FAExpr (FVar (L _ v),_,_)] _
            | v == name -> max c nat
            | otherwise -> c
          FApp _ fexs _ -> maximum (c : map (go 0) fexs)
          _ -> c

numberOfNows = numberOfGets

numberOfRests :: [(FExpr,FExpr)] -> Name -> Int
numberOfRests ces name = maximum (map (\(c,e) -> max (go 0 c) (go 0 e)) ces)
  where go :: Int -> FExpr -> Int
        go c fex = case fex of
          FApp (L _ "rest", _) [FAExpr (_,_,Nat _ nat), FAExpr (FVar (L _ v),_,_)] _
            | v == name -> max c nat
            | otherwise -> c
          FApp _ fexs _ -> maximum (c : map (go 0) fexs)
          _ -> c

procedureLeftRecursiveWithStreamInput :: TFunc -> TMM CProc
procedureLeftRecursiveWithStreamInput tfunc@(name,_,f@(F inps fguards rType),a,_) = case fguards of

  NoFGuards expr -> throw (TErr
                           RecursionWithoutCondition
                           Nothing
                           ("Function "
                            ++ name
                            ++ " is recursive without condition.")
                           NoLoc) >> noRet

  FGuards fgs -> do

    --getInputs ++ putStates ++ while
    --while = getStates ++ -- parei aqui
    let n = length fgs
        nsInps = takeNonStreamInps inps
        sInps  = takeStreamInps inps
        initStates = map (\(inp,t) -> GETINPUT (getVal inp, t)) nsInps
        fifoName i inp = "__fifo__"
                         ++ name
                         ++ "__cond__"
                         ++ show i
                         ++ "__in__"
                         ++ inp
        sFifoName i n inp = "__fifo__"
                            ++ name
                            ++ "__cond__"
                            ++ show i
                            ++ "__in__"
                            ++ "now__"
                            ++ show n ++ "__"
                            ++ inp
        fifoOutName i = "__fifo__"
                        ++ name
                        ++ "__cond__"
                        ++ show i
                        ++ "__out"
    putConds' <- forM [1..n] $ \i -> do
      forM nsInps $ \(inp',t) -> do
        let inp = getVal inp'
        yn <- doesLogicalConnectionExist (fifoName i inp)
        case yn of
          False -> return Nothing
          True  -> return $ Just $ PUT (fifoName i inp, t) inp
    sInitStates <- forM sInps $ \(inp',t) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows = getNumber
          rests = numberOfRests fgs inp
          savs = nows - rests
      debugs $ for [(rests+1)..nows] $ \d -> GETSTREAM d (inp, t)
      return $ for [(rests+1)..nows] $ \d -> GETSTREAM d (inp, t)
    sSwitchStates <- forM sInps $ \(inp',t) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows = getNumber
          rests = numberOfRests fgs inp
          savs = nows - rests
      return $ for [(rests+1)..nows] $ \d -> SWITCH (inp,t) (d - rests) d
    sGetStreams <- forM sInps $ \(inp',t) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows = getNumber
          rests = numberOfRests fgs inp
          savs = nows - rests
      debugs $ for [(savs+1)..nows] $ \d -> GETSTREAM d (inp,t)
      return $ for [(savs+1)..nows] $ \d -> GETSTREAM d (inp,t)
    sPutConds' <- forM [1..n] $ \i -> do
      x <- forM sInps $ \(inp',t) -> do
        let inp = getVal inp'
            getNumber = numberOfGets fgs inp
        forM [1..getNumber] $ \d -> do
          yn <- doesLogicalConnectionExist (sFifoName i d inp)
          case yn of
            False -> return []
            True  -> return [PUTSTREAM d (sFifoName i d inp, t) inp]
      return $ concat $ concat x
    let putConds = concat $ map (map just . filter isJust) putConds'
        sPutConds = concat sPutConds'
        getConds = for [1..n] $ \i -> GET (fifoOutName i, Bit NoLoc)
        cond = COND n name
        ifElseIfElse n i
          | i == 1    = IF     (i-1) <$> insideCond i
          | n == i    = ELSE         <$> insideCond i
          | otherwise = ELSEIF (i-1) <$> insideCond i
        insideCond i = do
          let isRecursiveGuard i fgs = case fgs !! (i-1) of
                (_,FApp (L _ x,_) _ _)
                  | x == name -> True
                  | otherwise -> False
                _ -> False
              isIt = isRecursiveGuard i fgs
              fifoOut inp = "__fifo__"
                            ++ name
                            ++ "__expr__"
                            ++ show i
                            ++ "__in__"
                            ++ inp
              sFifoOut inp k = "__fifo__"
                               ++ name
                               ++ "__expr__"
                               ++ show i
                               ++ "__in__now__"
                               ++ show k
                               ++ "__" ++ inp
              fifoOutExpr = "__fifo__"
                            ++ name
                            ++ "__expr__"
                            ++ show i
                            ++ "__out"
              fifoInRec inp j = "__fifo__"
                                ++ name
                                ++ "__rec"
                                ++ "__expr__"
                                ++ show i
                                ++ "__" ++ show j
                                ++ "__in__"
                                ++ inp
              sFifoInRec inp j k = "__fifo__"
                                  ++ name
                                  ++ "__rec"
                                  ++ "__expr__"
                                  ++ show i
                                  ++ "__" ++ show j
                                  ++ "__in__now__"
                                  ++ show k ++ "__"
                                  ++ inp
              fifoOutRec j = "__fifo__"
                             ++ name
                             ++ "__rec"
                             ++ "__expr__"
                             ++ show i
                             ++ "__" ++ show j
                             ++ "__out"

          case isIt of
            True  -> do
              puts' <- forM [1..a] $ \j -> do
                putInputs' <- forM nsInps $ \(inp',t) -> do
                  let inp = getVal inp'
                  yn <- doesLogicalConnectionExist (fifoInRec inp j)
                  case yn of
                    False -> return Nothing
                    True  -> return $ Just $ PUT (fifoInRec inp j,t) inp
                return $ map just $ filter isJust putInputs'
              sPuts' <- forM [1..a] $ \j -> do
                putInputs' <- forM sInps $ \(inp',t) -> do
                  let inp = getVal inp'
                      getNumber = numberOfGets fgs inp
                  forM [1..getNumber] $ \d -> do
                    yn <- doesLogicalConnectionExist (sFifoInRec inp j d)
                    case yn of
                      False -> return Nothing
                      True  -> return $ Just $ PUTSTREAM d (sFifoInRec inp j d,t) inp
                return $ map just $ filter isJust (concat putInputs')
              let puts = concat puts'
                  sPuts = concat sPuts'
                  gets = concat $ for (zip inps [1..a]) get''
                  get'' ((_,t),j) | isStreamT t = []
                                  | otherwise = [GET (fifoOutRec j, t)]
                  putStates = concat $ for (zip inps [1..a]) put''
                  put'' ((inp,t),j) | isStreamT t = []
                                    | otherwise   = [PUTSTATE (getVal inp) (fifoOutRec j)]
              return $ puts ++ sPuts ++ gets ++ putStates
            False -> do
              putInputs' <- forM nsInps $ \(inp',t) -> do
                let inp = getVal inp'
                yn <- doesLogicalConnectionExist (fifoOut inp)
                case yn of
                  False -> return Nothing
                  True  -> return $ Just $ PUT (fifoOut inp,t) inp
              sPutInputs' <- forM sInps $ \(inp',t) -> do
                let inp = getVal inp'
                    getNumber = numberOfGets fgs inp
                forM [1..getNumber] $ \d -> do
                  yn <- doesLogicalConnectionExist (sFifoOut inp d)
                  case yn of
                    False -> return Nothing
                    True  -> return $ Just $ PUTSTREAM d (sFifoOut inp d,t) inp
              let putInputs = map just $ filter isJust putInputs'
                  sPutInputs = map just $ filter isJust $ concat sPutInputs'
                  getOutput = GET (fifoOutExpr, rType)
                  putOutput = PUTOUTPUT "out" fifoOutExpr
              return $ putInputs ++ sPutInputs ++ [getOutput,putOutput] ++ [DESTROY (map (\(a,b) -> (getVal a, b)) sInps), BREAK]
    ifelses <- forM [1..n] (ifElseIfElse n)
    ret $ initStates ++ concat sInitStates ++ [LOOP (concat sSwitchStates ++ concat sGetStreams ++ putConds ++ sPutConds ++ getConds ++ [cond] ++ ifelses)]

procedureLeftRecursiveNormal :: TFunc -> TMM CProc
procedureLeftRecursiveNormal tfunc@(name,_,f@(F inps fguards rType),a,_) = case fguards of

  NoFGuards expr -> throw (TErr
                           RecursionWithoutCondition
                           Nothing
                           ("Function "
                            ++ name
                            ++ " is recursive without condition.")
                           NoLoc) >> noRet

  FGuards fgs -> do

    --getInputs ++ putStates ++ while
    --while = getStates ++ -- parei aqui
    let n = length fgs
        initStates = map (\(inp,t) -> GETINPUT (getVal inp, t)) inps
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
          | i == 1    = IF     (i-1) <$> insideCond i
          | n == i    = ELSE         <$> insideCond i
          | otherwise = ELSEIF (i-1) <$> insideCond i
        insideCond i = do
          let isRecursiveGuard i fgs = case fgs !! (i-1) of
                (_,FApp (L _ x,_) _ _)
                  | x == name -> True
                  | otherwise -> False
                _ -> False
              isIt = isRecursiveGuard i fgs
              fifoOut inp = "__fifo__"
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
              fifoInRec inp j = "__fifo__"
                                ++ name
                                ++ "__rec"
                                ++ "__expr__"
                                ++ show i
                                ++ "__" ++ show j
                                ++ "__in__"
                                ++ inp
              fifoOutRec j = "__fifo__"
                             ++ name
                             ++ "__rec"
                             ++ "__expr__"
                             ++ show i
                             ++ "__" ++ show j
                             ++ "__out"

          case isIt of
            True  -> do
              puts' <- forM [1..a] $ \j -> do
                putInputs' <- forM inps $ \(inp',t) -> do
                  let inp = getVal inp'
                  yn <- doesLogicalConnectionExist (fifoInRec inp j)
                  case yn of
                    False -> return Nothing
                    True  -> return $ Just $ PUT (fifoInRec inp j,t) inp
                return $ map just $ filter isJust putInputs'
              let puts = concat puts'
                  gets = for (zip inps [1..a]) $ \((_,t),j) -> GET (fifoOutRec j, t)
                  putStates = for (zip inps [1..a]) $ \((inp,t),j) -> PUTSTATE (getVal inp) (fifoOutRec j)
              return $ puts ++ gets ++ putStates
            False -> do
              putInputs' <- forM inps $ \(inp',t) -> do
                let inp = getVal inp'
                yn <- doesLogicalConnectionExist (fifoOut inp)
                case yn of
                  False -> return Nothing
                  True  -> return $ Just $ PUT (fifoOut inp,t) inp
              let putInputs = map just $ filter isJust putInputs'
                  getOutput = GET (fifoOutExpr, rType)
                  putOutput = PUTOUTPUT "out" fifoOutExpr
              return $ putInputs ++ [getOutput,putOutput] ++ [BREAK]
    ifelses <- forM [1..n] (ifElseIfElse n)
    ret $ initStates ++ [LOOP (putConds ++ getConds ++ [cond] ++ ifelses)]
    
makeInstancesFromConst :: CompName -> (FCons, Id, FType) -> TM ()
makeInstancesFromConst compName (const, _  , Nat _ _) = return ()
makeInstancesFromConst compName (const, fid, ftype) = case const of
  FBin (L _ bin) -> do
    let nameInst = "const_bin_" ++ bin
    id <- getIdForInstance compName nameInst --aqui
    let inst = (compName
               , fid
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
               , fid
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
               , fid
               , NameId nameInst id
               , ConstDecI
                 dec
                 ("out", ftype)
               , False)
    addInstance inst

makeInstancesFromDep :: CompName -> (Name, Id, [FType]) -> TMM ()
makeInstancesFromDep compName (dep, fid, ts)
  | isStreamFunc dep = mok
  | special dep = makeSpecialInstance compName (dep, fid, ts)
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
          let inst = (compName, fid, NameId n id, I inps out, False)
          addInstance inst
          ret ()

takeNats :: [FExpr] -> [FType]
takeNats fexs = map toNat (takeWhile isNat fexs)
  where toNat (FAExpr (_,_,t)) = t
        isNat (FAExpr (_,_,Nat _ _)) = True
        isNat _ = False

takeNowArg :: [FExpr] -> (FVar, FType)
takeNowArg = toVar . head . dropWhile isNat
  where toVar (FAExpr (FVar ln,_,t)) = (ln, t)
        isNat (FAExpr (_,_,Nat _ _)) = True
        isNat _ = False

makeSpecialInstance :: CompName -> (Name, Id, [FType]) -> TMM ()
makeSpecialInstance compName (s,fid,ts) = do
  let len = length $ takeWhile isNat ts
      isNat (Nat _ _) = True
      isNat _ = False
      ins = map (("in"++) . show) [1..]
      args = take len ts
  id <- getIdForInstance compName ((put_ s) ++ appendNats args)
  let inst = (compName
             , fid
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
connect (comp, _, F inps fguards ftype, arity,_) = case fguards of
  
  FGuards fguards -> do
    m <- forM (zip [1..] fguards) $ \(i, (fcond, fexpr)) -> do
      ma <- connectOutterCond "cond" i fcond
      mb <- connectOutterCond "expr" i fexpr
      cont [ma, mb] $ ret ()
    cont m $ ret ()
    
  NoFGuards fexpr -> connectOutter fexpr
  
  where

    getOut :: I -> COutput
    getOut i = case i of
      I         _ out   -> out
      ConstBinI _ out   -> out
      ConstHexI _ out   -> out
      ConstDecI _ out   -> out
      SpecialI  _ out _ -> out
      FifoI     _ out   -> out

    getInput :: Int -> I -> TMM CInput
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
        FApp (L _ instName, id) args ftype -> do
          let nats = takeNats args
              pos = appendNats nats
          minst <- getNextInstance comp (put_ instName ++ pos)
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("16Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", ftype)))
            setInstanceUsed comp nid
            mapM_ (connectInner fexp inst) (zip (filter (not . isFExprNat) args) [0..])
            ret ()
        FAExpr (FVar v@(L _ vs), id, t)
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
              cont1 minst $ \inst@(_,_,nid',ins',_) -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(NameId comp 1, ("out", t)))
                setInstanceUsed comp nid'
                ret ()
            
        FAExpr (FCons (FBin (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FBin (L _ c)), id, t) -> do
          let instName = "const_bin_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("18Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FHex (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FHex (L _ c)), id, t) -> do
          let instName = "const_hex_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("19Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FDec (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FDec (L _ c)), id, t) -> do
          let instName = "const_dec_" ++ show c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("20Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(NameId comp 1, ("out", t)))
            setInstanceUsed comp nid

    connectInner :: FExpr -> TInst -> (FExpr,Int) -> TMM ()
    connectInner fexpr inst@(_,_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ instName, id) args' ftype -> do
        let nats = takeNats args'
            pos = appendNats nats
        minst <- getNextInstance comp (put_ instName ++ pos)
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("1Couldn't get next instance for "
                          ++ instName)
                        NoLoc)
        cont1 minst $ \inst'@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            let fifoName = "__fifo__"
                t = snd inp
            fifoId <- getIdForInstance comp fifoName
            let fifo = FifoI ("in", t) ("out", t)
                fifoNId = NameId fifoName fifoId
            addInstance (comp, 1, fifoNId, fifo, True)
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(fifoNId, ("in", t)))
            addConnection (comp
                          ,(fifoNId, ("out", t))
                          ,(nid, inp))
            setInstanceUsed comp nid'
            mapM_ (connectInner fexpr inst') (zip (filter (not . isFExprNat) args') [0..])
            ret ()
              
      FAExpr (FVar v@(L _ vs), id, t)
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
                  addInstance (comp, 1, forkNId, fork, False)

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
            cont1 minst $ \inst@(_,_,nid',ins',_) -> do
              mayInp <- getInput n ins
              cont1 mayInp $ \inp -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(nid, inp))
                setInstanceUsed comp nid'
              ret ()
            
      FAExpr (FCons (FBin (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FBin (L _ c)), id, t) -> do
        let instName = "const_bin_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("3Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()
      FAExpr (FCons (FHex (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FHex (L _ c)), id, t) -> do
        let instName = "const_hex_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("4Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()

      FAExpr (FCons (FDec (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FDec (L _ c)), id, t) -> do
        let instName = "const_dec_" ++ show c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("5Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
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
      addInstance (comp, 1, fifoNId, fifo, True)
      case fexpr of
        FApp (L _ instName, id) [FAExpr (_,_,Nat _ k), FAExpr (FVar (L _ var),_,_)] t'
          | instName == "now" -> do
              let vs = instName ++ "__" ++ show k ++ "__" ++ var
                  fifoName' = "__fifo__"
                              ++ comp
                              ++ "__" ++ txt ++ "__"
                              ++ show i
                              ++ "__in__"
                              ++ vs
                  fifo' = FifoI ("in", t') ("out", t')
                  fifoNId' = NameId fifoName' 1
              ----------
              let newInst = (comp, 1, fifoNId', fifo', True)
              isIt <- isInstanceAdded newInst
              case isIt of
                True -> ok
                False -> do
                  addInstance (comp, 1, fifoNId', fifo', True)
              addLogicalConnection fifoName'
              ----------                                   
              addConnection (comp
                            ,(fifoNId', ("out",t'))
                            ,(fifoNId, ("in",t)))
              ret ()
          | instName == "rest" -> mok
        FApp (L _ instName, id) args ftype
          | instName == comp -> do
              mapM_
                (connectOutterRecursive txt i)
                (zip (filter (not . isFExprNat) args) [1..])
              ret ()
          | otherwise -> do
              let nats = takeNats args
                  pos = appendNats nats
          
              minst <- getNextInstance comp (put_ instName ++ pos)
              mayThrow minst (TErr
                              CouldntGetNextInstance
                              Nothing
                              ("6Couldn't get next instance for "
                               ++ instName)
                              NoLoc)
              cont1 minst $ \inst@(_,_,nid,ins,_) -> do
                addConnection (comp
                              ,(nid, getOut ins)
                              ,(fifoNId, ("in", t)))
                setInstanceUsed comp nid
                mapM_
                  (connectInnerCond txt i fexpr inst)
                  (zip (filter (not . isFExprNat) args) [0..])
                ret ()
        FAExpr (FVar v@(L _ vs), id, t')
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
              let newInst = (comp, 1, fifoNId', fifo', True)
              isIt <- isInstanceAdded newInst
              case isIt of
                True -> ok
                False -> do
                  addInstance (comp, 1, fifoNId', fifo', True)
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
              cont1 minst $ \inst@(_,_,nid',ins',_) -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(fifoNId, ("in", t)))
                setInstanceUsed comp nid'
                ret ()
            
        FAExpr (FCons (FBin (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FBin (L _ c)), id, t) -> do
          let instName = "const_bin_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("8Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FHex (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FHex (L _ c)), id, t) -> do
          let instName = "const_hex_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("9Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FDec (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FDec (L _ c)), id, t) -> do
          let instName = "const_dec_" ++ show c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("10Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid
            
    countFunction :: Name -> Int -> Name -> FExpr -> Int
    countFunction name num var fex = go 0 fex
      where go c fex' = case fex' of
              FApp (L _ n, _) [FAExpr (_,_,Nat _ i), FAExpr (FVar (L _ v),_,_)] _
                | n == name && i == num && v == var -> c + 1
              FApp _ fexs _ -> c + sum (map (go 0) fexs)
              FAExpr _ -> c
              
    connectInnerCond
      :: String -> Int -> FExpr -> TInst -> (FExpr,Int) -> TMM ()
    connectInnerCond
      txt i fexpr inst@(_,_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ instName, id) [FAExpr (_,_,Nat _ k), FAExpr (FVar (L _ var),_,_)] t
        | instName == "now" -> do

            let c = countFunction instName k var fexpr
                vs = instName ++ "__" ++ show k ++ "__" ++ var

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
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1,fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(forkNId, ("in",t)))
                  addInstance (comp, 1, forkNId, fork, False)

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
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1, fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(nid, inp))
                  ret ()
        | instName == "rest" -> mok
      FApp (L _ instName, id) args' t -> do
        let nats = takeNats args'
            pos = appendNats nats
        minst <- getNextInstance comp (put_ instName ++ pos)
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("11Couldn't get next instance for "
                          ++ instName)
                        NoLoc)
        cont1 minst $ \inst'@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            let fifoName = "__fifo__"
                t = snd inp
            fifoId <- getIdForInstance comp fifoName
            let fifo = FifoI ("in", t) ("out", t)
                fifoNId = NameId fifoName fifoId
            addInstance (comp, 1, fifoNId, fifo, True)
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(fifoNId, ("in", t)))
            addConnection (comp
                          ,(fifoNId, ("out", t))
                          ,(nid, inp))
            setInstanceUsed comp nid'
            mapM_ (connectInnerCond txt i fexpr inst') (zip (filter (not . isFExprNat) args') [0..])
            ret ()
              
      FAExpr (FVar v@(L _ vs), id, t)
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
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1,fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(forkNId, ("in",t)))
                  addInstance (comp, 1, forkNId, fork, False)

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
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1, fifoNId', fifo', True)
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
            cont1 minst $ \inst@(_,_,nid',ins',_) -> do
              mayInp <- getInput n ins
              cont1 mayInp $ \inp -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(nid, inp))
                setInstanceUsed comp nid'
              ret ()
            
      FAExpr (FCons (FBin (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FBin (L _ c)), id, t) -> do
        let instName = "const_bin_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("13Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()
      FAExpr (FCons (FHex (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FHex (L _ c)), id, t) -> do
        let instName = "const_hex_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("14Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()

      FAExpr (FCons (FDec (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FDec (L _ c)), id, t) -> do
        let instName = "const_dec_" ++ show c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("15Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()

    connectOutterRecursive :: String -> Int -> (FExpr,Int) -> TMM ()
    connectOutterRecursive txt i (fexpr, n) = do
      let fifoName = "__fifo__"
                     ++ comp
                     ++ "__rec"
                     ++ "__" ++ txt
                     ++ "__" ++ show i
                     ++ "__" ++ show n
                     ++ "__out"
          t = getTypeFromFExpr fexpr --Bit NoLoc
          fifo = FifoI ("in", t) ("out", t)
          fifoNId = NameId fifoName 1
      addInstance (comp, 1, fifoNId, fifo, True)
      case fexpr of
        FApp (L _ instName, id) [FAExpr (_,_,Nat _ k), FAExpr (FVar (L _ var),_,_)] t'
          | instName == "now" -> do
              let vs = instName ++ "__" ++ show k ++ "__" ++ var
                  fifoName' = "__fifo__"
                             ++ comp
                             ++ "__rec"
                             ++ "__" ++ txt
                             ++ "__" ++ show i
                             ++ "__" ++ show n
                             ++ "__in__"
                             ++ vs
                  fifo' = FifoI ("in", t') ("out", t')
                  fifoNId' = NameId fifoName' 1
              ----------
              let newInst = (comp, 1, fifoNId', fifo', True)
              isIt <- isInstanceAdded newInst
              case isIt of
                True -> ok
                False -> do
                  addInstance (comp, 1, fifoNId', fifo', True)
              addLogicalConnection fifoName'
              ----------                                   
              addConnection (comp
                            ,(fifoNId', ("out",t'))
                            ,(fifoNId, ("in",t)))
              ret ()
          | instName == "rest" -> mok
        FApp (L _ instName, id) args ftype -> do
          let nats = takeNats args
              pos = appendNats nats
          minst <- getNextInstance comp (put_ instName ++ pos)
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("6Couldn't get next instance for "
                            ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid
            mapM_
              (connectInnerRecursive txt i n fexpr inst)
              (zip (filter (not . isFExprNat) args) [0..])
            ret ()
        FAExpr (FVar v@(L _ vs), id, t')
          | elem v (map fst inps) -> do
              let fifoName' = "__fifo__"
                             ++ comp
                             ++ "__rec"
                             ++ "__" ++ txt
                             ++ "__" ++ show i
                             ++ "__" ++ show n
                             ++ "__in__"
                             ++ vs
                  fifo' = FifoI ("in", t') ("out", t')
                  fifoNId' = NameId fifoName' 1
              ----------
              let newInst = (comp, 1, fifoNId', fifo', True)
              isIt <- isInstanceAdded newInst
              case isIt of
                True -> ok
                False -> do
                  addInstance (comp, 1, fifoNId', fifo', True)
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
              cont1 minst $ \inst@(_,_,nid',ins',_) -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(fifoNId, ("in", t)))
                setInstanceUsed comp nid'
                ret ()
            
        FAExpr (FCons (FBin (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FBin (L _ c)), id, t) -> do
          let instName = "const_bin_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("8Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FHex (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FHex (L _ c)), id, t) -> do
          let instName = "const_hex_" ++ c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("9Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

        FAExpr (FCons (FDec (L _ c)), id, Nat _ _) -> mok
        FAExpr (FCons (FDec (L _ c)), id, t) -> do
          let instName = "const_dec_" ++ show c
          minst <- getNextInstance comp instName
          mayThrow minst (TErr
                          CouldntGetNextInstance
                          Nothing
                          ("10Couldn't get next instance for "
                           ++ instName)
                          NoLoc)
          cont1 minst $ \inst@(_,_,nid,ins,_) -> do
            addConnection (comp
                          ,(nid, getOut ins)
                          ,(fifoNId, ("in", t)))
            setInstanceUsed comp nid

    connectInnerRecursive
      :: String -> Int -> Int -> FExpr -> TInst -> (FExpr,Int) -> TMM ()
    connectInnerRecursive
      txt i j fexpr inst@(_,_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ instName, id) [FAExpr (_,_,Nat _ k), FAExpr (FVar (L _ var),_,_)] t
        | instName == "now" -> do
            let c = countFunction instName k var fexpr
                vs = instName ++ "__" ++ show k ++ "__" ++ var
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
                                ++ "__rec"
                                ++ "__" ++ txt
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__in__"
                                ++ vs
                      fifo' = FifoI ("in", t) ("out", t)
                      fifoNId' = NameId fifoName' 1
                  -----------
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1, fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(forkNId, ("in",t)))
                  addInstance (comp, 1, forkNId, fork, False)

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
                                ++ "__rec"
                                ++ "__" ++ txt
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__in__"
                                ++ vs
                    fifo' = FifoI ("in", t) ("out", t)
                    fifoNId' = NameId fifoName' 1
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  -----------
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1, fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(nid, inp))
                  ret ()
        | instName == "rest" -> mok
      FApp (L _ instName, id) args' ftype -> do
        let nats = takeNats args'
            pos = appendNats nats
        minst <- getNextInstance comp (put_ instName ++ pos)
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("11Couldn't get next instance for "
                          ++ instName)
                        NoLoc)
        cont1 minst $ \inst'@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            let fifoName = "__fifo__"
                t = snd inp
            fifoId <- getIdForInstance comp fifoName
            let fifo = FifoI ("in", t) ("out", t)
                fifoNId = NameId fifoName fifoId
            addInstance (comp, 1, fifoNId, fifo, True)
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(fifoNId, ("in", t)))
            addConnection (comp
                          ,(fifoNId, ("out", t))
                          ,(nid, inp))
            setInstanceUsed comp nid'
            mapM_ (connectInnerRecursive txt i j fexpr inst') (zip (filter (not . isFExprNat) args') [0..])
            ret ()
              
      FAExpr (FVar v@(L _ vs), id, t)
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
                                ++ "__rec"
                                ++ "__" ++ txt
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__in__"
                                ++ vs
                      fifo' = FifoI ("in", t) ("out", t)
                      fifoNId' = NameId fifoName' 1
                  -----------
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1, fifoNId', fifo', True)
                  addLogicalConnection fifoName'
                  -----------
                  addConnection (comp
                                ,(fifoNId', ("out",t))
                                ,(forkNId, ("in",t)))
                  addInstance (comp, 1, forkNId, fork, False)

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
                                ++ "__rec"
                                ++ "__" ++ txt
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__in__"
                                ++ vs
                    fifo' = FifoI ("in", t) ("out", t)
                    fifoNId' = NameId fifoName' 1
                mayInp <- getInput n ins
                cont1 mayInp $ \inp -> do
                  -----------
                  let newInst = (comp, 1, fifoNId', fifo', True)
                  isIt <- isInstanceAdded newInst
                  case isIt of
                    True -> ok
                    False -> do
                      addInstance (comp, 1, fifoNId', fifo', True)
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
            cont1 minst $ \inst@(_,_,nid',ins',_) -> do
              mayInp <- getInput n ins
              cont1 mayInp $ \inp -> do
                addConnection (comp
                              ,(nid', getOut ins')
                              ,(nid, inp))
                setInstanceUsed comp nid'
              ret ()
            
      FAExpr (FCons (FBin (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FBin (L _ c)), id, t) -> do
        let instName = "const_bin_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("13Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()
      FAExpr (FCons (FHex (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FHex (L _ c)), id, t) -> do
        let instName = "const_hex_" ++ c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("14Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()

      FAExpr (FCons (FDec (L _ c)), id, Nat _ _) -> mok
      FAExpr (FCons (FDec (L _ c)), id, t) -> do
        let instName = "const_dec_" ++ show c
        minst <- getNextInstance comp instName
        mayThrow minst (TErr
                        CouldntGetNextInstance
                        Nothing
                        ("15Couldn't get next instance for "
                         ++ instName)
                        NoLoc)
        cont1 minst $ \inst@(_,_,nid',ins',_) -> do
          mayInp <- getInput n ins
          cont1 mayInp $ \inp -> do
            addConnection (comp
                          ,(nid', getOut ins')
                          ,(nid, inp))
            setInstanceUsed comp nid'
            ret ()
isFExprNat :: FExpr -> Bool
isFExprNat (FAExpr (_,_,Nat _ _)) = True
isFExprNat _ = False

special :: Name -> Bool
special n
  = n `elem` ["add","sub","mul","and","or","not","equ","sli","rep","cat","cons","now","rest"]

getVariablesFromF :: F -> [(Name, Id, [FType])]
getVariablesFromF (F fvars fguards ftype) = case fguards of
  NoFGuards expr -> go expr
  FGuards condsExprs
    -> concat $ map (\(a,b) -> go a ++ go b) condsExprs
  where go (FApp (L _ n, id) es t) =
          [(n, id, map getTypeFromFExpr es ++ [t])] ++ concat (map go es)
        go (FAExpr (FVar x, id, t)) = [(getVal x, id, [t])]
        go (FAExpr _) = []

getConstantsFromF :: F -> [(FCons, Id, FType)]
getConstantsFromF (F fvars fguards ftype) = case fguards of
  NoFGuards expr -> go expr
  FGuards condsExprs
    -> concat $ map (\(a,b) -> go a ++ go b) condsExprs
  where go (FApp _ es t) = concat (map go es)
        go (FAExpr (FCons c, id, t)) = [(c, id, t)]
        go (FAExpr _) = []

getInputsFromF :: F -> [CInput]
getInputsFromF (F fvars expr ftype) = map (\(x,y) -> (getVal x,y)) fvars

getDependencies :: F -> [(Name, Id, [FType])]
getDependencies f =
  let inputs = getInputsFromF f
      vars   = getVariablesFromF f
      isInput x = elem (fst3 x) (map fst inputs)
      inputsOut x
        | isInput x = False
        | otherwise = True
  in filter inputsOut vars

getInputsInBody :: [(FVar, FType)] -> FExpr -> [L Name]
getInputsInBody inps expr = go expr
  where
    go exp = case exp of
      FApp _ es _ -> concat (map go es)
      FAExpr (FVar v, _, _)
        | elem v (map fst inps) -> [v]
        | otherwise   -> []
      FAExpr (FCons c, _, _) -> []
