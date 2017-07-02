module Components where

----------------------- External Imports

import Prelude as P hiding (lookup,log) 
import Control.Monad.State
import Data.List ((\\), findIndex, nub, nubBy, elem, delete, deleteBy, elemIndex)
import Control.Monad

----------------------- Internal Imports

import Function
import Lexer
import TransformationMonad
import Types
import Aux

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
      debug $ "NAME " ++name
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
synth tfunc@(name,_,_,_,clss@(rc,_,_),_) = case rc of
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
synthNonRecursiveFunction tfunc@(name,_,f@(F _ _ returnType),_,_,_) = do
  let deps   = getDependencies f
      consts = getConstantsFromF f
      constStreams = getConstantStreamsFromF f
  cs <- mapM toC (map fst3 (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM (makeInstancesFromConst name) consts
    debug "44444"
    debugs constStreams
    mapM (makeInstancesFromConstStreams name) constStreams
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

makename :: [FExpr] -> Name
makename = concat . map f
  where f :: FExpr -> Name
        f (FAExpr (FCons fcons, _, t)) = case fcons of
          FBin (L _ b) -> "_bin" ++ b
          FHex (L _ h) -> "_hex" ++ h
          FDec (L _ d) -> "_dec" ++ show d
          FForeverWait -> ""
        f _ = error "makename"

makename' :: [FCons] -> Name
makename' = concat . map f
  where f :: FCons -> Name
        f fcons = case fcons of
          FBin (L _ b) -> "_bin" ++ b
          FHex (L _ h) -> "_hex" ++ h
          FDec (L _ d) -> "_dec" ++ show d
          FForeverWait -> ""

constantsWithForeverWait :: [FExpr] -> Bool
constantsWithForeverWait fexs = and (map isCons fexs) && isFW (last fexs)
  where isCons (FAExpr (FCons _,_,_)) = True
        isCons _ = False
        isFW (FAExpr (FCons FForeverWait,_,_)) = True
        isFW _ = False

synthLeftRecursion :: TFunc -> FunctionClassification -> TMM C
synthLeftRecursion tfunc clss@(_,tc,_) = case tc of
  InputRecursive       -> synthLeftRecursionWithInputStream tfunc
  OutputRecursive      -> synthLeftRecursionWithInputStream tfunc
  OutputInputRecursive -> synthLeftRecursionWithInputStream tfunc
  NoRecursiveTypes     -> synthLeftRecursionWithNormalTypes tfunc

synthLeftRecursionWithInputStream :: TFunc -> TMM C
synthLeftRecursionWithInputStream tfunc@(name,_,f@(F _ _ returnType),_,_,_) = do
  let deps   = filter ((/=name) . fst3) (getDependencies f)
      consts = getConstantsFromF f
      constStreams = getConstantStreamsFromF f
  cs <- mapM toC (map fst3 (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM_ (makeInstancesFromConst name) consts
    debug "11111"
    debugs constStreams
    mapM (makeInstancesFromConstStreams name) constStreams
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
synthLeftRecursionWithNormalTypes tfunc@(name,_,f@(F _ _ returnType),_,_,_) = do
  let deps   = filter ((/=name) . fst3) (getDependencies f)
      consts = getConstantsFromF f
      constStreams = getConstantStreamsFromF f
  cs <- mapM toC (map fst3 (nub deps))
  cont cs $ do
    okDeps <- mapM (makeInstancesFromDep name) deps
    mapM_ (makeInstancesFromConst name) consts
    debug "22222"
    debugs constStreams
    mapM (makeInstancesFromConstStreams name) constStreams
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
synthRightRecursion (_,_,f,_,_,_) = do
  debug "righttt"
  ret $ C f [] [] ("aaa",Bit NoLoc) [] []

procedure :: TFunc -> TMM CProc
procedure tfunc@(name,_,_,_,(classification,_,_),_) = case classification of
  LeftRecursive  -> procedureLeftRecursive tfunc
  RightRecursive -> noRet
  MultipleRecursive -> noRet
  NonTerminatingRecursion -> noRet
  NonRecursive -> procedureNonRecursive tfunc

procedureNonRecursive :: TFunc -> TMM CProc
procedureNonRecursive tfunc@(name,_,f@(F inps fguards rType),a,_,_) = case fguards of

  NoFGuards (FAExpr (FVar (L _ vs), _, t)) -> do
    yn <- doesLogicalConnectionExist (name ++ "__" ++ vs)
    ret $ case yn of
      False -> []
      True  -> [GETINPUT (vs, t)
               ,PUTOUTPUT "out" vs]

  NoFGuards _ -> ret []
  
  FGuards fgs -> do
    
    let isId i = case fgs !! (i-1) of
          (_, FAExpr (FVar ln,_,t))
            | elem ln (map fst inps) -> Just (getVal ln,t, just (elemIndex ln (map fst inps)))
          _ -> Nothing
        typeOfExpr i = case fgs !! (i-1) of
          (_, expr) -> getTypeFromFExpr expr
        n = length fgs
        nsInps = takeNonStreamInps inps
        sInps  = takeStreamInps inps
        getInputs = map (\(inp,t) -> GETINPUT (getVal inp, t)) nsInps
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
    sInitStates <- forM (filter (\((_,t),_) -> isStreamT t) (zip inps [1..a])) $ \((inp',t),h) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows  = getNumber
          rests = numberOfRests fgs inp
          savs  = nows - rests
          getstreams
            | nows == 0 = []
            | otherwise = for [(rests+1)..nows] $ \d -> GETSTREAMSAFE(rests+1,nows) d (inp, t)
      return getstreams
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
    putConds' <- forM [1..n] $ \i -> do
      forM inps $ \(inp',t) -> do
        let inp = getVal inp'
        yn <- doesLogicalConnectionExist (fifoName i inp)
        return $ case yn of
          False -> []
          True
            | isStreamT t -> [] ---h
            | otherwise   -> [PUT (fifoName i inp, t) inp]
    getConds' <- forM [1..n] $ \i -> do
      yn <- doesLogicalOutputExist (fifoOutName i)
      case yn of
        False -> return [GET (fifoOutName i, Bit NoLoc)]
        True  -> do
          (lo,lot,isNow) <- getLogicalOutput (fifoOutName i)
          return $ case (isStreamT lot, isNow) of
            (False,_) -> [PUT (fifoOutName i, Bit NoLoc) lo
                         ,GET (fifoOutName i, Bit NoLoc)]
            (True,Just z) -> [PUTSTREAM z (fifoOutName i, Bit NoLoc) lo
                             ,GET (fifoOutName i, Bit NoLoc)]-- isnow
            (True,Nothing) -> []
    let putConds = concat $ concat putConds'
        sPutConds = concat sPutConds'
        getConds = concat getConds'
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
            return $ case yn of
              False -> []
              True
                | isStreamT t -> [PCOPY (numberOfGets fgs inp) t (fifoOut inp) inp]
                | otherwise   -> [PUT (fifoOut inp,t) inp]
          let putInputs = concat putInputs'
              outputs = case isId i of
                Nothing -> case isStreamT (typeOfExpr i) of
                  True  -> [COPY (typeOfExpr i) "out" fifoOutExpr]
                  False -> [GET (fifoOutExpr, rType)
                           ,PUTOUTPUT "out" fifoOutExpr]
                Just (v,t,a')
                  | isStreamT t && immediateOutput name a' v fgs -> [COPY t "out" (v ++ "__save")]
                  | isStreamT t && increasingOutput name a' v fgs -> [COPYV t "out" (v ++ "__savev")]
                  | isStreamT t && functionOutput name v fgs -> [COPYV t "out" (v ++ "__savev")]
                  | isStreamT t -> [PCOPY (numberOfGets fgs v) t "out" v]
                  | otherwise -> [PUTOUTPUT "out" v]
              destroys = map destroy $ filter (\((_,t),_) -> isStreamT t) (zip inps [1..a])
              destroy ((s',ty),a') =
                let s = getVal s'
                in if isVector name s a' fgs
                   then DESTROYV (s, ty)
                   else DESTROY (numberOfGets fgs s) (s,ty)
          return $ putInputs ++ outputs ++ destroys
    ifelses <- forM [1..n] (ifElseIfElse n)
    ret $ getInputs ++ concat sInitStates ++ putConds ++ sPutConds ++ getConds ++ [cond] ++ ifelses

procedureLeftRecursive :: TFunc -> TMM CProc
procedureLeftRecursive tfunc@(_,_,_,_,(_,tc,_),_) = case tc of
  InputRecursive       -> procedureLeftRecursiveWithStreamInput tfunc
  OutputRecursive      -> procedureLeftRecursiveWithStreamInput tfunc
  OutputInputRecursive -> procedureLeftRecursiveWithStreamInput tfunc
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
procedureLeftRecursiveWithStreamInput tfunc@(name,_,f@(F inps fguards rType),a,_,_) = case fguards of

  NoFGuards expr -> throw (TErr
                           RecursionWithoutCondition
                           Nothing
                           ("Function "
                            ++ name
                            ++ " is recursive without condition.")
                           NoLoc) >> noRet

  FGuards fgs -> do

    v <- getLogicalOutputs
    debugs v
    --getInputs ++ putStates ++ while
    --while = getStates ++ -- parei aqui
    let isRecursiveGuard i fgs = case fgs !! (i-1) of
          (_, FApp (L _ x,_) _ _)
            | x == name -> (True, Nothing)
          (_, FAExpr (FVar ln,_,t))
            | elem ln (map fst inps) -> (False, Just (getVal ln, t, just (elemIndex ln (map fst inps))))
          _ -> (False, Nothing)
        typeOfExpr i = case fgs !! (i-1) of
          (_, expr) -> getTypeFromFExpr expr
        n = length fgs
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
      forM inps $ \(inp',t) -> do
        let inp = getVal inp'
        yn <- doesLogicalConnectionExist (fifoName i inp)
        return $ case yn of
          False -> []
          True
            | isStreamT t -> [COPYV t (fifoName i inp) (inp++"__savev")] ---m
            | otherwise   -> [PUT (fifoName i inp, t) inp]
    sInitStates <- forM (filter (\((_,t),_) -> isStreamT t) (zip inps [1..a])) $ \((inp',t),h) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows  = getNumber
          rests = numberOfRests fgs inp
          savs  = nows - rests
          savestreams
            | isVector name inp h fgs = [SAVEV (inp,t)]
            | immediateOutput name h inp fgs = [SAVE (inp,t)]
            | otherwise = []
          -- savestreams = for [1..n] sstreams
          -- sstreams i | fst (isRecursiveGuard i fgs) = case transitionExpression inp' fgs i h of
          --                (_,ConsRTransition d)  -> 
          --                (_,RestTransition)     -> saveornotsave h name (inp,t) fgs
          --                (_,ConsTransition d)   -> [SAVEV (inp,t)]
          --                (_,IdTransition)       -> saveornotsave h name (inp,t) fgs
          --                (_,FunctionTransition) -> [SAVEV (inp,t)] ---m
          --            | otherwise = []
          getstreams
            | nows == 0 = [BLOB 555]
            | isVector name inp h fgs = [BLOB 666] --for [(rests+1)..nows] $ \d -> GETSTREAMV d (inp, t) ---m
            | otherwise = for [(rests+1)..nows] $ \d -> GETSTREAMSAFE (rests+1,nows) d (inp, t)
      return $ savestreams ++ getstreams
    sSwitchStates <- forM sInps $ \(inp',t) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows = getNumber
          rests = numberOfRests fgs inp
          savs = nows - rests
      return $ concat $ for [(rests+1)..nows] $ \d -> if rests == 0
                                                      then [] ----m
                                                      else [SWITCH (inp,t) (d - rests) d]
    sGetStreams <- forM (filter (\((_,t),_) -> isStreamT t) (zip inps [1..a])) $ \((inp',t),h) -> do
      let inp = getVal inp'
          getNumber = numberOfGets fgs inp
          nows = getNumber
          rests = numberOfRests fgs inp
          savs = nows - rests
      return $ case nows of
        0 -> []
        _ | isVector name inp h fgs -> for [1..nows] $ \d -> GETSTREAMV d (inp,t) ---m
          | otherwise -> for [(savs+1)..nows] $ \d -> GETSTREAMSAFE (savs+1, nows) d (inp,t)
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
    getConds' <- forM [1..n] $ \i -> do
      yn <- doesLogicalOutputExist (fifoOutName i)
      case yn of
        False -> return [GET (fifoOutName i, Bit NoLoc)]
        True -> do
          (lo,lot,isNow) <- getLogicalOutput (fifoOutName i)
          return $ case (isStreamT lot,isNow) of
            (False,_)  -> [PUT (fifoOutName i, Bit NoLoc) lo
                          ,GET (fifoOutName i, Bit NoLoc)]
            (True,Just z) -> [PUTSTREAM z (fifoOutName i, Bit NoLoc) lo
                             ,GET (fifoOutName i, Bit NoLoc)]
            (True,Nothing) -> [] -- err?
    let putConds  = concat $ concat putConds'
        sPutConds = concat sPutConds'
        getConds  = concat getConds'
        cond = COND n name
        ifElseIfElse n i
          | i == 1    = IF     (i-1) <$> insideCond i
          | n == i    = ELSE         <$> insideCond i
          | otherwise = ELSEIF (i-1) <$> insideCond i
        insideCond i = do
          let (isIt,typeOfId) = isRecursiveGuard i fgs
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
              sFifoConsROut j h = "__fifo__"
                                  ++ name
                                  ++ "__rec"
                                  ++ "__expr"
                                  ++ "__" ++ show i
                                  ++ "__" ++ show j
                                  ++ "__consR"
                                  ++ "__" ++ show h
                                  ++ "__out"
              sFifoConsRInp inp j h = "__fifo__"
                                      ++ name
                                      ++ "__rec"
                                      ++ "__expr"
                                      ++ "__" ++ show i
                                      ++ "__" ++ show j
                                      ++ "__consR"
                                      ++ "__" ++ show h
                                      ++ "__in__"
                                      ++ inp
              sFifoConsRNowInp inp j h w = "__fifo__"
                                         ++ name
                                         ++ "__rec"
                                         ++ "__expr"
                                         ++ "__" ++ show i
                                         ++ "__" ++ show j
                                         ++ "__consR"
                                         ++ "__" ++ show h
                                         ++ "__in__now__"
                                         ++ show w ++ "__"
                                         ++ inp

          case isIt of
            True  -> do
              puts' <- forM [1..a] $ \j -> do
                putInputs' <- forM inps $ \(inp',t) -> do
                  let inp = getVal inp'
                  yn <- doesLogicalConnectionExist (fifoInRec inp j)
                  return $ case yn of
                    False -> []
                    True
                      | isStreamT t -> [] --m
                      | otherwise   -> [PUT (fifoInRec inp j,t) inp]
                return $ concat putInputs'
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
              gets' <- forM (zip inps [1..a]) $ \((_,t),j) -> do
                yn <- doesLogicalOutputExist (fifoOutRec j)
                return $ case (yn,isStreamT t) of
                  (True,_) -> []
                  (_,True) -> []
                  _        -> [GET (fifoOutRec j, t)]
              let puts = concat puts'
                  sPuts = concat sPuts'
                  gets = concat gets'
                  
              putStreams <- forM (filter (\((_,t),_) -> isStreamT t)
                                   (zip inps [1..a])) $ \((inp,_),h) ->
                case transitionExpression inp fgs i h of
                      (_,ConsRTransition d)  -> do
                        x <- forM inps $ \(L _ nsInp,t) -> do
                          forM [1..d] $ \d' -> do
                            yn <- doesLogicalConnectionExist (sFifoConsRInp nsInp h d')
                            return $ case yn of
                              False -> []
                              True
                                | isStreamT t -> [COPYV t (sFifoConsRInp nsInp h d') (nsInp++"__savev")] ---m
                                | otherwise   -> [PUT (sFifoConsRInp nsInp h d',t) nsInp]
                        y <- forM sInps $ \(L _ sInp,t) -> do ------ aqui tem caso normal?????
                          forM [1..d] $ \d' -> do
                            let getNumber = numberOfGets fgs sInp
                            forM [1..getNumber] $ \w -> do
                              yn <- doesLogicalConnectionExist (sFifoConsRNowInp sInp h d' w)
                              case yn of
                                False -> return []
                                True  -> return [PUTSTREAM w (sFifoConsRNowInp sInp h d' w,t) sInp]
                        return $ concat $ concat $ x ++ concat y
                      (_,RestTransition) -> return []
                      (_,ConsTransition d)   -> do
                        x <- forM inps $ \(L _ nsInp,t) -> do
                          forM [1..d] $ \d' -> do
                            yn <- doesLogicalConnectionExist (sFifoConsRInp nsInp h d')
                            return $ case yn of
                              False -> []
                              True
                                | isStreamT t -> [COPYV t (sFifoConsRInp nsInp h d') (nsInp++"__savev")] ---m
                                | otherwise   -> [PUT (sFifoConsRInp nsInp h d',t) nsInp]
                        y <- forM sInps $ \(L _ sInp,t) -> do ------ aqui tem caso normal?????
                          forM [1..d] $ \d' -> do
                            let getNumber = numberOfGets fgs sInp
                            forM [1..getNumber] $ \w -> do
                              yn <- doesLogicalConnectionExist (sFifoConsRNowInp sInp h d' w)
                              case yn of
                                False -> return []
                                True  -> return [PUTSTREAM w (sFifoConsRNowInp sInp h d' w,t) sInp]
                        return $ concat $ concat $ x ++ concat y
                      (_,IdTransition)       -> return [] -- ??????
                      (_,FunctionTransition) -> do
                        x <- forM inps $ \(L _ nsInp,t) -> do
                          yn <- doesLogicalConnectionExist (fifoInRec nsInp h)
                          return $ case yn of
                            False -> []
                            True
                              | isStreamT t -> [COPYV t (fifoInRec nsInp h) (nsInp++"__savev")] ---m
                              | otherwise   -> [PUT (fifoInRec nsInp h,t) nsInp]
                        y <- forM sInps $ \(L _ sInp,t) -> do ------ aqui tem caso normal?????
                          let getNumber = numberOfGets fgs sInp
                          forM [1..getNumber] $ \w -> do
                            yn <- doesLogicalConnectionExist (sFifoInRec sInp h w)
                            return $ case yn of
                              False -> []
                              True  -> [PUTSTREAM w (sFifoInRec sInp h w,t) sInp]
                        return $ concat $ x ++ concat y -- ??????
              getStreams <- forM (filter (\((_,t),_) -> isStreamT t)
                                   (zip inps [1..a])) $ \((inp,t),h) ->
                case transitionExpression inp fgs i h of
                      (_,ConsRTransition d)  -> do
                        forM [1..d] $ \d' -> do
                          yn <- doesLogicalOutputExist (sFifoConsROut h d')
                          case yn of
                            False -> return [GET (sFifoConsROut h d', t)
                                            ,PUTOUTPUT "out" (sFifoConsROut h d')]
                            True -> do
                              (lo,lot,isNow) <- getLogicalOutput (sFifoConsROut h d')
                              return $ case (isStreamT lot, isNow) of
                                (False,_) -> [PUTOUTPUT "out" lo]
                                (True,Nothing) -> [COPY lot "out" lo]
                                (True,Just z)  -> [PUTOUTPUTSTREAM z "out" lo]
                      (_,RestTransition)
                        | isVector name (getVal inp) h fgs -> return [[RESTV (getVal inp)]]
                        | otherwise -> return []
                      (_,ConsTransition d)   -> do
                        forM [1..d] $ \d' -> do
                          yn <- doesLogicalOutputExist (sFifoConsROut h d')
                          case yn of
                            False -> return [GET (sFifoConsROut h d', t)
                                            ,PUTV (getVal inp ++ "__savev", t) (sFifoConsROut h d')]
                            True -> do
                              (lo,lot,isNow) <- getLogicalOutput (sFifoConsROut h d')
                              return $ case (isStreamT lot, isNow) of
                                (False,_)      -> [PUTV (getVal inp ++ "__savev", lot) lo]
                                (True,Nothing) -> [] -- caso impossivel?? [COPYV lot (getVal inp ++ "__savev") lo] --m
                                (True,Just z)  -> [PUTOUTPUTSTREAMV lot z (getVal inp ++ "__savev") lo]
                      (_,IdTransition)       -> return [] -- ??????
                      (_,FunctionTransition) -> do --m
                        yn <- doesLogicalOutputExist (fifoOutRec h)
                        case yn of
                          False -> return [[CLEARV (getVal inp)
                                           ,MAKEV t (getVal inp) (fifoOutRec h)]]
                          True -> do
                            (lo,lot,isNow) <- getLogicalOutput (fifoOutRec h)
                            return $ case (isStreamT lot, isNow) of
                              (False,_)      -> [] --[[PUTV (getVal inp ++ "__savev", lot) lo]]
                              (True,Nothing) -> [[CLEARV (getVal inp)
                                                 ,MAKEV lot (getVal inp) lo]] --m
                              (True,Just z)  -> [] --[[PUTOUTPUTSTREAMV lot z (getVal inp ++ "__savev") lo]]
              putStates' <- forM (zip inps [1..a]) $ \((inp,t),j) -> do
                yn <- doesLogicalOutputExist (fifoOutRec j)
                return $ case (yn, isStreamT t) of
                  (True,_) -> []
                  (_,True) -> []
                  _        -> [PUTSTATE (getVal inp) (fifoOutRec j)]
              sIgnores <- forM sInps $ \(inp',t) -> do
                let inp = getVal inp'
                    getNumber = numberOfGets fgs inp
                    nows = getNumber
                    rests = numberOfRests fgs inp
                    savs = nows - rests
                return $ if nows < rests
                  then for [(nows+1)..rests] $ \d -> GETSTREAMSAFE (nows+1,rests) d (inp,t)
                  else []
              let putStates = concat putStates' 
              return $ puts ++ sPuts ++ gets ++ concat putStreams ++ concat (concat getStreams) ++ putStates ++ concat sIgnores
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
                  outputs = case typeOfId of
                    Nothing -> case isStreamT (typeOfExpr i) of
                      True  -> [COPY (typeOfExpr i) "out" fifoOutExpr]
                      False -> [GET (fifoOutExpr, rType)
                               ,PUTOUTPUT "out" fifoOutExpr]
                    Just (v,t,a')
                      | isStreamT t && immediateOutput name a' v fgs -> [COPY t "out" (v ++ "__save")]
                      | isStreamT t && isVector name v a' fgs -> [COPYV t "out" (v ++ "__savev")]
                      | isStreamT t -> [PCOPY (numberOfGets fgs v) t "out" v] --[COPY t "out" v]
                      | otherwise -> [PUTOUTPUT "out" v]
                  destroys = map destroy $ filter (\((_,t),_) -> isStreamT t) (zip inps [1..a])
                  destroy ((s',ty),a') =
                    let s = getVal s'
                    in if isVector name s a' fgs
                       then DESTROYV (s, ty)
                       else DESTROY (numberOfGets fgs s) (s,ty)
              return $ putInputs ++ sPutInputs ++ outputs ++ destroys++ [BREAK]
    ifelses <- forM [1..n] (ifElseIfElse n)
    ret $ initStates ++ concat sInitStates ++ [LOOP (concat sSwitchStates ++ concat sGetStreams ++ putConds ++ sPutConds ++ getConds ++ [cond] ++ ifelses)]

isVector :: Name -> Name -> Int -> [(FExpr,FExpr)] -> Bool
isVector name v a fgs
  = increasingOutput name a v fgs
  || functionOutput name v fgs
  || functionTransition name a fgs
  || idOutput name a v fgs

{-saveornotsave :: Int -> Name -> (Name,FType) -> [(FExpr,FExpr)] -> CProc
saveornotsave h name (v,t) fgs =
  case or [functionOutput name v fgs, idOutput h name v fgs] of
    True  -> [SAVEV (v,t)]
    False -> []-}

idOutput :: Name -> Int -> Name -> [(FExpr,FExpr)] -> Bool
idOutput name h v fgs = or (map (g . snd) fgs)
  where g :: FExpr -> Bool
        g (FApp (ln, _) fexs _)
          | getVal ln == name = or (map isIdTransition (zip fexs [1..]))
        g _ = False
        isIdTransition :: (FExpr,Int) -> Bool
        isIdTransition (_,i)
          | i == h = False
        isIdTransition (FAExpr (FVar (L _ v'), _, _),_)
          | v == v' = True
        isIdTransition _ = False

functionTransition :: Name -> Int -> [(FExpr,FExpr)] -> Bool
functionTransition name a fgs = or (map (g . snd) fgs)
  where g :: FExpr -> Bool
        g (FApp (ln, _) fexs _)
          | getVal ln == name = isFunc (fexs !! (a-1))
        g _ = False
        isFunc :: FExpr -> Bool
        isFunc (FApp (L _ n,_) fexs' _)
          | n == "consR" = False
          | n == "cons"  = False
          | n == "rest"  = False
          | n == "now"   = False
          | otherwise    = True
        isFunc _ = False

functionOutput :: Name -> Name -> [(FExpr,FExpr)] -> Bool
functionOutput name v fgs = or (map g fgs)
  where g :: (FExpr,FExpr) -> Bool
        g (cond,expr) = isFunc cond || gExpr expr
        gExpr :: FExpr -> Bool
        gExpr (FApp (ln, _) fexs _)
          | getVal ln == name = or (map isFunc fexs)
        gExpr _ = False
        isFunc :: FExpr -> Bool
        isFunc (FApp (L _ n,_) fexs' _)
          | n == "consR" = or (map (hasVar v) fexs')
          | n == "cons"  = or (map (hasVar v) fexs')
          | n == "rest"  = False
          | n == "now"   = False
          | otherwise    = or (map (hasVar v) fexs')
        isFunc _ = False
        hasVar :: Name -> FExpr -> Bool
        hasVar v (FApp (L _ m,_) fexs _)
          | m == "consR" = or (map (hasVar v) fexs)
          | m == "cons"  = or (map (hasVar v) fexs)
          | m == "now"   = False
          | m == "rest"  = False
          | otherwise    = or (map (hasVar v) fexs)
        hasVar v (FAExpr (FVar (L _ v'), _, _))
          | v == v' = True
        hasVar _ _ = False

immediateOutput :: Name -> Int -> Name -> [(FExpr,FExpr)] -> Bool
immediateOutput name a v fgs = or (map (g . snd) fgs)
  where g :: FExpr -> Bool
        g (FApp (ln, _) fexs _)
          | getVal ln == name = case fexs !! (a-1) of
              FApp (L _ "consR",_) _ _ -> True
              _ -> False
        g _ = False

increasingOutput :: Name -> Int -> Name -> [(FExpr,FExpr)] -> Bool
increasingOutput name a v fgs = or (map (g . snd) fgs)
  where g :: FExpr -> Bool
        g (FApp (ln, _) fexs _)
          | getVal ln == name = case fexs !! (a-1) of
              FApp (L _ "cons",_) _ _ -> True
              _ -> False
        g _ = False

transitionExpression :: L Name -> [(FExpr,FExpr)] -> Int -> Int -> (FExpr, TransitionType)
transitionExpression inp fgs g h = case snd (fgs !! (g-1)) of
  FApp (L _ _, _) fexs _ -> let fex = fexs !! (h-1) in (fex, transType inp fex)
  x -> error $ "transition expr err " ++ show x

transType :: L Name -> FExpr -> TransitionType
transType inp fex = case fex of
  FApp (L _ name,_) fexs _ 
    | name == "consR" -> ConsRTransition (go "consR" 1 fexs)
    | name == "cons"  -> ConsTransition (go "cons" 1 fexs)
    | name == "rest"  -> RestTransition
    | otherwise       -> FunctionTransition
  FAExpr (FVar lname,_,_)
    | lname == inp -> IdTransition
  FAExpr (fv, _, _) -> FunctionTransition
  where go f n [_,e] = case e of
          FApp (L _ name,_) fexs _
            | name == f -> go f (n+1) fexs
          _ -> n
        go _ n _ = n
  
procedureLeftRecursiveNormal :: TFunc -> TMM CProc
procedureLeftRecursiveNormal tfunc@(name,_,f@(F inps fguards rType),a,_,_) = case fguards of

  NoFGuards expr -> throw (TErr
                           RecursionWithoutCondition
                           Nothing
                           ("Function "
                            ++ name
                            ++ " is recursive without condition.")
                           NoLoc) >> noRet

  FGuards fgs -> do

    let isId i = case fgs !! (i-1) of
          (_, FAExpr (FVar ln,_,t))
            | elem ln (map fst inps) -> Just (getVal ln, t, just (elemIndex ln (map fst inps)))
          _ -> Nothing
        typeOfExpr i = case fgs !! (i-1) of
          (_, expr) -> getTypeFromFExpr expr
        n = length fgs
        nsInps = takeNonStreamInps inps
        sInps  = takeStreamInps inps
        initStates = map (\(inp,t) -> GETINPUT (getVal inp, t)) inps
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
    putConds' <- forM [1..n] $ \i -> do
      forM inps $ \(inp',t) -> do
        let inp = getVal inp'
        yn <- doesLogicalConnectionExist (fifoName i inp)
        return $ case yn of
          False -> []
          True
            | isStreamT t -> []
            | otherwise   -> [PUT (fifoName i inp, t) inp]
    getConds' <- forM [1..n] $ \i -> do
      yn <- doesLogicalOutputExist (fifoOutName i)
      case yn of
        False -> return [GET (fifoOutName i, Bit NoLoc)]
        True -> do
          (lo,lot,isNow) <- getLogicalOutput (fifoOutName i)
          return $ case (isStreamT lot, isNow) of
            (False,_) -> [PUT (fifoOutName i, Bit NoLoc) lo
                         ,GET (fifoOutName i, Bit NoLoc)]
            (True,Nothing) -> [] -- error?
            (True,Just z) -> [PUTSTREAM z (fifoOutName i, Bit NoLoc) lo
                             ,GET (fifoOutName i, Bit NoLoc)]
    let putConds = concat $ concat putConds'
        sPutConds = sPutConds'
        getConds = concat getConds'
        cond = COND n name
        ifElseIfElse n i
          | i == 1    = IF     (i-1) <$> insideCond i
          | n == i    = ELSE         <$> insideCond i
          | otherwise = ELSEIF (i-1) <$> insideCond i
        insideCond i = do
          let isRecursiveGuard i fgs = case fgs !! (i-1) of
                (_, FApp (L _ x,_) _ _)
                  | x == name -> True
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
              gets' <- forM (zip inps [1..a]) $ \((_,t),j) -> do
                yn <- doesLogicalOutputExist (fifoOutRec j)
                return $ case yn of
                  True  -> []
                  False -> [GET (fifoOutRec j, t)]
              putStates' <- forM (zip inps [1..a]) $ \((inp,t),j) -> do
                yn <- doesLogicalOutputExist (fifoOutRec j)
                return $ case yn of 
                  True  -> []
                  False -> [PUTSTATE (getVal inp) (fifoOutRec j)]
              let puts = concat puts'
                  gets = concat gets'
                  putStates = concat putStates'
              return $ puts ++ gets ++ putStates
            False -> do
              putInputs' <- forM inps $ \(inp',t) -> do
                let inp = getVal inp'
                yn <- doesLogicalConnectionExist (fifoOut inp)
                return $ case yn of
                  False -> []
                  True
                    | isStreamT t -> [PCOPY (numberOfGets fgs inp) t (fifoOut inp) inp]
                    | otherwise   -> [PUT (fifoOut inp,t) inp]
              let putInputs = concat putInputs'
                  outputs = case isId i of
                    Nothing -> case isStreamT (typeOfExpr i) of
                      True  -> [COPY (typeOfExpr i) "out" fifoOutExpr]
                      False -> [GET (fifoOutExpr, rType)
                               ,PUTOUTPUT "out" fifoOutExpr]
                    Just (v,t,a')
                      | isStreamT t && immediateOutput name a' v fgs -> [COPY t "out" (v ++ "__save")]
                      | isStreamT t && increasingOutput name a' v fgs -> [COPYV t "out" (v ++ "__savev")]
                      | isStreamT t && functionOutput name v fgs -> [COPYV t "out" (v ++ "__savev")]
                      | isStreamT t -> [PCOPY (numberOfGets fgs v) t "out" v] --[COPY t "out" v]
                      | otherwise -> [PUTOUTPUT "out" v]
              return $ putInputs ++ outputs ++ [BREAK]
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
  FForeverWait -> ok

makeInstancesFromConstStreams :: Name -> ([FCons],Id,FType) -> TM ()
makeInstancesFromConstStreams compName (stream,fid,ftype) = do
  let nameInst = "const_stream" ++ makename' stream
  debug "lllllllllll"
  debug nameInst
  id <- getIdForInstance compName nameInst
  let inst = (compName
             , fid
             , NameId nameInst id
             , ConstStrI
               stream
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
connect (comp, _, F inps fguards ftype, arity,_,_) = case fguards of
  
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
      ConstStrI _ out   -> out
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
      ConstStrI inp _
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
        FApp (L _ "consR", id) args t
          | constantsWithForeverWait args -> do
              let instName = "const_stream" ++ makename args
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
          | elem v (map fst inps) -> do

              addLogicalConnection (comp ++ "__" ++ vs)
              ret ()

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
      FApp (L _ "consR", id) args t
        | constantsWithForeverWait args -> do
            let instName = "const_stream" ++ makename args
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

      _ -> mok

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
        FApp (L _ "consR", id) args t
          | constantsWithForeverWait args -> do
              let instName = "const_stream" ++ makename args
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
        FApp (L _ instName, id) [FAExpr (_,_,Nat _ k), FAExpr (FVar (L _ var),_,t')] _
          | instName == "now" -> do

              addLogicalOutput ("__fifo__"
                                ++ comp
                                ++ "__" ++ txt ++ "__"
                                ++ show i
                                ++ "__out", var, t', Just k)
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

              addLogicalOutput ("__fifo__"
                                ++ comp
                                ++ "__" ++ txt ++ "__"
                                ++ show i
                                ++ "__out", vs, t', Nothing)
                
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

        _ -> mok
            
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
      FApp (L _ "consR", id) args t
        | constantsWithForeverWait args -> do
            let instName = "const_stream" ++ makename args
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

    connectOutterConsR :: Int -> Int -> Int -> (FExpr,FExpr) -> TMM ()
    connectOutterConsR i n q (ex1,ex2) = do
      case ex2 of
        FApp (L _ cons, id) [expr1, expr2] t'
          | cons == "cons"  -> connectOutterConsR i n (q+1) (expr1, expr2)
          | cons == "consR" -> connectOutterConsR i n (q+1) (expr1, expr2)
        _ -> mok
      let fifoName = "__fifo__"
                     ++ comp
                     ++ "__rec"
                     ++ "__expr"
                     ++ "__" ++ show i
                     ++ "__" ++ show n
                     ++ "__consR"
                     ++ "__" ++ show q
                     ++ "__out"
          t = getTypeFromFExpr ex1 --Bit NoLoc
          fifo = FifoI ("in", t) ("out", t)
          fifoNId = NameId fifoName 1
      addInstance (comp, 1, fifoNId, fifo, True)
      case ex1 of
        FApp (L _ instName, id) [FAExpr (_,_,Nat _ k), FAExpr (FVar (L _ var),_,t')] _
          | instName == "now" -> do

              addLogicalOutput ("__fifo__"
                                ++ comp
                                ++ "__rec"
                                ++ "__expr"
                                ++ "__" ++ show i
                                ++ "__" ++ show n
                                ++ "__consR"
                                ++ "__" ++ show q
                                ++ "__out", var, t', Just k)
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
              (connectInnerConsR i n q ex1 inst)
              (zip (filter (not . isFExprNat) args) [0..])
            ret ()
        FAExpr (FVar v@(L _ vs), id, t')
          | elem v (map fst inps) -> do

              addLogicalOutput ("__fifo__"
                                ++ comp
                                ++ "__rec"
                                ++ "__expr"
                                ++ "__" ++ show i
                                ++ "__" ++ show n
                                ++ "__consR"
                                ++ "__" ++ show q
                                ++ "__out", vs, t', Nothing)
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

        _ -> mok
        
    connectInnerConsR :: Int -> Int -> Int -> FExpr -> TInst -> (FExpr, Int) -> TMM ()
    connectInnerConsR i j q fexpr inst@(_,_,nid,ins,_) (expr,n) = case expr of
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
                                ++ "__expr"
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__consR"
                                ++ "__" ++ show q
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
                                ++ "__expr"
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__consR"
                                ++ "__" ++ show q
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
            mapM_
              (connectInnerConsR i j q expr inst')
              (zip (filter (not . isFExprNat) args') [0..])
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
                                ++ "__expr"
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__consR"
                                ++ "__" ++ show q
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
                                ++ "__expr"
                                ++ "__" ++ show i
                                ++ "__" ++ show j
                                ++ "__consR"
                                ++ "__" ++ show q
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
        FApp (L _ "consR", id) args t
          | constantsWithForeverWait args -> do
              let instName = "const_stream" ++ makename args
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
        FApp (L _ cons, id) [expr1, expr2] t'
          | cons == "cons"  -> connectOutterConsR i n 1 (expr1, expr2)
          | cons == "consR" -> connectOutterConsR i n 1 (expr1, expr2)
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

              addLogicalOutput ("__fifo__"
                                ++ comp
                                ++ "__rec"
                                ++ "__" ++ txt
                                ++ "__" ++ show i
                                ++ "__" ++ show n
                                ++ "__out", vs, t', Nothing)
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
            
        _ -> mok
        
    connectInnerRecursive
      :: String -> Int -> Int -> FExpr -> TInst -> (FExpr,Int) -> TMM ()
    connectInnerRecursive
      txt i j fexpr inst@(_,_,nid,ins,_) (expr,n) = case expr of
      FApp (L _ "consR", id) args t
        | constantsWithForeverWait args -> do
            let instName = "const_stream" ++ makename args
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

      _ -> mok
      
isFExprNat :: FExpr -> Bool
isFExprNat (FAExpr (_,_,Nat _ _)) = True
isFExprNat _ = False

special :: Name -> Bool
special n
  = n `elem` ["add","sub","mul","and","or","not","equ","sli","rep","cat","cons","consR","now","rest","mrest"]

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
  FGuards condsExprs -> concat $ map (\(a,b) -> go a ++ go b) condsExprs
  where go (FApp (L _ "consR",_) es _)
          | constantsWithForeverWait es = []
        go (FApp _ es t) = concat (map go es)
        go (FAExpr (FCons c, id, t)) = [(c, id, t)]
        go (FAExpr _) = []

getConstantStreamsFromF :: F -> [([FCons],Id, FType)]
getConstantStreamsFromF (F fvars fguards ftype) = case fguards of
  NoFGuards expr -> go expr
  FGuards condsExprs -> concat $ map (\(a,b) -> go a ++ go b) condsExprs
  where go :: FExpr -> [([FCons],Id,FType)]
        go (FApp (L _ "consR",id) es t)
          | constantsWithForeverWait es = [(concat (map go' es), id, t)]
          | otherwise = concat (map go es)
        go (FApp (L _ _,_) es _) = concat (map go es)
        go _ = []
        go' (FAExpr (FCons FForeverWait, _, _)) = []
        go' (FAExpr (FCons c, _, _)) = [c]
        go' _ = []

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
