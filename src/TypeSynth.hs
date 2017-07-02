module TypeSynth where

import Control.Monad (forM_,forM)
import TransformationMonad
import Types
import Lexer2
import Aux

noLocLow = L NoLoc . Low
noLocUpp = L NoLoc . Upp
noLocDec = L NoLoc . Dec
noLocBin = L NoLoc . Bin

typeSynth :: TMM ()
typeSynth = do
  definePatternFunctions
  changeTypes

definePatternFunctions :: TMM ()
definePatternFunctions = do
  dds <- getDataDecls
  forM_ dds $ \(L _ name,_,_,_) -> do
    definePatternFunctions' name
  ret ()
  
definePatternFunctions' :: Name -> TMM ()
definePatternFunctions' name = do
  mdd <- searchDataDecl name
  cont1 mdd $ \(L s _, constrs, isRec, used) -> do
    case used of
      True -> mok
      False -> do
        let p = length constrs
            n = ceiling (logBase 2 (fromIntegral p))
            tags = map (toBin n) [0..(p-1)]
            constrsNtags = zip3 constrs tags [0..]
        mm <- calcVecNumber constrs isRec
        cont1 mm $ \(m, lens) -> do
          let len = n + m
          debug $ "H: " ++ show len
          addTypeChange name len
          forM_ constrsNtags $ \(CConstr (L s (Upp datacons)) cfts,tag,i) -> do
            let typeCons = CTAExpr (L s (Upp name))
                bool = CTAExpr (L s (Upp "Bool"))
                nat n = CTApp (noLocUpp "Nat") [CTAExpr (noLocDec n)]
                vec n | n == 1 = CTAExpr (noLocUpp "Bit")
                      | otherwise = CTApp (noLocUpp "Vec") [CTAExpr (noLocDec n)]
                isExpr = TCApp (noLocLow "equ")
                         [TCAExpr (noLocBin tag, vec n)
                         ,TCApp (noLocLow "sli")
                           [TCAExpr (noLocDec 0, nat 0)
                           ,TCAExpr (noLocDec (n-1), nat (n-1))
                           ,TCAExpr (noLocLow "_x0", typeCons)]
                           (vec n)]
                         bool
                getN (_,t) = getNumberFromType t
                catVars [v] = TCAExpr v
                catVars (v:vs) = TCApp (noLocLow "cat")
                                 [TCAExpr v, catVars vs]
                                 (vec (getN v + sum (map getN vs)))
                consExpr vars = case vars of
                  []  -> TCAExpr (noLocBin tag, vec n)
                  [x] -> TCApp (noLocLow "cat") [TCAExpr  x,TCAExpr (noLocBin tag, vec n)] (vec len)
                  xs  -> TCApp (noLocLow "cat") [catVars xs,TCAExpr (noLocBin tag, vec n)] (vec len)
            addTCFunc $ TCFunc
              (noLocLow ("__is__" ++ datacons))
              [(noLocLow "_x0", typeCons)]
              (TCNoGuards isExpr)
              bool
            mcfts_ <- mapM changeType cfts
            cont_ mcfts_ $ \cfts_ -> do
              let vars = zip (map (noLocLow . ("_x"++) . show) [0..]) cfts_
              addTCFunc $ TCFunc
                (L s (Upp datacons))
                vars
                (TCNoGuards (consExpr vars))
                typeCons
              forM_ (zip cfts [0..]) $ \(cft,j) -> do
                isThere <- isThereTypeChange cft --getname?
                case isThere of
                  True  -> mok
                  False -> definePatternFunctions' (getNameFromType cft)
                mcft' <- changeType cft
                cont1 mcft' $ \cft' -> do
                  debugs (j,m)
                  let (_,lens') = lens !! i
                      ki = sum (take j lens') + n
                      k = lens' !! j
                      kf = ki + k - 1 
                      getExpr = TCApp (noLocLow "sli")
                                [TCAExpr (noLocDec ki, nat ki)
                                ,TCAExpr (noLocDec kf, nat kf)
                                ,TCAExpr (noLocLow "_x0", typeCons)]
                                (vec k)
                  addTCFunc $ TCFunc
                    (noLocLow ("__get__" ++ datacons ++ "__" ++ show j))
                    [(noLocLow "_x0", typeCons)]
                    (TCNoGuards getExpr)
                    cft
                  setDataUsed name 
                  ret ()
              ret ()
            ret ()
          ret ()

getNameFromType :: CFType -> Name
getNameFromType cft = case cft of
  CTAExpr (L _ (Upp n)) -> n
  n -> error $ "getnamefromtype " ++ show n

getNumberFromType :: CFType -> Int
getNumberFromType cft = case cft of
  CTApp (L _ (Upp "Vec")) [CTAExpr (L _ (Dec n))] -> n
  n -> error $ "getnumberfromtype " ++ show n

calcVecNumber :: [CConstr] -> IsRec -> TMM (Int, [(Int, [Int])])
calcVecNumber constrs isRec = do
  meach <- forM constrs $ \(CConstr _ cfts) -> do
    mnums <- forM cfts $ \cft -> do
      isThere <- isThereTypeChange cft --getname?
      case isThere of
        True  -> mok
        False -> definePatternFunctions' (getNameFromType cft)
      mcft' <- changeType cft
      cont1 mcft' $ \cft' -> do
        ret $ getNumberFromType cft'
    cont_ mnums $ \nums -> do
      ret (sum nums, nums)
  cont_ meach $ \each -> do
    ret (maximum (map fst each), each)
  
changeTypes :: TMM ()
changeTypes = do
  TCore fs <- getTCore
  mfs' <- forM fs $ \(TCFunc ltk vars tcgs ty) -> do
    mvars' <- forM vars $ \(vltk, vty) -> do
      mvty' <- changeType vty
      cont1 mvty' $ \vty' -> do
        debug "lllllllllll"
        ret (vltk, vty')
    mty' <- changeType ty
    mtcgs' <- changeTypeGs tcgs
    cont_ mvars' $ \vars' -> do
      debug "kkkkkkkkkk"
      cont1 mty' $ \ty' -> do
        debug "jjjjjjjjjjjjjj"
        cont1 mtcgs' $ \tcgs' -> do
          debug "ggggggggg"
          ret $ TCFunc ltk vars' tcgs' ty'
  cont_ mfs' $ \fs' -> do
    debug $ "aaaaaaa " ++ show (fs == fs')
    putTCore (TCore fs')
    ret ()

changeTypeGs :: TCGuards -> TMM TCGuards
changeTypeGs tcgs = case tcgs of
  TCNoGuards e -> do
    me' <- changeTypeExpr e
    cont1 me' $ \e' -> do
      ret $ TCNoGuards e'
  TCGuards ces -> do
    mces' <- forM ces $ \(c,e) -> do
      mc' <- changeTypeExpr c
      me' <- changeTypeExpr e
      cont2 mc' me' $ \c' e' -> do
        ret (c',e')
    cont_ mces' $ \ces' -> do
      ret $ TCGuards ces'

changeTypeExpr :: TCExpr -> TMM TCExpr
changeTypeExpr tcexpr = case tcexpr of
  TCApp name args ty -> do
   margs' <- mapM changeTypeExpr args
   mty' <- changeType ty
   cont_ margs' $ \args' -> do
     cont1 mty' $ \ty' -> do
       ret $ TCApp name args' ty'
  TCAExpr (name,ty) -> do
    mty' <- changeType ty
    cont1 mty' $ \ty' ->
      ret $ TCAExpr (name,ty')

toBin n = reverse . putZeros . dropWhile (=='0') . toBin'
  where
    putZeros s
      | length s < n = replicate (n - length s) '0' ++ s
      | otherwise    = s
    toBin' 0 = "0"
    toBin' n | n `mod` 2 == 1 = toBin' (n `div` 2) ++ "1"
             | n `mod` 2 == 0 = toBin' (n `div` 2) ++ "0"
