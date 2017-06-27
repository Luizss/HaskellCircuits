module Core where

import Data.List (dropWhile)
import Control.Monad (forM_)
import Data.List (foldl)

import Lexer2
import Parser2
import Types
import TransformationMonad
import Aux

storeDataInState :: P2Result -> TM ()
storeDataInState (P2Result pdecls) = do
  forM_ (filter isData pdecls)
    $ \(PDataType (L s (Low name)) pconstrs) ->
        addData (L s name, pconstrs)
    
isData (PDataType _ _) = True
isData  _              = False

gatherFunctions :: P2Result -> TMM [(Name, PFType, [Name], PGuards)]
gatherFunctions (P2Result pds) = do
  let pds' = filter (not . isData) pds
  fs <- glueFunctions pds'
  ret $ map patternsToGuards fs
  where patternsToGuards (n,ft,fd)
          = (n,ft,varsFromPats fd,concatGuards (map destroyPats fd))

varsFromPats :: [([Pat], a)] -> [Name]
varsFromPats xs = map toName [0..(length pats - 1)]
  where (pats,_) = head xs
        toName i = "__i" ++ show i        
          
glueFunctions :: [PDecl] -> TM [(Name, PFType, [([Pat],PGuards)])]
glueFunctions pds = case pds of
  PFType (L _ (Low name)) ft : ps -> do
    let r   = takeFunctionDef name ps
        ps' = dropFunctionDef name ps
    rest <- glueFunctions ps'
    return $ (name, ft, r) : rest
  [] -> return []
  x -> error (show x)

dropFunctionDef :: Name -> [PDecl] -> [PDecl]
dropFunctionDef name = dropWhile (isSameFunc name)

isSameFunc :: Name -> PDecl -> Bool
isSameFunc name p = case p of
  PFunc (L _ (Low n)) pats pgs
    | n == name -> True
  _ -> False
  
takeFunctionDef :: Name -> [PDecl] -> [([Pat], PGuards)]
takeFunctionDef name = map patsNguards . takeWhile (isSameFunc name)
  where patsNguards (PFunc (L _ (Low n)) pats pgs) = (pats, pgs)
        patsNguards _ = error "patsnguards"

destroyPats :: ([Pat],PGuards) -> PGuards
destroyPats (pats, pgs) =
  destroy (zip pats (map makeExpr [0..])) pgs
          
destroy :: [(Pat, PExpr)] -> PGuards -> PGuards
destroy [] pgs = pgs
destroy ((pat,expr):pis) pgs = case pat of
    Pat (L _ (Low  x)) -> destroy pis (replaceBy x expr pgs)
    Pat (L _ (Upp  x)) -> destroy pis (df (x,expr) pgs)
    Pat (L _ (Bin  x)) -> error "bindechex" -- type?
    Pat (L _ (Hex  x)) -> error "bindechex" -- type?
    Pat (L _ (Dec  x)) -> error "bindechex" -- type?
    ConsPat (L _ (Upp x)) pats ->
      let argExprs = zip pats (map
                                (concatExpr x expr)
                                (zip pats [0..]))
          pgs'  = destroy argExprs pgs
          pgs'' = df (x,expr) pgs'
      in destroy pis pgs''
    _ -> error "destroypats"

concatExpr :: Name -> PExpr -> (Pat,Int) -> PExpr
concatExpr x pexpr (p,i)
  = PApp (noLocLow ("__get__" ++ x ++ "__" ++ show i)) [pexpr]

replaceBy :: Name -> PExpr -> PGuards -> PGuards
replaceBy name pexpr pgs =
  let replace :: PExpr -> PExpr
      replace e = case e of
        PAExpr (L _ (Low x))
          | x == name -> pexpr
        PAExpr x -> PAExpr x
        PApp ltn exps -> PApp ltn (map replace exps)
  in case pgs of
    PNoGuards e -> PNoGuards $ replace e
    PGuards ces -> PGuards $ map (\(c,e)
                                   -> (replace c,replace e)) ces

makeExpr i = PAExpr $ noLocLow $ "__i" ++ show i

isPAExpr (PAExpr _) = True
isPAExpr  _ = False

noLoc = L NoLoc

noLocLow = L NoLoc . Low

df :: (Name, PExpr) -> PGuards -> PGuards
df (nameCons,pexpr) pgs = case pgs of
  PNoGuards expr -> PGuards [(cond,expr)]
  PGuards ces -> PGuards $ for ces $ \(c,e) ->
    let c' = andExpr cond c
    in (c',e)
  where cond = PApp (noLocLow ("__is__" ++ nameCons)) [pexpr]

andExpr e1 e2 = PApp (noLocLow "and") [e1,e2]
trueCond = PAExpr (noLocLow "otherwise")

concatGuards :: [PGuards] -> PGuards
concatGuards pgs = foldl1 f pgs
  where f :: PGuards -> PGuards -> PGuards
        f (PNoGuards _) (PNoGuards _)
          = error "concatguards"
        f (PNoGuards p1) (PGuards ps2)
          = error "concatguards1"
        f (PGuards ps1) (PNoGuards p2)
          = PGuards (ps1 ++ [(trueCond,p2)])
        f (PGuards ps1) (PGuards ps2)
          = PGuards (ps1 ++ ps2)

toCore :: [(Name, PFType, [Name], PGuards)] -> Core
toCore dcs = Core (map toCore' dcs)
  where toCore' :: (Name, PFType, [Name], PGuards) -> CFunc
        toCore' (n,pfty,vars,pgs)
          = CFunc (noLocLow n) (map noLocLow vars) (toCGS pgs) (toCFTy pfty)
          
        toCGS :: PGuards -> CGuards
        toCGS (PNoGuards x) = CNoGuards x
        toCGS (PGuards xys) = CGuards xys

        toCFTy :: PFType -> [CFType]
        toCFTy (PTApp pft1 pft2)
          = let [cft1] = toCFTy pft1
                [cft2] = toCFTy pft2
            in [CTApp cft1 cft2]
        toCFTy (PTAExpr x)
          = [CTAExpr x]
        toCFTy (PTArrow pft1 pft2)
          = let cfts1 = toCFTy' pft1
                cfts2 = toCFTy' pft2
            in cfts1 ++ cfts2

        toCFTy' :: PFType -> [CFType]
        toCFTy' (PTApp pft1 pft2)
          = let [cft1] = toCFTy' pft1
                [cft2] = toCFTy' pft2
            in [CTApp cft1 cft2]
        toCFTy' (PTAExpr x)
          = [CTAExpr x]
        toCFTy' (PTArrow pft1 pft2)
          = let cfts1 = toCFTy' pft1
                cfts2 = toCFTy' pft2
            in [CTArrow (cfts1 ++ cfts2)]
        
data Core = Core [CFunc]
          deriving Show

data CFunc = CFunc LToken [LToken] CGuards [CFType]
           -- data colocada no estado
           deriving Show

data CGuards = CNoGuards PExpr
             | CGuards [(PExpr,PExpr)]
             deriving Show

data CFType = CTApp CFType CFType
            | CTArrow [CFType]
            | CTAExpr LToken
            deriving (Show, Eq)

------- typedcore

data TCore = TCore [TCFunc]
           deriving Show

data TCFunc = TCFunc LToken [(LToken,CFType)] TCGuards CFType
           deriving Show

data TCGuards = TCNoGuards TCExpr
              | TCGuards [(TCExpr,TCExpr)]
              deriving Show

data TCExpr = TCApp LToken [TCExpr] CFType
            | TCAExpr (LToken,CFType)
            deriving Show

-------- type checking / type inference

typeCheck :: Core -> TCore
typeCheck core = undefined
