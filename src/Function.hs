module Function where

-- module will be more comples in later modules

import ParserCore
import LexerCore

type FName = String
type FSymbol = String
type FVars = [L FName]

data F = F FVars FExpr deriving Show
data FVarCons = FVar (L FName)
              | FCons (L Int)
              deriving Show
data FExpr = FApp (L FName) [FExpr]
           | FAExpr FVarCons
           deriving Show

-- interpret must be more complex for functions
-- with type signatures and pattern matching definitions
interpret :: Program -> [(L FName, F)]
interpret (Program fs) = map interpretEach fs

interpretEach :: Func -> (L FName, F)
interpretEach (Func name vars body)
  = (fmap getName name
    , F (map (fmap getName) vars) (getFExpr body))
  where getName (Low n) = n
        getFExpr expr = case expr of
          App e1 e2       ->
            let e1' = getFExpr e1
                e2' = getFExpr e2
            in joinApps e1' e2'
          Binop ltok e1 e2 ->
            let e1' = getFExpr e1
                e2' = getFExpr e2
            in joinApps (joinApps (FAExpr (FVar (fmap (\(Sym s) -> s) ltok))) e1') e2'
          AExpr (L s (Low v)) -> FAExpr (FVar (L s v))
          AExpr (L s (Int i)) -> FAExpr (FCons (L s i))
          

        joinApps (FApp f args) expr = FApp f (args ++ [expr])
        joinApps (FAExpr (FCons c)) _ = error "aaa" -- constante
        joinApps (FAExpr (FVar v)) expr = FApp v [expr]

-- esse módulo deve pegar erros de arity

{-
data FExpr' = FApp' (L FName) FExpr'
            | FAExpr' FVarCons
            | 
            deriving Show
-}
