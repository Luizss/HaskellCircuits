{

module ParserCore where

import LexerCore

import Control.Monad.State (StateT(..), evalStateT)

}

%name programParser
%tokentype { LToken }
%error { parseError }
%monad { P } { >>= } { return }
%token

  '('    { L _ LParen }
  ')'    { L _ RParen }

  ';'    { L _ Semic }

  '='    { L _ Equal }

  ':'    { L _ Colon }

  VARID  { L _ (Low _) }
  CONID  { L _ (Upp _) }
  '+'    { L _ (Sym "+") }
  '-'    { L _ (Sym "-") }
  '*'    { L _ (Sym "*") }
  '/'    { L _ (Sym "/") }
  SYMID  { L _ (Sym _) }

  BIN    { L _ (Bin _) }
  HEX    { L _ (Hex _) }
  DEC    { L _ (Dec _) }

  EOF    { L _ EOF }

%left '+' '-'
%left '*' '/'
%%

program : decls { Program (r $1) }

decls : decls ';' decl { $3 : $1  }
      | decls ';'      { $1 }
      | decl           { [$1] }
      | {- empty -}    { [] }

decl : function_decl { $1 }

function_decl : VARID varsAndTypes ':' typeExpr '=' expr { Func $1 (r $2) $6 $4 }

varAndType : '(' VARID ':' typeExpr ')'   { Ty $4 $2 }
binAndType : '(' BIN ':' typeExpr ')'     { Ty $4 $2 }
hexAndType : '(' HEX ':' typeExpr ')'     { Ty $4 $2 }
decAndType : '(' DEC ':' typeExpr ')'     { Ty $4 $2 }

varsAndTypes : varsAndTypes varAndType    { $2 : $1 }
             | varAndType                 { [$1] }
             | {- empty -}                { [] }

expr : expr aexpr ':' typeExpr {App $1 $2 $4}
     | binops { $1 }
     | aexpr { $1 }
aexpr : VARID        { AExpr   $1 }
      | varAndType   { ATyExpr $1 }
      | binAndType   { ATyExpr $1 }
      | hexAndType   { ATyExpr $1 }
      | decAndType   { ATyExpr $1 }
      | '(' expr ')' { $2 }
binops : expr '+'  expr  { Binop $2 $1 $3 } -- binops not really necessary
       | expr '-'  expr  { Binop $2 $1 $3 }
       | expr '*'  expr  { Binop $2 $1 $3 }
       | expr '/'  expr  { Binop $2 $1 $3 }
       | expr SYMID expr { Binop $2 $1 $3 }

typeExpr : typeExpr aType { TApp $1 $2 } 
         | aType          { $1 }

aType : VARID   { TAExpr $1 }
      | CONID   { TAExpr $1 }
      | DEC     { TAExpr $1 }
       
{

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y
  
instance Ord a => Ord (L a) where
  compare (L _ x) (L _ y) = compare x y

data TypeExpr = TApp TypeExpr TypeExpr
              | TAExpr LToken
              deriving (Show, Eq)

data Ty a = Ty TypeExpr a
          deriving (Show, Eq, Functor)

type TyLToken = Ty LToken

data PState = NoState deriving Show
-- 'Either String a' pode ser substituido por 'ParseResult a'
type P a = StateT PState (Either String) a

data Program = Program [Func] deriving Show
data Func = Func LToken [TyLToken] Expr TypeExpr deriving Show
data Expr = App Expr Expr
          | ATyExpr TyLToken
          | AExpr LToken
          | Binop LToken Expr Expr
          deriving Show

------------- Top level 'parse' function

r :: [a] -> [a]
r = reverse

runPWithoutError :: ([LToken] -> P Program) -> [LToken] -> Either String Program
runPWithoutError p inp = evalStateT (p inp) initialState
  where initialState = NoState

runP :: ([LToken] -> P Program) -> [LToken] -> Program
runP p = catchEither . runPWithoutError p

parse :: [LToken] -> Program
parse = runP programParser

parse' :: [LToken] -> Either String Program
parse' = runPWithoutError programParser

parseError :: [LToken] -> P a
parseError []
  = left $ "Parse error at end (is there a ')','}' or ']' missing?)."
parseError (L NoLoc t : ltoks)
  = left $ "Parse error: " ++ show t ++ " (is your indentation wrong?)"
parseError (L (SrcLoc _f l c) t : ltoks)
  = left $ "Parse error in {" ++ show l ++ "," ++ show c ++ "}: " ++ show t

left :: String -> P a
left l = StateT (\s -> Left l)

}
