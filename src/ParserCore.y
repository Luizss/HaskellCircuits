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
  
  '->'   { L _ RArrow }
  '::'   { L _ DColon }

  VARID  { L _ (Low _) }
  CONID  { L _ (Upp _) }
  '+'    { L _ (Sym "+") }
  '-'    { L _ (Sym "-") }
  '*'    { L _ (Sym "*") }
  '/'    { L _ (Sym "/") }
  SYMID  { L _ (Sym _) }

  INT    { L _ (Int _) }

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
     | function_type_decl { $1 } 

function_decl : VARID vars '=' expr { Func $1 (r $2) $4 }

vars : vars VARID    { $2 : $1 }
     | VARID         { [$1] }
     | {- empty -}   { [] }

expr : expr aexpr {App $1 $2}
     | binops { $1 }
     | aexpr { $1 }
aexpr : VARID  {AExpr $1}
      | INT {AExpr $1}
      | '(' expr ')' { $2 }
binops : expr '+'  expr  { Binop $2 $1 $3 }
       | expr '-'  expr  { Binop $2 $1 $3 }
       | expr '*'  expr  { Binop $2 $1 $3 }
       | expr '/'  expr  { Binop $2 $1 $3 }
       | expr SYMID expr { Binop $2 $1 $3 }

function_type_decl : VARID '::' type_expr { FuncType $1 $3 }

type_expr : btype '->' type_expr { TArrow $1 $3 }
          | btype                { $1 }
btype : btype atype              { TApp $1 $2 }
      | atype                    { $1 }
atype : VARID                    { TAExpr $1 }
      | CONID                    { TAExpr $1 }
      | INT                      { TAExpr $1 }

{

data PState = NoState deriving Show
-- 'Either String a' pode ser substituido por 'ParseResult a'
type P a = StateT PState (Either String) a

data Program = Program [Decl] deriving Show
data Decl = Func LToken [LToken] Expr
          | FuncType LToken TypeExpr
          deriving Show
data Expr = App Expr Expr
          | AExpr LToken
          | Binop LToken Expr Expr
          deriving Show
data TypeExpr = TArrow TypeExpr TypeExpr
              | TApp TypeExpr TypeExpr
              | TAExpr LToken
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
