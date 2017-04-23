{

module Parser where

import Lexer

import Control.Monad.State (StateT(..), evalStateT)

}

%name programParser
%tokentype { LToken }
%error { parseError }
%monad { P } { >>= } { return }
%token

  where  { L _ Where }

  let    { L _ Let }
  in     { L _ In }

  if     { L _ If }
  then   { L _ Then }
  else   { L _ Else }

  case   { L _ Case }
  of     { L _ Of }

  type   { L _ Type }
  data   { L _ Data }

  '('    { L _ LParen }
  ')'    { L _ RParen }

  '['    { L _ LBracket }
  ']'    { L _ RBracket }

  '{'    { L _ LBrace }
  '}'    { L _ RBrace }
  ';'    { L _ Semic }

  ':'    { L _ Colon }
  ','    { L _ Comma }

  'λ'    { L _ Lambda }
  '->'   { L _ Arrow }
  '='    { L _ Equal }
  '|'    { L _ Pipe }
  '..'   { L _ TwoDots }
  '::'   { L _ TwoColons }

  VARID  { L _ (Low _) }
  CONID  { L _ (Upp _) }

  '=='   { L _ TwoEqual }
  '/='   { L _ Diff }
  '>'    { L _ (Sym "<") }
  '<'    { L _ (Sym ">") }
  '+'    { L _ (Sym "+") }
  '-'    { L _ (Sym "-") }
  '*'    { L _ (Sym "*") }
  '/'    { L _ (Sym "/") }
  SYMID  { L _ (Sym _) }

  INT    { L _ (Int _) }
  STRING { L _ (String _) }

  EOF    { L _ EOF }

%right ':'
%nonassoc '>' '<'
%nonassoc '==' '/='
%left '+' '-'
%left '*' '/'
%%

program : decls { Program (r $1) }

decls : decls ';' decl { $3 : $1  }
      | decls ';'      { $1 }
      | decl           { [$1] }
      | {- empty -}    { [] }

decl : function_decl { Function $1 }
     | type_syn_decl { TypeSyn $1 }
     | data_decl {DataDef $1}
     | function_type_decl {FunctionType $1}

function_decls : function_decls ';' function_decl { $3 : $1  }
               | function_decls ';'               { $1 }
               | function_decl                    { [$1] }
               | {- empty -}                      { [] }

function_decl : function_body where '{' function_decls '}' { F $1 (r $4) }
              | function_body                              { F $1 [] }

function_body : VARID pats function_expr { FB $1 (r $2) $3 }

function_expr : '=' expr   { NoCond $2 }
              | '|' guards { Guards (r $2) }

guards : guards '|' guard { $3 : $1  }
       | guards '|'       { $1 }
       | guard            { [$1] }

guard : expr '=' expr { Guard $1 $3 }

pats : pats pat    { $2 : $1 }
     | pat         { [$1] }
     | {- empty -} { [] }             

pat : VARID         { Pat $1 }
    | INT           { Pat $1 }
    | CONID         { Pat $1 }
    | '(' ')'       { UnitPat }
    | '[' ']'       { EmptyListPat }
    | '[' ppats_comma ']' { ListPat $2 EmptyListPat }
    | '(' ppats_comma ')' { TupPat $2 }
    | '(' ppat ')'  { $2 }
ppats_comma : ppat ',' ppats_comma { $1 : $3  }
            | ppat                 { [$1] }
ppats_colon : ppatNotColon ':' ppats_colon { $1 : $3 }
            | ppatNotColon         { [$1] }
ppat : CONID pats   { ConsPat $1 (r $2) }
     | pat                 { $1 } -- ugly but works
     | pat ':' ppats_colon { ListPat (init ($1:$3)) (last ($1:$3)) }
     | CONID pats ':' ppats_colon { ListPat (init (ConsPat $1 (r $2):$4)) (last (ConsPat $1 (r $2) :$4)) }
ppatNotColon : CONID pats { ConsPat $1 (r $2) }
             | pat        { $1 }

exprs : exprs expr   { $2 : $1 }
      | expr         { [$1] }
      | {- empty -}  { [] }             

expr : expr aexpr {App $1 $2}
     | binops { $1 }
     | if expr then expr else expr {IfThenElse $2 $4 $6}
     | let '{' function_decls '}' in expr {LetIn (r $3) $6}
     | case expr of '{' alts '}' {CaseOf $2 (r $5)}
     | 'λ' pats '->' expr {Lam (r $2) $4}
     | aexpr { $1 }
aexpr : VARID  {AExpr $1}
      | INT {AExpr $1}
      | STRING {AExpr $1}
      | CONID {AExpr $1}
      | '(' ')' { UnitExpr }
      | '[' ']' { EmptyListExpr }
      | '[' paexprs_comma ']' { ListExpr (r $2) }
      | '(' paexprs_comma ')' { TupExpr (r $2)}
      | '(' paexpr ')' { $2 }
paexprs_comma : paexprs_comma ',' paexpr { $3 : $1  }
              | paexpr                 { [$1] }
paexpr : CONID exprs { CExpr $1 (r $2) }
       | expr { $1 }

binops : expr '==' expr  { Binop $2 $1 $3 }
       | expr '/=' expr  { Binop $2 $1 $3 }
       | expr '>'  expr  { Binop $2 $1 $3 }
       | expr '<'  expr  { Binop $2 $1 $3 }
       | expr '+'  expr  { Binop $2 $1 $3 }
       | expr '-'  expr  { Binop $2 $1 $3 }
       | expr '*'  expr  { Binop $2 $1 $3 }
       | expr '/'  expr  { Binop $2 $1 $3 }
       | expr ':'  expr  { Binop $2 $1 $3 } 
       | expr SYMID expr { Binop $2 $1 $3 }

alts : alts alt    { $2 : $1 }
     | alts ';'    { $1 }
     | alt         { [$1] }
     | {- empty -} { [] }             

alt : ppat '->'expr { Alt $1 $3 }

type_syn_decl : type CONID vars '=' type_decl { TS $2 (r $3) $5 }

type_decl : btype '->' type_decl { TArrow $1 $3 }
          | btype                { $1 }
btype : btype atype              { TApp $1 $2 }
      | atype                    { $1 }
atype : CONID                    { AType $1 }
      | VARID                    { AType $1 }
      | '(' types_comma ')'      { TupType (r $2) }
      | '[' type_decl ']'        { ListType $2 }
      | '(' type_decl ')'        { $2 }

types_comma : types_comma ',' type_decl { $3 : $1  }
            | type_decl                 { [$1] }

vars : vars VARID    { $2 : $1 }
     | VARID       { [$1] }
     | {- empty -} { [] }

data_decl : data CONID vars '=' constrs { D $2 (r $3) (r $5) }

constrs : constrs '|' constr { $3 : $1 }
        | constrs '|'        { $1 }
        | constr             { [$1] }

constr : CONID atypes { Constr $1 $2}

-- right recursion because of infinite loop
-- reverse not necessary
atypes : atype atypes { $1 : $2  }
       | {- empty-}   { [] }

function_type_decl : VARID '::' type_decl {FT $1 $3}

{

data D = D LToken [LToken] [Constr] deriving Show
data Constr = Constr LToken [Type] deriving Show
data Expr = App Expr Expr
          | IfThenElse Expr Expr Expr
          | LetIn [F] Expr
          | CaseOf Expr [Alt]
          | Lam [Pat] Expr
          | AExpr LToken
          | UnitExpr
          | EmptyListExpr
          | TupExpr [Expr]
          | CExpr LToken [Expr]
          | Binop LToken Expr Expr
          | ListExpr [Expr]
          deriving Show
data FT = FT LToken Type deriving Show

data PState = NoState deriving Show
-- 'Either String a' pode ser substituido por 'ParseResult a'
type P a = StateT PState (Either String) a

data Program = Program [Decl] deriving Show
data Decl = Function F
          | TypeSyn TS
          | DataDef D
          | FunctionType FT
          deriving Show
data TS = TS LToken [LToken] Type deriving Show
data Type = TArrow Type Type
          | TApp Type Type
          | AType LToken
          | TupType [Type]
          | ListType Type
          deriving Show
data F = F FBody [F] deriving Show
data FBody = FB LToken [Pat] FExpr
           deriving Show
data FExpr = NoCond Expr
           | Guards [Guard]
           deriving Show
type Cond = Expr
data Guard = Guard Cond Expr
           deriving Show
data Pat = Pat LToken
         | UnitPat
         | EmptyListPat
         | TupPat [Pat]
         | ListPat [Pat] Pat
         | ConsPat LToken [Pat]
         deriving Show
data Alt = Alt Pat Expr
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
