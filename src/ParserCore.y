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
  '|'    { L _ Pipe  }
  
  VARID  { L _ (Low _) }
  CONID  { L _ (Upp _) }

  BIN    { L _ (Bin _) }
  HEX    { L _ (Hex _) }
  DEC    { L _ (Dec _) }

  EOF    { L _ EOF }

%left '+' '-'
%left '*' '/'
%%

program : decls { PResult (r $1) }

decls : decls ';' decl { $3 : $1  }
      | decls ';'      { $1 }
      | decl           { [$1] }
      | {- empty -}    { [] }

decl : function_decl { $1 }

function_decl : VARID varsAndTypes ':' typeExpr function_expr { PFunc $1 (r $2) $5 $4 }

function_expr : '=' expr   { PNoGuards $2 }
              | '|' guards { PGuards (r $2) }
guards : guards '|' guard { $3 : $1  }
       | guards '|'       { $1 }
       | guard            { [$1] }
guard : expr '=' expr { ($1, $3) }

varAndType : '(' VARID ':' typeExpr ')'   { ($2, $4) }
binAndType : '(' BIN ':' typeExpr ')'     { ($2, $4) }
hexAndType : '(' HEX ':' typeExpr ')'     { ($2, $4) }
decAndType : '(' DEC ':' typeExpr ')'     { ($2, $4) }

varsAndTypes : varsAndTypes varAndType    { $2 : $1 }
             | varAndType                 { [$1] }
             | {- empty -}                { [] }

exprs : exprs expr    { $2 : $1 }
      | expr          { [$1] }
      | {- empty -}   { [] }

expr : VARID exprs ':' typeExpr { PApp $1 (r $2) $4 }
     | aexpr { $1 }
aexpr : varAndType   { PAExpr $1 }
      | binAndType   { PAExpr $1 }
      | hexAndType   { PAExpr $1 }
      | decAndType   { PAExpr $1 }
      | '(' expr ')' { $2 }

typeExpr : typeExpr aType { PTApp $1 $2 } 
         | aType          { $1 }
aType : VARID   { PTAExpr $1 }
      | CONID   { PTAExpr $1 }
      | DEC     { PTAExpr $1 }
      | '(' typeExpr ')' { $2 }
       
{

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y
  
instance Ord a => Ord (L a) where
  compare (L _ x) (L _ y) = compare x y

data PTypeExpr = PTApp PTypeExpr PTypeExpr
               | PTAExpr LToken
               deriving (Show, Eq)

data PState = NoState deriving Show
-- 'Either String a' pode ser substituido por 'ParseResult a'
type P a = StateT PState (Either String) a

data PGuards = PNoGuards PExpr
             | PGuards [(PExpr,PExpr)]
             deriving Show
data PResult = PResult [PFunc] deriving Show
data PFunc = PFunc LToken [(LToken, PTypeExpr)] PGuards PTypeExpr
           deriving Show
data PExpr = PApp LToken [PExpr] PTypeExpr
           | PAExpr (LToken, PTypeExpr)
           deriving Show

------------- Top level 'parse' function

r :: [a] -> [a]
r = reverse

runPWithoutError :: ([LToken] -> P PResult) -> [LToken] -> Either String PResult
runPWithoutError p inp = evalStateT (p inp) initialState
  where initialState = NoState

runP :: ([LToken] -> P PResult) -> [LToken] -> PResult
runP p = catchEither . runPWithoutError p

parse :: [LToken] -> PResult
parse = runP programParser

parse' :: [LToken] -> Either String PResult
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
