{

module Parser2 where

import Lexer2

import Control.Monad.State (StateT(..), evalStateT)

}

%name programParser
%tokentype { LToken }
%error { parseError }
%monad { P } { >>= } { return }
%token

  data  { L _ Data }
  
  '('    { L _ LParen }
  ')'    { L _ RParen }

  ';'    { L _ Semic }

  '='    { L _ Equal }

  ':'    { L _ Colon }
  '|'    { L _ Pipe  }
  
  '::'   { L _ TwoColons }
  '->'   { L _ Arrow }
  
  VARID  { L _ (Low _) }
  CONID  { L _ (Upp _) }

  BIN    { L _ (Bin _) }
  HEX    { L _ (Hex _) }
  DEC    { L _ (Dec _) }

  EOF    { L _ EOF }

%left '+' '-'
%left '*' '/'
%%

program : decls { P2Result (r $1) }

decls : decls ';' decl { $3 : $1  }
      | decls ';'      { $1 }
      | decl           { [$1] }
      | {- empty -}    { [] }

decl : function_decl      { $1 }
     | data_decl          { $1 }
     | function_type_decl { $1 }

function_decl : VARID pats function_expr { PFunc $1 (r $2) $3 }

pats : pats pat    { $2 : $1 }
     | pat         { [$1] }
     | {- empty -} { [] }
     
pat : VARID         { Pat $1 }
    | BIN           { Pat $1 }
    | HEX           { Pat $1 }
    | DEC           { Pat $1 }
    | CONID         { Pat $1 }
    | '(' ppat ')'  { $2 }
    
ppat : CONID pats   { ConsPat $1 (r $2) }
     | pat          { $1 }
     
function_expr : '=' expr   { PNoGuards $2    }
              | '|' guards { PGuards  (r $2) }
              
guards : guards '|' guard { $3 : $1  }
       | guards '|'       { $1 }
       | guard            { [$1] }
       
guard : expr '=' expr { ($1, $3) }

aexprs : aexprs aexpr    { $2 : $1 }
       | aexpr           { [$1] }
       | {- empty -}     { [] }
      
expr : VARID aexprs { PApp $1 (r $2) }
     | CONID aexprs { PApp $1 (r $2) }
     | aexpr { $1 }
     
aexpr : VARID        { PAExpr $1 }
      | BIN          { PAExpr $1 }
      | HEX          { PAExpr $1 }
      | DEC          { PAExpr $1 }
      | CONID        { PAExpr $1 }
      | '(' expr ')' { $2 }
 
typeExpr : CONID atypes2 { PTApp $1 (r $2) }
         | VARID atypes2 { PTApp $1 (r $2) }
         | aType         { $1 }
         
aType : VARID   { PTAExpr $1 }
      | CONID   { PTAExpr $1 }
      | DEC     { PTAExpr $1 }
      | '(' typeExpr ')' { $2 }

function_type_decl : VARID '::' type_decl { PFType $1 $3 }

type_decl : btype '->' type_decl { PTArrow $1 $3 }
          | btype                { $1 }

btype : VARID atypes2            { PTApp $1 (r $2) }
      | CONID atypes2            { PTApp $1 (r $2) }
      | atype                    { $1 }
      
atype : CONID                    { PTAExpr $1 }
      | VARID                    { PTAExpr $1 }
      | DEC                      { PTAExpr $1 }
      | '(' type_decl ')'        { $2 }
      
vars : vars VARID    { $2 : $1 }
     | VARID         { [$1] }
     | {- empty -}   { [] }

constrs : constrs '|' constr { $3 : $1 }
        | constrs '|'        { $1 }
        | constr             { [$1] }

constr : CONID atypes { PConstr $1 $2 }

data_decl : data CONID '=' constrs { PDataType $2 (r $4) }

atypes : atype atypes { $1 : $2  }
       | {- empty-}   { [] }

atypes2 : atype atypes2 { $1 : $2  }
        | atype { [$1] }

{

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y
  
instance Ord a => Ord (L a) where
  compare (L _ x) (L _ y) = compare x y

data P2Result = P2Result [PDecl]
             deriving Show

data PDecl = PFunc LToken [Pat] PGuards
           | PFType LToken PFType
           | PDataType LToken [PConstr]
           deriving Show

data Pat = Pat LToken
         | ConsPat LToken [Pat]
         deriving Show

data PGuards = PNoGuards PExpr
             | PGuards [(PExpr,PExpr)]
             deriving Show

data PFType = PTApp LToken [PFType]
            | PTArrow PFType PFType
            | PTAExpr LToken
            deriving (Show, Eq)

data PExpr = PApp LToken [PExpr]
           | PAExpr LToken
           deriving Show

data PConstr = PConstr LToken [PFType]
             deriving Show


data PState = NoState deriving Show
type P a = StateT PState (Either String) a

------------- Top level 'parse' function

r :: [a] -> [a]
r = reverse

runPWithoutError :: ([LToken] -> P P2Result) -> [LToken] -> Either String P2Result
runPWithoutError p inp = evalStateT (p inp) initialState
  where initialState = NoState

runP :: ([LToken] -> P P2Result) -> [LToken] -> P2Result
runP p = catchEither . runPWithoutError p

parse :: [LToken] -> P2Result
parse = runP programParser

parse' :: [LToken] -> Either String P2Result
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
