module Types where

------------------ External Imports

import Control.Monad.State (State(..))
import Data.Map hiding ((\\), map, findIndex)

------------------ Internal Imports

import Parser2 as P2
import Lexer2 as L2
import LexerCore hiding (L(..), SrcLoc(..))
import ParserCore

------------------ Aliases

type Name       = String
type Msg        = String
type WhereMsg   = Msg
type Arity      = Int
type CompName   = Name
type Used       = Bool
type SourceCode = String
type Id         = Int

------------------ Function

type FVar = L Name

data FType = BitVec SrcLoc Int
           | Bit SrcLoc
           | Function SrcLoc
           | Nat SrcLoc Int
           | Stream FType
           deriving (Show,Eq)

data FGuards = FGuards [(FExpr,FExpr)]
             | NoFGuards FExpr
             deriving (Show,Eq)

data F = F [(FVar, FType)] FGuards FType
       | SpecialF
       deriving (Show,Eq)

data FVarCons = FVar FVar
              | FCons FCons
              deriving (Show,Eq)

data FCons = FBin (L String)
           | FHex (L String)
           | FDec (L Int)
           | FForeverWait
           deriving  (Show,Eq)

data FExpr = FApp (L Name, Id) [FExpr] FType
           | FAExpr (FVarCons, Id, FType)
           deriving (Show,Eq)

------------------ Transformation Monad

type TM a = State TState a

type TMM a = TM (Maybe a)


data TState =
  TState {
  sourceCode           :: SourceCode
  , dataDecls          :: [(L Name, [CConstr])]
  , funcTypes          :: [(Name, [Constraint], [CFType])]
  , typeCheckState     :: [[CFType]]
  , actualStage        :: TStage
  , parsedResult       :: PResult
  , tLogs              :: [TLog]
  , tFuncs             :: [TFunc]
  , tTypes             :: [TFuncType]
  , functionIds        :: [(Name, Id, [FType])]
  , components         :: [TComp]
  , instances          :: [TInst]
  , connections        :: [CConn]
  , logicalConnections :: [Name]
  , logicalOutputs     :: [(Name, Name, FType, Maybe Int)]
  , systemC            :: SystemC
  , timesForked        :: [(CompName, String, Int)]
  , core               :: Core
  } deriving Show

data ErrType = ErrConstantAsFunction
             | ArityMismatch
             | FunctionNotDeclared
             | ComponentNotDone
             | UniqueInstanceNotFound
             | ModifyUnexistingInstance
             | AllInstancesUsed
             | NoConnectionsForComponent
             | CouldntGetNextInstance
             | VariableNotInScope
             | WrongInstanceNumberInput
             | ConstantsHaveNoInputs
             | ImpossibleConnection
             | ExpressionConstructionErr
             | TypeNotPermitted
             | CannotSynth
             | RecursionWithoutCondition
             | CantMatchTypes
             deriving (Show,Eq)

data TErr = TErr ErrType (Maybe WhereMsg) Msg SrcLoc
          deriving (Show,Eq)

data TLog = TLog Msg TStage
          | TLogErr TErr TStage
          | TLogDebug Msg TStage
          deriving (Show,Eq)

type IsConsExpr = Bool
data RecursionClassification = LeftRecursive
                             | RightRecursive
                             | MultipleRecursive
                             | NonTerminatingRecursion
                             | NonRecursive
                             deriving (Show, Eq)
data TypeClassification = OutputRecursive
                        | InputRecursive
                        | OutputInputRecursive
                        | NoRecursiveTypes
                        deriving (Show, Eq)
type FunctionClassification
  = (RecursionClassification, TypeClassification, IsConsExpr)

type HighOrder = [Int]

type TFunc = (Name, SrcLoc, F, Arity, FunctionClassification, HighOrder)

type TFuncType = (Name, SrcLoc, [PTypeExpr])

type TComp = (Name, C)

type TInst = (CompName, Id, NameId, I, Used)

data TStage = TInitialStage
            | TInterpretationStage
            deriving (Show,Eq)

data NameId = NameId Name Id
            deriving (Show, Eq)

initialTState :: TState
initialTState = TState {
  sourceCode = ""
  , dataDecls = []
  , funcTypes = preDefinedfunctionsTypes
  , typeCheckState = []
  , actualStage = TInitialStage
  , parsedResult = PResult []
  , tLogs   = []
  , tFuncs  = specialFuncs
  , tTypes  = []
  , functionIds = []
  , components = []
  , instances = []
  , connections = []
  , logicalConnections = []
  , logicalOutputs = []
  , systemC = []
  , timesForked = []
  , core = Core []
  }

type Constraint = (Name,Name)

preDefinedfunctionsTypes :: [(Name, [Constraint], [CFType])]
preDefinedfunctionsTypes
  = [("True" ,[],[ty "Bool"])
    ,("False",[],[ty "Bool"])
    ,("and"  ,[],[ty "Bool", ty "Bool", ty "Bool"])
    ,("or"   ,[],[ty "Bool", ty "Bool", ty "Bool"])
    ,("not"  ,[],[ty "Bool", ty "Bool"])
    ,("equ"  ,[("NotRec","a")],[tyvar "a", tyvar "a", ty "Bool"])
    ,("add"  ,[("Num","a")],[tyvar "a", tyvar "a", tyvar "a"])
    ,("sub"  ,[("Num","a")],[tyvar "a", tyvar "a", tyvar "a"])
    ,("mul"  ,[("Num","a")],[tyvar "a", tyvar "a", tyvar "a"])
    ,("div"  ,[],[fixed "m" "n", fixed "m" "n", fixed "m" "n"])
    ]
  where ty    = CTAExpr . L NoLoc . L2.Upp
        tyvar = CTAExpr . L NoLoc . L2.Low
        fixed m n
          = CTApp (L NoLoc (L2.Upp "Fixed")) [tyvar m,tyvar n]
        int n
          = CTApp (L NoLoc (L2.Upp "Int")) [tyvar n]

specialFuncs :: [TFunc]
specialFuncs
  = [("add",NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("sub",NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("mul",NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("and",NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("or" ,NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("not",NoLoc,SpecialF,1,(NonRecursive, NoRecursiveTypes, False),[])
    ,("equ",NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("sli",NoLoc,SpecialF,3,(NonRecursive, NoRecursiveTypes, False),[])
    ,("cat",NoLoc,SpecialF,2,(NonRecursive, NoRecursiveTypes, False),[])
    ,("cons",NoLoc,SpecialF,2
     ,(NonRecursive,OutputInputRecursive, False),[])
    ,("consR",NoLoc,SpecialF,2
     ,(NonRecursive,OutputInputRecursive, False),[])
    ,("now",NoLoc,SpecialF,2
     ,(NonRecursive,InputRecursive, False),[])
    ,("rest",NoLoc,SpecialF,2
     ,(NonRecursive,OutputInputRecursive, False),[])
    ,("mrest",NoLoc,SpecialF,2
     ,(NonRecursive,OutputInputRecursive, False),[])
    ]

------------------ Components

type FTyped a = (a, FType)

type CInput  = FTyped String
type COutput = FTyped String
type CSignal = FTyped String -- input or output

type CConn = (CompName,(NameId,CSignal),(NameId,CSignal))

type CProc = [CProcUnit]

data CProcUnit = GETINPUT (FTyped String)
               | GETSTREAMSAFE (Int,Int) Int (FTyped String)
               | GETSTREAMV Int (FTyped String)
               | PUTSTREAM Int (FTyped String) String
               | SWITCH (FTyped String) Int Int
               | PUTOUTPUT String String
               | PUTOUTPUTSTREAM Int String String
               | PUTOUTPUTSTREAMV FType Int String String
               | GET (FTyped String)
               | PUT (FTyped String) String
               | PUTV (FTyped String) String
               | COND Int String
               | IF Int CProc
               | ELSEIF Int CProc
               | ELSE CProc
               | LOOP CProc
               | BREAK
--               | DESTROY [(String, FType)]
               | DESTROY Int (String, FType)
               | DESTROYV (String, FType)
               | PUTSTATE String String
               | SAVE (FTyped String)
               | SAVEV (FTyped String)
               | COPY FType String String
               | PCOPY Int FType String String
               | COPYV FType String String
               | MAKEV FType String String
               | CLEARV String
               | RESTV String
               | BLOB Int
               deriving (Show, Eq)

data TransitionType = ConsRTransition Int
                    | RestTransition
                    | ConsTransition Int
                    | IdTransition
                    | FunctionTransition
                    deriving Show

data I = I [CInput] COutput
       | ConstBinI String COutput
       | ConstHexI String COutput
       | ConstDecI Int COutput
       | ConstStrI [FCons] COutput
       | SpecialI [CInput] COutput [Int]
       | FifoI CInput COutput
       | ForkI Int CInput [COutput]
       deriving (Show, Eq)

data C = C F [TInst] [CInput] COutput [CConn] CProc
       deriving Show

------------------ System C

type File = (Name, String)

type SystemC = [File]

------------- core

data Core = Core [CFunc]
          deriving Show

data CFunc = CFunc L2.LToken [L2.LToken] CGuards [CFType]
           -- data colocada no estado
           deriving Show

data CGuards = CNoGuards P2.PExpr
             | CGuards [(P2.PExpr,P2.PExpr)]
             deriving Show

data CFType = CTApp L2.LToken [CFType]
            | CTArrow SrcLoc [CFType]
            | CTAExpr L2.LToken
            deriving (Show, Eq)

data CConstr = CConstr L2.LToken [CFType]
             deriving Show

------- typedcore

data TCore = TCore [TCFunc]
           deriving Show

data TCFunc = TCFunc L2.LToken [(L2.LToken,CFType)] TCGuards CFType
           deriving Show

data TCGuards = TCNoGuards TCExpr
              | TCGuards [(TCExpr,TCExpr)]
              deriving Show

data TCExpr = TCApp L2.LToken [TCExpr] CFType
            | TCAExpr (L2.LToken,CFType)
            deriving Show
