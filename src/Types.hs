module Types where

------------------ External Imports

import Control.Monad.State (State(..))
import Data.Map hiding ((\\), map, findIndex)

------------------ Internal Imports

import LexerCore
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
           | Nat SrcLoc Int
           deriving (Show,Eq)

data FGuards = FGuards [(FExpr,FExpr)]
             | NoFGuards FExpr
             deriving (Show,Eq)

data F = F [(FVar, FType)] FGuards FType
       | SpecialF
       deriving (Show,Eq)

data FVarCons = FVar (L Name)
              | FCons FCons
              deriving (Show,Eq)

data FCons = FBin (L String)
           | FHex (L String)
           | FDec (L Int)
           deriving  (Show,Eq)

data FExpr = FApp (L Name) [FExpr] FType
           | FAExpr (FVarCons, FType)
           deriving (Show,Eq)

------------------ Transformation Monad

type TM a = State TState a

type TMM a = TM (Maybe a)

data TState =
  TState {
  sourceCode           :: SourceCode
  , actualStage        :: TStage
  , parsedResult       :: PResult
  , tLogs              :: [TLog]
  , tFuncs             :: [TFunc]
  , tTypes             :: [TFuncType]
  , components         :: [TComp]
  , instances          :: [TInst]
  , connections        :: [CConn]
  , logicalConnections :: [Name]
  , systemC            :: SystemC
  , timesForked        :: [(CompName, String, Int)]
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
             deriving (Show,Eq)

data TErr = TErr ErrType (Maybe WhereMsg) Msg SrcLoc
          deriving (Show,Eq)

data TLog = TLog Msg TStage
          | TLogErr TErr TStage
          | TLogDebug Msg TStage
          deriving (Show,Eq)

data FunctionClassification = LeftRecursive
                            | RightRecursive
                            | MultipleRecursive
                            | NonTerminatingRecursion
                            | NonRecursive
                            deriving (Show, Eq)
  
type TFunc = (Name, SrcLoc, F, Arity, FunctionClassification)

type TFuncType = (Name, SrcLoc, [PTypeExpr])

type TComp = (Name, C)

type TInst = (CompName, NameId, I, Used)

data TStage = TInitialStage
            | TInterpretationStage
            deriving (Show,Eq)

data NameId = NameId Name Id
            deriving (Show, Eq)

initialTState :: TState
initialTState = TState {
  sourceCode = ""
  , actualStage = TInitialStage
  , parsedResult = PResult []
  , tLogs   = []
  , tFuncs  = specialFuncs
  , tTypes  = []
  , components = []
  , instances = []
  , connections = []
  , logicalConnections = []
  , systemC = []
  , timesForked = []
  }

specialFuncs :: [TFunc]
specialFuncs
  = [("add",NoLoc,SpecialF,2,NonRecursive)
    ,("sub",NoLoc,SpecialF,2,NonRecursive)
    ,("mul",NoLoc,SpecialF,2,NonRecursive)
    ,("and",NoLoc,SpecialF,2,NonRecursive)
    ,("or" ,NoLoc,SpecialF,2,NonRecursive)
    ,("not",NoLoc,SpecialF,1,NonRecursive)
    ,("equ",NoLoc,SpecialF,2,NonRecursive)
    ,("sli",NoLoc,SpecialF,3,NonRecursive)
    ,("cat",NoLoc,SpecialF,2,NonRecursive)
    ]

------------------ Components

type FTyped a = (a, FType)

type CInput  = FTyped String
type COutput = FTyped String
type CSignal = FTyped String -- input or output

type CConn = (CompName,(NameId,CSignal),(NameId,CSignal))

type CProc = [CProcUnit]

data CProcUnit = GETINPUT (FTyped String)
               | PUTOUTPUT String String
               | GET (FTyped String)
               | PUT (FTyped String) String
               | COND Int String
               | IF Int CProc
               | ELSEIF Int CProc
               | ELSE CProc
               | LOOP CProc
               | BREAK
               | PUTSTATE String String
               deriving (Show, Eq)

data I = I [CInput] COutput
       | ConstBinI String COutput
       | ConstHexI String COutput
       | ConstDecI Int COutput
       | SpecialI [CInput] COutput [Int]
       | FifoI CInput COutput
       | ForkI Int CInput [COutput]
       deriving (Show, Eq)

data C = C F [TInst] [CInput] COutput [CConn] CProc
       deriving Show

------------------ System C

type File = (Name, String)

type SystemC = [File]

