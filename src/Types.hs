module Types where

import Control.Monad.State (State(..))
import Data.Map hiding ((\\), map, findIndex)

import LexerCore
import ParserCore

type Name = String

--- from Functions.hs

type FSymbol = String

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

--------

--- Types

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
             | SomeError
             deriving (Show,Eq)
data TErr = TErr ErrType (Maybe WhereMsg) Msg SrcLoc deriving (Show,Eq)

type Msg = String
type WhereMsg = Msg
data TLog = TLog Msg Stage
          | TLogErr TErr Stage
          | TLogDebug Msg Stage
          deriving (Show,Eq)

data NameType = NameType
              deriving (Show,Eq)
type TName = (NameType, Name)

type Arity = Int
type TFunc = (Name, SrcLoc, F, Arity)
type TFuncType = (Name, SrcLoc, [TypeExpr])
type TComp = (Name, C)
type TInst = (CompName, NameId, I, Used)

specialFuncs
  = [("add",NoLoc,SpecialF,2)
    ,("sub",NoLoc,SpecialF,2)
    ,("mul",NoLoc,SpecialF,2)
    
    ,("and",NoLoc,SpecialF,2)
    ,("or",NoLoc,SpecialF,2)
    
    ,("not",NoLoc,SpecialF,1)

    ,("equ",NoLoc,SpecialF,2)
    
    ,("sli",NoLoc,SpecialF,3)
    ,("cat",NoLoc,SpecialF,2)
    ]

data Stage = InitialStage
           | InterpretationStage
           deriving (Show,Eq)

type SourceCode = String

type Id = Int
data NameId = NameId Name Id
            deriving (Show, Eq)

type CompName = Name
type Used = Bool

type File = (Name, String)
type SystemC = [File]

data TState =
  TState {
  sourceCode :: SourceCode
  , actualStage :: Stage
  , program :: Program
  , tLogs   :: [TLog]
  , tFuncs  :: [TFunc]
  , tTypes :: [TFuncType]
  , components :: [TComp]
  , instances :: [TInst]
  , connections :: [TConn]
  , logicalConnections :: [Name]
  , systemC :: SystemC
  , timesForked :: [(CompName, String, Int)]
  } deriving Show

initialTState :: TState
initialTState = TState {
  sourceCode = ""
  , actualStage = InitialStage
  , program = Program []
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

type TM a = State TState a

type TMM a = TM (Maybe a)

------ Components

type Input  = (String, FType)
type Output = (String, FType)
type Signal = (String, FType) -- input or output

type TConn = (CompName,(NameId,Signal),(NameId,Signal))
type Proc = [ProcUnit]

data ProcUnit = GETINPUT (String, FType)
              | PUTOUTPUT String String
              | GET (String, FType)
              | PUT (String, FType) String
              | COND Int String
              | IF Int Proc
              | ELSEIF Int Proc
              | ELSE Proc
              deriving (Show, Eq)

data I = I [Input] Output
       | ConstBinI String Output
       | ConstHexI String Output
       | ConstDecI Int Output
       | SpecialI [Input] Output [Int]
       | FifoI Input Output
       | ForkI Int Input [Output]
       deriving (Show, Eq)

data C = C F [TInst] [Input] Output [TConn] Proc
       deriving Show

type Errors = [String]

type A a = State (Map (L Name) F, Map Name C, Errors) a
