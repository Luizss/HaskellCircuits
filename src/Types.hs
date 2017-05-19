module Types where

import Control.Monad.State (State(..))
import Data.Map hiding ((\\), map, findIndex)

import LexerCore
import ParserCore

type Name = String

--- from Functions.hs

type FSymbol = String
type FVars = [L Name]

data F = F FVars FExpr
       | SpecialF
       deriving (Show,Eq)
data FVarCons = FVar (L Name)
              | FCons (L Int)
              deriving (Show,Eq)
data FExpr = FApp (L Name) [FExpr]
           | FAExpr FVarCons
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

instance Eq a => Eq (L a) where
  L _ x == L _ y = x == y
  
instance Ord a => Ord (L a) where
  compare (L _ x) (L _ y) = compare x y

type Arity = Int
type TFunc = (Name, SrcLoc, F, Arity)
type TComp = (Name, C)
type TInst = (CompName, NameId, I, Used)

specialFuncs
  = [("add",NoLoc,SpecialF,2)
    ,("sub",NoLoc,SpecialF,2)
    ,("mul",NoLoc,SpecialF,2)
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
  , components :: [TComp]
  , instances :: [TInst]
  , connections :: [TConn]
  , systemC :: SystemC
  } deriving Show

initialTState :: TState
initialTState = TState {
  sourceCode = ""
  , actualStage = InitialStage
  , program = Program []
  , tLogs   = []
  , tFuncs  = specialFuncs
  , components = []
  , instances = []
  , connections = []
  , systemC = []
  }

type TM a = State TState a

type TMM a = TM (Maybe a)

------ Components

type Input  = String
type Output = String
type Signal = String -- input or output

type TConn = (CompName,(NameId,Signal),(NameId,Signal))
data Proc = Get String Proc
          | Put String String Proc
          | EndProc
          deriving Show

data I = I [Input] Output
       | ConstI Int Output
       | SpecialI [Input] Output
       | FifoI Input Output
       deriving Show

data C = C F [TInst] [Input] Output [TConn] Proc
       deriving Show

type Errors = [String]

type A a = State (Map (L Name) F, Map Name C, Errors) a
