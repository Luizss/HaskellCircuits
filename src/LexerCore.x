{

{-# OPTIONS_GHC -w #-}

module LexerCore where

import Codec.Binary.UTF8.String (encodeChar)
import Data.Word (Word8)
import Data.Bits (shiftR, shiftL)
import Control.Monad.State (StateT(..), evalStateT, get, put)

}

$digit    = 0-9
$alphaLow = [a-z]
$alphaUpp = [A-Z]
$alpha    = [$alphaLow $alphaUpp]
$eol      = [\n]
$symbol   = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]

@decimal  = $digit+
@exponent = [eE] [\-\+]? @decimal
@fpoint   = @decimal \. @decimal @exponent? | @decimal @exponent

haskell :-

<lexComment> {

  "-}"          { changeState 0 }
  [.\n]         ;

}

<0> {

  $eol                             ;
  $white+                          ;

  "--".*                           ;
  "{-"                             { changeState lexComment }
  "->"                             { consLoc RArrow }
  "::"                             { consLoc DColon }
  
  \(                               { consLoc LParen }
  \)                               { consLoc RParen }

  \;                               { consLoc Semic }

  \=                               { consLoc Equal }

  $symbol+                              { funLoc (\s -> Sym s) }
  [$alphaLow \_] [$alpha $digit \_ \']* { funLoc (\s -> Low s) }
  $alphaUpp [$alpha $digit \_ \']*      { funLoc (\s -> Upp s) }
  -- [\-]? @fpoint                      { funLoc (\s -> Real (read s)) }
  [\-]? @decimal                        { funLoc (\s -> Int (read s)) }

}


{

--------------- Token type
data Token
  = LParen
  | RParen
  | Equal

  | RArrow
  | DColon
  
  | Low String
  | Upp String
  | Sym String

  | Semic

  | Int Int

  | EOF
  deriving (Eq,Show)

--------------- Data types

type Byte = Word8
type Bytes = [Byte]
type FileName = String
type Line = Int -- caracter count
type Col  = Int -- caracter count

data SrcLoc = SrcLoc FileName !Line !Col
            | NoLoc
            deriving (Show, Eq, Ord)
data Buffer = Buf {
  getPrevious :: Char,
  getBytes :: Bytes,
  getString :: String
}

type AlexState = Int
data AlexInput = AI SrcLoc Buffer AlexState

data L a = L {
  getLoc :: SrcLoc,
  getVal :: a
} deriving Show
type LToken = L Token

instance Functor L where
  fmap f (L s x) = L s (f x)

-- If token is Nothing thenoutput can be skipped
data AlexResult = AR (Maybe Token) SrcLoc AlexState deriving Show

type Action = String -> SrcLoc -> AlexState -> AlexResult

--------------- Lexing Monad

-- It was a P monad (parsing) but things changed
type T a = StateT AlexInput (Either String) a

runT :: T a -> String -> a
runT p = catchEither . runTWithoutError p

runTWithoutError :: T a -> String -> Either String a
runTWithoutError p str = evalStateT p initialState
  where initialState = AI (SrcLoc "" 1 1) (Buf '\n' [] str) 0

errorT :: String -> T a
errorT err = StateT (\s -> Left err)

--------------- Alex functions

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (AI loc buf st) =
  case buf of
    Buf c (b:bs) s -> Just (b, AI loc (Buf c bs s) st)
    Buf _ [] (c:s) -> let (b:bs) = encodeChar c
                          newLoc = nextSrcLoc loc c
                      in Just (b, AI newLoc (Buf c bs s) st)
    Buf c [] []    -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ (Buf c _ _) _)  = c

--------------- Top level 'tokenize' function

{- Explanation
The monadic function 'readToken' was implemented but is not used.
Instead, the parser uses [LToken] as input.
This way we can use the 'layout' function between lexing and parsing.
The layout function could be implemented inside readToken but for now
it's implemented as [LToken] -> [LToken]
-}

tokenize :: String -> [LToken]
tokenize = runT collectAll
  where
    collectAll :: T [LToken]
    collectAll = go []
      where
        go xs = do
          L loc tok <- readToken
          if tok == EOF
            then return (reverse xs)
            else go (L loc tok : xs)

readToken :: T LToken
readToken = do
  inp@(AI loc buf lexState) <- get
  case alexScan inp lexState of
    AlexEOF            -> return (L NoLoc EOF)
    AlexError _        -> errorT  "Invalid lexeme."
    AlexSkip  inp' len -> do 
      put inp'                        -- changes input
      readToken                       -- ignores output
    AlexToken (AI loc' buf' _) len tokenize ->
      case tokenize (take len (getString buf)) loc lexState of 
        AR Nothing _ newState -> do 
          put (AI loc' buf' newState) -- changes input state
          readToken                   -- ignores output
        alexRes@(AR _ _ newState) -> do
          put (AI loc' buf' newState) -- changes input
          return (toLToken alexRes)   -- outputs LToken
    where toLToken :: AlexResult -> LToken
          toLToken (AR (Just tok) loc _state) = L loc tok
          toLToken _ = error "This shouldn't happen."


--------------- Action functions

consLoc :: Token -> Action
consLoc t _ l st = AR (Just t) l st

funLoc :: (String -> Token) -> Action
funLoc tf s l st = AR (Just (tf s)) l st

changeState :: Int -> Action
changeState newState s l _ = AR Nothing NoLoc newState

changeStateAndOutput :: Int -> (String -> Token) -> Action
changeStateAndOutput newState tf s l _ = AR (Just (tf s)) l newState

--------------- Auxiliar functions

-- Function is more permisive than ideal because it lets
-- newlines in strings, but it doesnt break the language
-- so its ok
unifyMultilineString :: String -> String
unifyMultilineString = reverse . go "" False
  where go :: String -> Bool -> String -> String

        go buf False "" = buf
        go buf True  "" = error "A '\\' is missing inside multiline string."

        go buf False ('\\':cs) = go buf     True  cs
        go buf False ('\n':cs) = error "Multiline string error."
        go buf False    (c:cs) = go (c:buf) False cs

        go buf True ('\\':cs) = go buf False cs
        go buf True    (c:cs) = go buf True  cs

nextSrcLoc :: SrcLoc -> Char -> SrcLoc
nextSrcLoc (SrcLoc f l c) char = case char of
  '\n' -> SrcLoc f (l + 1) 1
  '\t' -> SrcLoc f l (((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1)
  _    -> SrcLoc f l (c + 1)

catchEither :: Either String a -> a
catchEither e = case e of
  Left  err -> error err
  Right res -> res

}
