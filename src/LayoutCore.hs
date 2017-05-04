module LayoutCore where

--- almost copy de Layout.hs

import LexerCore

-------------- Context 

type Context = Int -- indentation column number

-------------- Layout function

layout :: [LToken] -> [LToken]
layout tokens = semicolonProcess (lay markedTokens [])
  where markedTokens = markTokensAfterKeywords fixedTokens
        -- Because of lexer state change, location is
        -- wrong for strings. We need to fix that
        -- before layout
        fixedTokens = map decreaseStringCol tokens
        {-decreaseStringCol (L (SrcLoc f l c) (String s))
          | s /= "" = L (SrcLoc f l (c-1)) (String s)
        decreaseStringCol x = x-}
        decreaseStringCol = id
        
semicolonProcess :: [LToken] -> [LToken]
semicolonProcess = destroyFirstSemic . destroyDuplicatedSemics
  where destroyFirstSemic = tail
        destroyDuplicatedSemics ltoks = case ltoks of
          [] -> [] -- shouldn't happen
          [x] -> [x]
          L p Semic : L q Semic : xs
            -> destroyDuplicatedSemics (L (choose p q) Semic : xs)
          x:xs -> x : destroyDuplicatedSemics xs
        choose NoLoc x = x
        choose x NoLoc = x
        choose loc1@(SrcLoc f l1 c1) loc2@(SrcLoc _ l2 c2)
          | c1 < c2   = loc1
          | c2 < c1   = loc2
          | l1 < l2   = loc1
          | otherwise = loc2

-- Function based on:
-- https://www.haskell.org/onlinereport/syntax-iso.html
-- It puts '{', '}' and ';' depending on indentation
lay :: [Marked LToken] -> [Context] -> [LToken]
lay [] [] = []
lay [] (m:ms)

  {-
  | m /= 0 = L NoLoc RBrace : lay [] ms -}

  | m == 0 = error "No matching '}' for '{' found."
  
lay (x:xs) ms

  {-
  | tok == RBrace
    && hasContext
    && actual ms == 0 = getMarked x : lay xs (rest ms)  -- 7

  | tok == RBrace = error "No matching '{' for '}' found." -- 8

  | tok == LBrace = getMarked x : lay xs (0:ms)  -- 9

  | not marked -- adicionado por mim
    && hasContext
    && tok == In = noLoc RBrace : getMarked x : lay xs (rest ms)-}

  {-
  | not marked -- 1
    && hasContext
    && n == actual ms = noLoc Semic : getMarked x : lay xs ms 

  | not marked -- 2
    && hasContext
    && n < actual ms = noLoc RBrace : lay (x:xs) (rest ms) -}

  | not marked -- adicionado por mim
    && hasNoContext
    && n == 1 = noLoc Semic : getMarked x : lay xs ms 

  | not marked = getMarked x : lay xs ms  -- 3

  {-
  | marked -- 4
    && hasContext
    && n > actual ms = noLoc LBrace : getMarked x : lay xs (n:ms) 

  | marked -- 5
    && hasNoContext
    && n > 0 = noLoc LBrace : getMarked x : lay xs [n] 

  | marked = noLoc LBrace : noLoc RBrace : lay ((markFalse' x):xs) ms  -- 6

  | hasContext -- 10
    && actual ms /= 0 = noLoc RBrace : lay (x:xs) (rest ms)
   
  | otherwise = getMarked x : lay xs ms -- 11 -}

  where marked = fst x
        L loc tok = getMarked x
        n = col loc
        hasNoContext = ms == []
        hasContext = ms /= []
        
        rest = tail
        actual = head
        errorWithLoc _ = error
        markFalse' (_,x) = (False, x)
        col (SrcLoc _ _ c) = c
        noLoc tkn = L NoLoc tkn

-------------- Auxiliar type and functions

type Marked a = (Bool, a)

getMarked :: Marked a -> a
getMarked (_, x) = x

-- function marks tokens after 'where','let' and 'of'
markTokensAfterKeywords :: [LToken] -> [Marked LToken]
markTokensAfterKeywords = {- go .-} map markFalse
  where markFalse x = (False, x)
    {-go  [] = []
    go [x] = [x]
    go (x1 : x2 : rest) =
      let x2' | isKeyword (getTok (getMarked x1))
                && getTok (getMarked x2) /= RBrace = markTrue' x2
              | otherwise = x2
      in x1 : go (x2' : rest)

    isKeyword :: Token -> Bool
    isKeyword tk = case tk of
      Where -> True
      Let   -> True
      Of    -> True
      _     -> False

    markTrue' (_,x) = (True, x)
    -}
