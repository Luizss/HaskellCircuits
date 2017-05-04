module Lib where

import Control.Monad.Except
import Data.List.Split
import Control.Monad.State

import LexerCore
import LayoutCore (layout)
import ParserCore
import Function
import Components
import ToSystemC

--import Layout (layout)
--import Parser

getTestInputs :: IO [String]
getTestInputs = do
  contents <- readFile "test/testLexCore"
  let (first:spliteds) = "====" `splitOn` contents
  return (first : map tail spliteds)

test :: IO ()
test = do
  inps <- getTestInputs
  putStrLn "========================="
  forM inps $ \inp -> do
    let tks  = tokenize inp
        tks' = layout tks
        expr = parse' tks'
        f = case expr of
          Right e -> interpret e
          Left _ -> []
        c = toComponents f
        g = case c of
          Right e -> toSystemC [1..10] (map snd e)
          Left _ -> []
    putStr inp              -- input
    print (map getVal tks)  -- tokenize
    print (map getVal tks') -- tokenize + layout
    print expr              -- tokenize + layout + parse
    print f
    putStrLn ""
    print c
    if g /= []
      then forM_ g $ \(x,y) -> do
        putStrLn x
        putStrLn y
      else putStr "aa"
    makeSystemC g
  --  putStrLn "========================="
  return ()
