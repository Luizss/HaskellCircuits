module Lib where

import Control.Monad.Except
import Data.List.Split
import Control.Monad.State

import Lexer
import Layout (layout)
import Parser

getTestInputs :: IO [String]
getTestInputs = do
  contents <- readFile "testLex"
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
    putStr inp              -- input
    print (map getTok tks)  -- tokenize
    print (map getTok tks') -- tokenize + layout
    print expr              -- tokenize + layout + parse
    putStrLn "========================="
  return ()
