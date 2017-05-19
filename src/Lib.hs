module Lib where

import Control.Monad.Except
import Data.List.Split
import Control.Monad.State

import LexerCore
import LayoutCore (layout)
import ParserCore
import TransformationMonad
import Function
import Components
import Types
import ToSystemC

--import Layout (layout)
--import Parser

getTestInputs :: IO [String]
getTestInputs = do
  contents <- readFile "test/test"
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
        f = do
          case expr of
            Right e -> do putSourceCode inp; putProgram e
            Left  _ -> return ()
          interpret
          checkForArityErrs
          toComponents
          toSystemC
       {- g = case c of
          Right e -> 
          Left _ -> []-}
    putStr inp              -- input
    print (map getVal tks)  -- tokenize
    print (map getVal tks') -- tokenize + layout
    print expr              -- tokenize + layout + parse
    putStrLn "LeN======"
    print (length (filter (\(_,_,c,_)->c/=SpecialF) (tFuncs (runTM f))))
    let st = runTM f
    putStrLn "EXEC======"
    print st
    putStrLn "Err======"
    showE st
    forM_ (systemC st) $ \(x,y) -> do
        putStrLn x
        putStrLn y
    if getErrs st == []
      then makeSystemC (systemC st)
      else return ()
    {-putStrLn ""
    print c
    if g /= []
      then 
      else putStr "aa"
    makeSystemC g-}
  --  putStrLn "========================="
  return ()

getErrs = filter isErr . tLogs
  where isErr x = case x of
          TLogErr _ _ -> True
          _ -> False

showE :: TState -> IO ()
showE state = do
  forM_ (reverse (tLogs state)) $ \log -> case log of
    TLogErr terr _ -> putStrLn $ show terr
    TLogDebug msg _ -> putStrLn msg
    _ -> return ()
