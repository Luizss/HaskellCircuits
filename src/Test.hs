module Test where

import Control.Monad.Except
import Data.List.Split
import Control.Monad.State
import System.Environment
import System.Directory

import Types
import LexerCore
import LayoutCore (layout)
import ParserCore
import Function
import TypeCheck
import TransformationMonad

test :: IO ()
test = do
  contents <- readFile "test/test"
  let tks  = tokenize contents
      tks' = layout tks
      expr = parse' tks'
      transformation = do
        case expr of
          Right e -> do
            putSourceCode contents
            putProgram e
          Left  _ -> return ()
        interpret
        createContext
        --checkForArityErrs
        typeCheck
  putStrLn "Content"
  putStrLn contents
  putStrLn "Tokens"
  print tks
  putStrLn "Layout"
  print tks'
  putStrLn "Parser"
  print expr

  putStrLn "Transformation"
  let st = runTM transformation
  when (getErrs st == []) $ do
    print (tFuncs st)
    showE st
    putStrLn "Ok!"
  when (getErrs st /= []) $ do
    showE st
    
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
