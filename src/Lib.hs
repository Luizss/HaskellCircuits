module Lib where

---------------- External Imports

import Control.Monad.Except
import Data.List.Split
import Control.Monad.State
import System.Environment
import System.Directory

---------------- Internal Imports

import LexerCore
import LayoutCore (layout)
import ParserCore
import TransformationMonad
import Function
import Types
import Components
import ToSystemC

getTestInputs :: IO [String]
getTestInputs = do
  contents <- readFile "test/test"
  let (first:spliteds) = "====" `splitOn` contents
  return (first : map tail spliteds)

userInterface :: IO ()
userInterface = do
  args <- getArgs
  when (length args < 1) $
    error "At least filename must be suplied."
  let filePath : tbs' = args
      tbs = map read tbs' :: [[Int]]
  withFile filePath tbs

withFile :: FileName -> [[Int]] -> IO ()
withFile filePath tbs = do
  contents <- readFile filePath
  withText filePath contents tbs

test :: FileName -> [[Int]] -> IO ()
test filePath tbs = do
  contents <- readFile filePath
  test' filePath contents tbs

test' :: FileName -> String -> [[Int]] -> IO ()
test' fileName text tbs = do
  let tks  = tokenize text
      tks' = layout tks
      expr = parse' tks'
      transformation = do
        case expr of
          Right e -> do
            putSourceCode text
            putParsedResult e
          Left  _ -> return ()
        getParsedFunctions_TransformToF_AddToState
        checkForArityErrs
        toComponents
        toSystemC tbs
      st = runTM transformation
  when (getErrs st == []) $ do
    print st
    forM_ (systemC st) $ \(x,y) -> do
      putStrLn x
      putStrLn y
    makeSystemC fileName (systemC st)
    showE st
    putStrLn "Ok!"
  when (getErrs st /= []) $ do
    print tks
    showE st

withText :: FileName -> String -> [[Int]] -> IO ()
withText fileName text tbs = do
  let tks  = tokenize text
      tks' = layout tks
      expr = parse' tks'
      transformation = do
        case expr of
          Right e -> do
            putSourceCode text
            putParsedResult e
          Left  _ -> return ()
        getParsedFunctions_TransformToF_AddToState
        checkForArityErrs
        toComponents
        toSystemC tbs
      st = runTM transformation
  when (getErrs st == []) $
    makeSystemC fileName (systemC st)
  when (getErrs st /= []) $ do
    showE st

makeSystemC :: FileName -> SystemC -> IO ()
makeSystemC dirName files = do
  let dir = "./" ++ dirName ++ "_result" ++ "/"
  doesIt <- doesDirectoryExist dir
  case doesIt of
    True -> do
      removeDirectoryRecursive dir
      createDirectory dir
      forM_ files $ \(filename, content) ->
        writeFile (dir ++ filename) content
      putStrLn "Ok!"
    False -> do
      createDirectory dir
      forM_ files $ \(filename, content) ->
        writeFile (dir ++ filename) content

a :: IO ()
a = do
  tx <- readFile "test/test"
  let tks = tokenize tx
      tks' = layout tks
      expr = parse' tks'
      transformation = do
        case expr of
          Right e -> do
            putSourceCode tx
            putParsedResult e
          Left  _ -> return ()
        getParsedFunctions_TransformToF_AddToState
        checkForArityErrs
        toComponents
      st = runTM transformation
  putStrLn "TOKENS"
  print tks
  putStrLn "LAYOUT TOKENS"
  print tks'
  putStrLn "EXPR"
  print expr
  putStrLn "FUNCTION"
  print (tFuncs st)
  putStrLn "COMPONENT"
  print (components st)
  when (getErrs st == []) $ do
    showE st
    putStrLn "NICE!"
  when (getErrs st /= []) $ do
    showE st
  
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
