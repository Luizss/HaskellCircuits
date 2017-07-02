{-# LANGUAGE FlexibleInstances #-}
module Test where

import System.Directory
import Control.Monad.Except
import Control.Monad (forM_)
import Layout2
import Lexer2
import Parser2
import Core
import TransformationMonad
import Types
import Aux
import TypeSynth
import Function2
import Components
import ToSystemC

a :: [[Int]] -> IO ()
a tbs = do
  tx <- readFile "test/test"
  let tks = tokenize tx
      tks' = layout tks
      Right expr = parse' tks'
      transformation = do
        storeDataInState expr
        storeCoreInState expr
        putDataDefsInState
        putFunctionTypesInState
        c <- getCore
        debug "!!!!!!!!!"
        debugs c
        x <- getCFuncTypes
        debug "AAAAAAAAA"
        debugs x
        debugs "TYPECHECK"
        typeCheck
        debugs "TYPESYNTH"
        typeSynth
        tc<-getTCore
        debugs tc
        getParsedFunctions_TransformToF_AddToState
        checkForArityErrs
        hey<-getFunctions
        debug "FUNCTION"
        debugs hey
        applyHighOrder
        debug "COMPONENTS"
        toComponents
        comp <- getComponents
        debugs comp
        toSystemC tbs
      st = runTM transformation
  putStrLn "TOKENS"
  print tks
  putStrLn "LAYOUT TOKENS"
  print tks'
  putStrLn "EXPR"
  print expr
  putStrLn "CORE"
  showE st
  putStrLn "SYST"
  print $ systemC st
  when (getErrs st == []) $ do
    print st
    forM_ (systemC st) $ \(x,y) -> do
      putStrLn x
      putStrLn y
    makeSystemC "test" (systemC st)
    showE st
    putStrLn "Ok!"
  when (getErrs st /= []) $ do
    print tks
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
