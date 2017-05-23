module ToSystemC where

import Data.Map (fromList,toList)
import Control.Monad
import Data.List (nub,intercalate)
import Data.String (lines,unlines)
import Data.Char (toUpper)

import LexerCore
import Components
import Types
import TransformationMonad
import Aux

------------

topLevel :: [[Int]] -> TComp -> TM ()
topLevel lst (_, C _ _ inps out _ _) = do
  addSystemCFile ("main.cpp", mainContent)
  addSystemCFile ("top.h", topLevelContent)
  addSystemCFile ("testbench.h",testbenchContent)
  where mainContent
          = "#include \"systemc.h\"\n"
          ++ "#include \"top.h\"\n\n"
          ++ "int sc_main (int argc, char *argv[]) {\n"
          ++ "top t(\"t\");\n"
          ++ "sc_start(1000, SC_PS);\n"
          ++ "return 0;\n"
          ++"}"
          
        topLevelContent
          = "#include \"systemc.h\"\n"
          ++ "#include \"testbench.h\"\n"
          ++ "#include \"mainFunc.h\"\n\n"
          ++ "SC_MODULE(top) {\n\n"
          ++ "mainFunc m;\n"
          ++ "testbench tb;\n"
          ++ signals inps [out] ++ "\n\n"
          ++ "SC_CTOR(top) : m(\"m\"), tb(\"tb\") {\n"
          ++ topConnections inps
          ++ topConnections [out] ++ "\n\n"
          ++ "}\n"
          ++ "};"
          
        topConnections
          = unlines
          . map (\p -> "m." ++ p ++ "(" ++ p ++ "); "
                       ++ "tb." ++ p ++ "(" ++ p ++ ");")
        
        signals is os
          = unlines
          $ map (\x -> "sc_fifo<int> " ++ x ++ ";") (is ++ os)
          
        testbenchContent
          = "#include \"systemc.h\"\n\n"
          ++ "SC_MODULE(testbench) {\n"
          ++ inputs [out]
          ++ outputs inps
          ++ "\n\nvoid proc();\n\n"
          ++ "SC_CTOR(testbench) {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void testbench::proc() {"
          ++ fillFifos inps lst ++ "\n\n"
          ++ "}"

        fillFifos :: [Input] -> [[Int]] -> String
        fillFifos inps tbs
          | length inps /= length tbs
             = error "Testbench does not match number of inputs of main function (" ++ show (length inps) ++ ")"
          | not (and (map ((length (head tbs) ==) . length) tbs))
             = error "Testbenches have different number of inputs"
          | otherwise = unlines (go 0)
          where go n
                  | n == length inps = []
                  | otherwise =
                      let vals = map (!!n) tbs
                          xs = zip inps vals
                      in map fill xs ++ [seeOut] ++ go (n+1)
                seeOut = "cout << out.read() << endl;"
                fill (i,v) = i ++ ".write(" ++ show v ++ ");"
                
-- faz o test bench e o top level
-- com base nos inputs e outputs da main

---------------

toSystemC :: [[Int]] -> TMM ()
toSystemC tbs = do
  comps <- getComponents
  maybeMain <- searchComponent "main"
  cont1 maybeMain $ \main -> do
    topLevel tbs main
    mapM componentToSystemC comps
    ret ()

componentToSystemC :: TComp -> TMM ()
componentToSystemC (name, C f insts inps out conns _proc) = do
  msc <- sconnections conns
  cont1 msc $ \sc -> do
    addSystemCFile (changeIfMain name ++ ".h", content sc)
    mapM_ instanceFromFile insts
    ret ()
  where
    content cs = unlines
      [ ifndef (changeIfMain name)
      , includeSystemC
      , includeInstances insts
      , ""
      , scModule name
      , inputs inps 
      , outputs [out]
      , intermediarySignals conns
      , instanceDeclaration insts
      , processDeclaration _proc
      , scCtor name insts
      , ""
      , cs
      , scThread _proc
      , closingBraces
      , voidProc name _proc
      , endif
      ]

toHeader :: Name -> String
toHeader name = map toUpper name ++ "_H_"

ifndef :: Name -> String
ifndef name =
  let pragma = toHeader name
  in unlines
     [ "#ifndef " ++ pragma
     , "#define " ++ pragma
     ]

endif :: String
endif = "#endif"

instanceFromFile :: TInst -> TM ()
instanceFromFile (comp,nid,inst,_) = case inst of
  ConstI      c out -> addSystemCFile $ makeConstFile nid c out
  SpecialI inps out -> addSystemCFile $ makeSpecialFile nid inps out
  ForkI n inp outs -> addSystemCFile $ makeForkFile nid n inp outs
  FifoI _ _ -> ok
  I     _ _ -> ok
  
includeSystemC :: String
includeSystemC = "#include \"systemc.h\""

includeInstances :: [TInst] -> String
includeInstances =
  unlines
  . map makeInclude
  . filter (/="")
  . nub
  . map getNameFromInstance
  where
    getNameFromInstance :: TInst -> Name
    getNameFromInstance (_,NameId name _,ins,_) = case ins of
      FifoI _ _ -> ""
      _ -> name

    makeInclude :: Name -> String
    makeInclude name = "#include \"" ++ name ++ ".h\""

scModule :: Name -> String
scModule name = "SC_MODULE(" ++ changeIfMain name ++ ") {"

inputs :: [Input] -> String
inputs = unlines . map makeInput
  where 
    makeInput inputName = "sc_fifo_in<int> " ++ inputName ++ ";"

outputs :: [Output] -> String
outputs = unlines . map makeOutput
  where
    makeOutput outputName = "sc_fifo_out<int> " ++ outputName ++ ";"

intermediarySignals :: [TConn] -> String
intermediarySignals = unlines . filter (/="") . nub . map intSig
  where intSig :: TConn -> String
        intSig (comp, (NameId m1 id1, p1), (NameId m2 id2, p2))
          | isInOrOut m1 ||
            isFifo m1    ||
            isInOrOut m2 ||
            isFifo m2 = ""
          | otherwise = "sc_fifo<int> "
                         ++ interSignal m1 id1 p1 m2 id2 p2
                         ++ ";"
          where isInOrOut x = x == comp'
                isFifo    x = x == "__fifo__"
                comp' = changeIfMain comp

interSignal m1 id1 p1 m2 id2 p2 =
  m1 ++ (show id1) ++ "_" ++ p1  ++ "__" ++ m2 ++ (show id2) ++ "_" ++ p2
  
instanceDeclaration :: [TInst] -> String
instanceDeclaration
  = unlines . map declInst
  where
    declInst :: TInst -> String
    declInst (_,NameId name id,inst,_) = case inst of
      FifoI _ _ -> "sc_fifo<int> __fifo__" ++ (show id) ++ ";"
      _         -> name ++ " " ++ name ++ (show id) ++ ";"

processDeclaration EndProc = ""
processDeclaration _ = "void proc();"

scCtor :: Name -> [TInst] -> String
scCtor name insts
  = "SC_CTOR(" ++ changeIfMain name ++ ")" ++ initInsts ++ " {"
  where initInsts = case filter (/="") (map initInst insts) of
                      []  -> ""
                      [x] -> " : " ++ x
                      xs  -> " : " ++ intercalate ", " xs
        --- parei aqui
        initInst (_,NameId n id,inst,_) = case inst of
          FifoI _ _ -> ""
          _         -> n ++ (show id) ++ "(\"" ++ n ++ (show id) ++ "\")"

sconnections :: [TConn] -> TMM String
sconnections conns = do
  maybeTxts <- mapM connToString conns
  cont maybeTxts $ do
    let txts = map just maybeTxts
    ret (unlines txts)
  where
    connToString :: TConn -> TMM String
    connToString (comp,(NameId m1 id1,p1),(NameId m2 id2,p2))
      | (isInOrOut m1 || isFifo m1)
        && (isInOrOut m2 || isFifo m2) =
          throw (TErr
                 ImpossibleConnection
                 Nothing
                 ("Impossible connection between "
                  ++ m1 ++ (show id1)
                  ++ " and "
                  ++ m2 ++ (show id2))
                 NoLoc) >> noRet
      | isInOrOut m1 =
          ret $ m2++(show id2) ++ "." ++ p2 ++ "(" ++ p1 ++ ");"
      | isInOrOut m2 =
          ret $ m1++(show id1) ++ "." ++ p1 ++ "(" ++ p2 ++ ");"
      | isFifo m1 =
          ret $ m2++(show id2) ++ "." ++ p2 ++ "(" ++ m1++(show id1) ++ ");"
      | isFifo m2 =
          ret $ m1++(show id1) ++ "." ++ p1 ++ "(" ++ m2++(show id2) ++ ");"
      | otherwise =
          let sig = interSignal m1 id1 p1 m2 id2 p2 
          in ret $ unlines [
             m1++(show id1) ++ "." ++ p1 ++ "(" ++ sig ++ ");"
            ,m2++(show id2) ++ "." ++ p2 ++ "(" ++ sig ++ ");"
            ]
      where isInOrOut x = x == comp
            isFifo    x = x == "__fifo__"
            comp' = changeIfMain comp

changeIfMain :: Name -> Name            
changeIfMain name = if name == "main" then "mainFunc" else name
    
scThread EndProc = ""
scThread _ = ""

closingBraces = "}\n};"

voidProc name process = ""

makeForkFile :: NameId -> Int -> Input -> [Output] -> File
makeForkFile (NameId name id) n inp outs =
  (name ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ "int in_aux;\n"
          ++ "sc_fifo_in<int> in;\n"
          ++ (unlines
              (map (\o -> "sc_fifo_out<int> "++ o ++";") outs))
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++ "in_aux = in.read();\n"
          ++ (unlines
              (map (\o -> o ++".write(in_aux);") outs))
          ++ "}\n"
          ++ "}\n"
          ++ endif

makeConstFile (NameId name id) cons out =
  (name ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ "sc_fifo_out<int> " ++ out ++ ";\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++ out ++ ".write(" ++ show cons ++ ");\n"
          ++ "}\n"
          ++ "}\n"
          ++ endif

makeSpecialFile (NameId name id) [in1,in2] out
  = (c ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ inputs [in1,in2]
          ++ outputs [out] ++ "\n\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++  out ++ ".write(" ++ in1 ++ ".read()" ++ symbol ++ in2 ++ ".read()" ++ ");\n"
          ++ "}\n"
          ++ "}\n"
          ++ endif
        symbol = case name of
          "mul" -> "*"
          "add" -> "+"
          "sub" -> "-"
          _ -> "&&"
