module ToSystemC where

import Data.Map (fromList,toList)
import Control.Monad
import Data.List (nub,intercalate,isPrefixOf)
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
          . map (\(p,_) -> "m." ++ p ++ "(" ++ p ++ "); "
                           ++ "tb." ++ p ++ "(" ++ p ++ ");")
        
        signals is os
          = unlines
          $ map (\(x,t) -> makeTypeSignal t ++ " " ++ x ++ ";") (is ++ os)
          
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

        fillFifos :: [CInput] -> [[Int]] -> String
        fillFifos inps tbs
          | length inps /= length tbs
             = error "Testbench does not match number of inputs of main function (" ++ show (length inps) ++ ")"
          | not (and (map ((length (head tbs) ==) . length) tbs))
             = error "Testbenches have different number of inputs"
          | otherwise = unlines (go 0)
          where go n
                  | n == length (last tbs) = []
                  | otherwise =
                      let vals = map (!!n) tbs
                          xs = zip inps vals
                      in map fill xs ++ [seeOut] ++ go (n+1)
                seeOut = "cout << out.read() << endl;"
                fill ((i,_),v) = i ++ ".write(" ++ show v ++ ");"
                
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
componentToSystemC (name, C f insts inps out conns proced) = do
  msc <- sconnections conns
  cont1 msc $ \sc -> do
    addSystemCFile (changeIfMain name ++ ".h", content sc)
    mapM_ instanceFromFile insts
    debug "sigs"
    debugs proced
    debug (procedureSignals proced)
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
      , procedureSignals proced
      , processDeclaration proced
      , scCtor name insts
      , ""
      , cs
      , scThread proced
      , closingBraces
      , procedureToSystemC name proced
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
  ConstBinI bin out      -> addSystemCFile $ makeConstBinFile nid bin out
  ConstHexI hex out      -> addSystemCFile $ makeConstHexFile nid hex out
  ConstDecI dec out      -> addSystemCFile $ makeConstDecFile nid dec out
  SpecialI inps out args -> addSystemCFile $ makeSpecialFile nid inps out args
  ForkI n inp outs       -> addSystemCFile $ makeForkFile nid n inp outs
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

inputs :: [CInput] -> String
inputs = unlines . map makeInput
  where 
    makeInput (inputName, t) = makeTypeInput t ++ " " ++ inputName ++ ";"

makeTypePure :: FType -> String
makeTypePure ty = case ty of
  BitVec l n -> "sc_lv<" ++ show n ++ ">"
  Bit    l   -> "sc_lv<1>"
  Nat    l n -> error $ "nat?1 " ++ show l ++ " : " ++ show n

makeTypeSignal :: FType -> String
makeTypeSignal ty = case ty of
  BitVec l n -> "sc_fifo<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "sc_fifo<sc_lv<1> >"
  Nat    l n -> error $ "nat?2 " ++ show l ++ " : " ++ show n

makeTypeInput :: FType -> String
makeTypeInput ty = case ty of
  BitVec l n -> "sc_fifo_in<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "sc_fifo_in<sc_lv<1> >"
  Nat    l n -> error $ "nat?3 " ++ show l ++ " : " ++ show n

makeTypeOutput :: FType -> String
makeTypeOutput ty = case ty of
  BitVec l n -> "sc_fifo_out<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "sc_fifo_out<sc_lv<1> >"
  Nat    l n -> error $ "nat?4 " ++ show l ++ " : " ++ show n

outputs :: [COutput] -> String
outputs = unlines . map makeOutput
  where
    makeOutput (outputName, t)
      = makeTypeOutput t ++ " " ++ outputName ++ ";"

intermediarySignals :: [CConn] -> String
intermediarySignals = unlines . filter (/="") . nub . map intSig
  where intSig :: CConn -> String
        intSig (comp, (NameId m1 id1, (p1,t)), (NameId m2 id2, (p2,_)))
          | isInOrOut m1 ||
            isFifo m1    ||
            isInOrOut m2 ||
            isFifo m2 = ""
          | otherwise = makeTypeSignal t ++ " "
                         ++ interSignal m1 id1 p1 m2 id2 p2
                         ++ ";"
          where isInOrOut x = x == comp'
                isFifo    x = isPrefixOf "__fifo__" x
                comp' = changeIfMain comp

interSignal m1 id1 p1 m2 id2 p2 =
  m1 ++ (show id1) ++ "_" ++ p1  ++ "__" ++ m2 ++ (show id2) ++ "_" ++ p2
  
instanceDeclaration :: [TInst] -> String
instanceDeclaration
  = unlines . filter (/="") . map declInst
  where
    declInst :: TInst -> String
    declInst (_,NameId name id,inst,_) = case inst of
      FifoI _ (_,t)
        | name == "__fifo__" -> makeTypeSignal t ++ " " ++ name ++ show id ++ ";"
        | otherwise   -> ""
      _               -> name ++ " " ++ name ++ (show id) ++ ";"

processDeclaration [] = ""
processDeclaration _  = "void proc();"

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

sconnections :: [CConn] -> TMM String
sconnections conns = do
  maybeTxts <- mapM connToString conns
  cont maybeTxts $ do
    let txts = map just maybeTxts
    ret (unlines txts)
  where
    connToString :: CConn -> TMM String
    connToString a@(comp,(NameId m1 id1,(p1,_)),(NameId m2 id2,(p2,_)))
      | (isInOrOut m1 || isFifo m1)
        && (isInOrOut m2 || isFifo m2) =
          throw (TErr
                 ImpossibleConnection
                 Nothing
                 ("Impossible connection between "
                  ++ m1 ++ (show id1)
                  ++ " and "
                  ++ m2 ++ (show id2) ++ "::::: " ++ show a)
                 NoLoc) >> noRet
      | isInOrOut m1 =
          ret $ m2++(show id2) ++ "." ++ p2 ++ "(" ++ p1 ++ ");"
      | isInOrOut m2 =
          ret $ m1++(show id1) ++ "." ++ p1 ++ "(" ++ p2 ++ ");"
      | isFifo m1 =
          ret $ m2++(show id2) ++ "." ++ p2 ++ "(" ++ m1++(show id1) ++ ");"
      | isFifo m2 =
          ret $ m1++(show id1) ++ "." ++ p1 ++ "(" ++ m2++(show id2) ++ ");"
      | isFifoC m1 =
          ret $ m2++(show id2) ++ "." ++ p2 ++ "(" ++ m1 ++ ");"
      | isFifoC m2 =
          ret $ m1++(show id1) ++ "." ++ p1 ++ "(" ++ m2 ++ ");"
      | otherwise =
          let sig = interSignal m1 id1 p1 m2 id2 p2 
          in ret $ unlines [
             m1++(show id1) ++ "." ++ p1 ++ "(" ++ sig ++ ");"
            ,m2++(show id2) ++ "." ++ p2 ++ "(" ++ sig ++ ");"
            ]
      where isInOrOut x = x == comp
            isFifoC   x = isPrefixOf "__fifo__" x
            isFifo    x = x == "__fifo__"
            comp' = changeIfMain comp

changeIfMain :: Name -> Name            
changeIfMain name = if name == "main" then "mainFunc" else name
    
scThread [] = ""
scThread _ = "SC_THREAD(proc);"

closingBraces = "}\n};"
 
procedureToSystemC name [] = ""
procedureToSystemC name p
  = unlines ["void " ++ changeIfMain name ++ "::proc() {"
            ,"while(true) {"
            , unlines (map procedureUnitToSystemC p)
            ,"}"
            ,"}"]

  where 

    procedureUnitToSystemC :: CProcUnit -> String
    procedureUnitToSystemC p = case p of
      PUTSTATE x f -> x ++ "__aux = " ++ f ++ "__aux;"
      GETINPUT (x,_) -> x ++ "__aux = " ++ x ++ ".read();"
      PUTOUTPUT y x -> y ++ ".write(" ++ x ++ "__aux);"
      LOOP qs -> "while (true) {\n" ++ unlines (map procedureUnitToSystemC qs) ++ "\n}"
      BREAK -> "break;"
      GET (x,_) -> x ++ "__aux = " ++ x ++ ".read();"
      PUT (y,_) x -> y ++ ".write(" ++ x ++ "__aux);"
      COND m n
        -> "cond = (" ++ (intercalate ", " (reverse (map
                                     (\i -> "__fifo__"
                                       ++ name
                                       ++ "__cond__"
                                       ++ show i
                                       ++ "__out__aux")
                                     [1..m]))
                  ) ++ ");"
      IF n qs -> "if (cond[" ++ show n ++ "]==1) {\n"
                 ++ unlines (map procedureUnitToSystemC qs) ++ "\n"
      ELSEIF n qs -> "} else if (cond[" ++ show n ++ "]==1) {\n"
                     ++ unlines (map procedureUnitToSystemC qs) ++ "\n"
      ELSE qs -> "} else {\n"
                 ++ unlines (map procedureUnitToSystemC qs) ++ "\n"
                 ++ "}"

procedureSignals :: CProc -> String
procedureSignals = unlines . filter (/="") . concat . map procedureSignal
  where 
    procedureSignal :: CProcUnit -> [String]
    procedureSignal p = case p of
      PUTSTATE x f -> []
      LOOP qs -> concat (map procedureSignal qs)
      BREAK -> []
      GETINPUT (x,t) -> [makeTypePure t ++ " " ++ x ++ "__aux;"]
      PUTOUTPUT _ _ -> []
      GET (x,t)   -> [makeTypePure t ++ " " ++ x ++ "__aux;"
                     ,makeTypeSignal t ++ " " ++ x ++ ";"]
      PUT (y,t) x
        | y == "out" -> []
        | otherwise  -> [makeTypeSignal t ++ " " ++ y ++ ";"]
      COND m _    -> ["sc_lv<" ++ show m ++ "> cond;"]
      IF _ qs     -> concat (map procedureSignal qs)
      ELSEIF _ qs -> concat (map procedureSignal qs)
      ELSE qs     -> concat (map procedureSignal qs)

makeForkFile :: NameId -> Int -> CInput -> [COutput] -> File
makeForkFile (NameId name id) n (_,t) outs =
  (name ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ makeTypePure t ++ " in__aux;\n"
          ++ makeTypeInput t ++ " in;\n"
          ++ (unlines
              (map (\(o,t) -> makeTypeOutput t ++ " " ++ o ++";") outs))
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++ "in__aux = in.read();\n"
          ++ (unlines
              (map (\(o,_) -> o ++".write(in__aux);") outs))
          ++ "}\n"
          ++ "}\n"
          ++ endif

makeConstBinFile (NameId name id) cons (out,t) =
  (name ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ makeTypeOutput t ++ " " ++ out ++ ";\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++ out ++ ".write(\"" ++ cons ++ "\");\n"
          ++ "}\n"
          ++ "}\n"
          ++ endif

makeConstHexFile (NameId name id) cons (out,t) =
  (name ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ makeTypeOutput t ++ " " ++ out ++ ";\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++ out ++ ".write(\"0x" ++ cons ++ "\");\n"
          ++ "}\n"
          ++ "}\n"
          ++ endif

makeConstDecFile (NameId name id) cons (out, t) =
  (name ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ makeTypeOutput t ++ " " ++ out ++ ";\n"
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

makeSpecialFile nid@(NameId name id) ins out args = case name of
  "cat"  -> makeSpecialFileCat nid ins out

  "equ"  -> makeSpecialFileLogic nid ins out

  "and_" -> makeSpecialFileLogic nid ins out
  "or_"  -> makeSpecialFileLogic nid ins out
  
  "not_" -> makeSpecialFileNot nid ins out
  
  "mul"  -> makeSpecialFileArith nid ins out
  "add"  -> makeSpecialFileArith nid ins out
  "sub"  -> makeSpecialFileArith nid ins out
  
  x | isPrefixOf "sli" x -> makeSpecialFileSli nid ins out args

makeSpecialFileSli (NameId name id) [(in1,t1)] (out,t) [a1,a2]
  = (c ++ ".h", content)
  where c = name
        content = unlines
          [ifndef c
          ,"#include \"systemc.h\""
          ,"SC_MODULE(" ++ c ++ ") {"
          , inputs [(in1,t1)]
          , outputs [(out,t)]
          ,"void proc();"
          ,"SC_CTOR(" ++ c ++ ") {"
          ,"SC_THREAD(proc);"
          ,"}"
          ,"};"
          ,"void " ++ c ++ "::proc() {"
          ,"while(true) {"
          , out ++ ".write(" ++ in1 ++ ".read().range(" ++ show a2 ++ ", " ++ show a1 ++ "));"
          ,"}"
          ,"}"
          ,"#endif"]

makeSpecialFileCat (NameId name id) [(in1,t1),(in2,t2)] (out,t)
  = (c ++ ".h", content)
  where c = name
        content = unlines
          [ifndef c
          ,"#include \"systemc.h\""
          ,"SC_MODULE(" ++ c ++ ") {"
          , inputs [(in1,t1),(in2,t2)]
          , outputs [(out,t)]
          ,"void proc();"
          ,"SC_CTOR(" ++ c ++ ") {"
          ,"SC_THREAD(proc);"
          ,"}"
          ,"};"
          ,"void " ++ c ++ "::proc() {"
          ,"while(true) {"
          ,out ++ ".write(("++in1++".read(),"++in2++".read()));"
          ,"}"
          ,"}"
          ,"#endif"]

makeSpecialFileArith (NameId name id) [(in1,t1),(in2,t2)] (out, t)
  = (c ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ inputs [(in1, t1),(in2, t2)]
          ++ outputs [(out, t)] ++ "\n\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++  out ++ ".write((sc_uint<" ++ len t1 ++ ">)" ++ in1 ++ ".read()" ++ symbol ++ "(sc_uint<" ++ len t2 ++ ">)" ++ in2 ++ ".read()" ++ ");\n"
          ++ "}\n"
          ++ "}\n"
          ++ endif
        symbol = case name of
          "mul" -> "*"
          "add" -> "+"
          "sub" -> "-"
        len (BitVec _ n) = show n
        len (Bit _) = show 1
        len _ = error "aaaa"

makeSpecialFileLogic (NameId name id) [(in1,t1),(in2,t2)] (out, t)
  = (c ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ inputs [(in1, t1),(in2, t2)]
          ++ outputs [(out, t)] ++ "\n\n"
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
          "and_" -> "&"
          "or_"  -> "|"
          "equ" -> "=="

makeSpecialFileNot (NameId name id) [(in1,t1)] (out, t)
  = (c ++ ".h", content)
  where c = name
        content
          = ifndef name
          ++ "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ inputs [(in1, t1)]
          ++ outputs [(out, t)] ++ "\n\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++  out ++ ".write(~" ++ in1 ++ ".read());\n"
          ++ "}\n"
          ++ "}\n"
          ++ endif
