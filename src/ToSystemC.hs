module ToSystemC where

import Data.Map (fromList,toList)
import Control.Monad
import Data.List (nub,intercalate,isPrefixOf)
import Data.String (lines,unlines)
import Data.Char (toUpper)

import Parser
import Lexer
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
                      let vals = map (!! n) tbs
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
  debug $ "comps " ++ show (length comps)
  debugs comps
  maybeMain <- searchComponent "main"
  cont1 maybeMain $ \main -> do
    topLevel tbs main
    mapM componentToSystemC comps
    ret ()

componentToSystemC :: TComp -> TMM ()
componentToSystemC (name, C f insts inps out conns proced) = do
  debug $ "COMPNAME " ++ name
  msc <- sconnections conns
  cont1 msc $ \sc -> do
    addSystemCFile (changeIfMain name ++ ".h", content sc)
    mapM_ instanceFromFile insts
    debug $ "sigs " ++ name
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
instanceFromFile (comp,fid,nid@(NameId n _),inst,_) = case inst of
  ConstBinI bin out      -> addSystemCFile $ makeConstBinFile fid  nid bin out
  ConstHexI hex out      -> addSystemCFile $ makeConstHexFile fid nid hex out
  ConstDecI dec out      -> addSystemCFile $ makeConstDecFile fid nid dec out
  ConstStrI str out      -> addSystemCFile $ makeConstStrFile fid nid str out
  SpecialI inps out args
    | isPrefixOf "now" n || isPrefixOf "rest" n || isPrefixOf "consR" n -> ok
    | otherwise -> addSystemCFile $ makeSpecialFile fid nid inps out args
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
    getNameFromInstance (_,fid,NameId name _,ins,_) = case ins of
      FifoI _ _ -> ""
      SpecialI _ _ _ -> name ++ (show fid) ++ "_"
      ConstBinI _ _  -> name ++ (show fid) ++ "_"
      ConstHexI _ _  -> name ++ (show fid) ++ "_"
      ConstDecI _ _  -> name ++ (show fid) ++ "_"
      ConstStrI _ _  -> name ++ (show fid) ++ "_"
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
  Stream t   -> makeTypePure t

makeTypeVector :: FType -> String
makeTypeVector ty = case ty of
  BitVec l n -> "std::vector<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "std::vector<sc_lv<1> >"
  Nat    l n -> error $ "nat?1 " ++ show l ++ " : " ++ show n
  Stream t   -> makeTypeVector t

makeTypeSignal :: FType -> String
makeTypeSignal ty = case ty of
  BitVec l n -> "sc_fifo<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "sc_fifo<sc_lv<1> >"
  Nat    l n -> error $ "nat?2 " ++ show l ++ " : " ++ show n
  Stream t   -> makeTypeSignal t

makeTypeInput :: FType -> String
makeTypeInput ty = case ty of
  BitVec l n -> "sc_fifo_in<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "sc_fifo_in<sc_lv<1> >"
  Nat    l n -> error $ "nat?3 " ++ show l ++ " : " ++ show n
  Stream t   -> makeTypeInput t

makeTypeOutput :: FType -> String
makeTypeOutput ty = case ty of
  BitVec l n -> "sc_fifo_out<sc_lv<" ++ show n ++ "> >"
  Bit    l   -> "sc_fifo_out<sc_lv<1> >"
  Nat    l n -> error $ "nat?4 " ++ show l ++ " : " ++ show n
  Stream t   -> makeTypeOutput t

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
    declInst (_,fid,NameId name id,inst,_) = case inst of
      FifoI _ (_,t)
        | name == "__fifo__" -> makeTypeSignal t ++ " " ++ name ++ show id ++ ";"
        | otherwise   -> ""
      SpecialI _ _ _  -> name ++ (show fid) ++ "_ " ++ name ++ show id ++ ";"
      ConstBinI _ _      -> name ++ (show fid) ++ "_ " ++ name ++ show id ++ ";"
      ConstHexI _ _      -> name ++ (show fid) ++ "_ " ++ name ++ show id ++ ";"
      ConstDecI _ _      -> name ++ (show fid) ++ "_ " ++ name ++ show id ++ ";"
      ConstStrI _ _      -> name ++ (show fid) ++ "_ " ++ name ++ show id ++ ";"
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
        initInst (_,fid,NameId n id,inst,_) = case inst of
          FifoI _ _ -> ""
          _         -> n ++ show id ++ "(\"" ++ n ++ show id ++ "\")"

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
      BLOB x -> "//blob " ++ show x
      COPY _ x y -> unlines [y ++ "__copy__val = " ++ y ++ ".read();"
                            ,x ++ ".write(" ++ y ++ "__copy__val);"
                            ,"if (" ++ y ++ "__copy__val != 0) {"
                            ,"while (" ++ y ++ ".nb_read(" ++ y ++ "__copy__val)) {"
                            , x ++ ".write(" ++ y ++ "__copy__val);"
                            ,"if (" ++ y ++ "__copy__val == 0) break;"
                            ,"}"
                            ,"}"]
      PCOPY k _ x y -> unlines $
                       map (\i -> unlines
                             [x ++ ".write(" ++ y ++ "__now__" ++ show i ++ ");"
                             ,"if (" ++ y ++ "__now__" ++ show i ++ " != 0) {"]) [1..k]
                       ++ [y ++ "__copy__val = " ++ y ++ ".read();"
                          ,x ++ ".write(" ++ y ++ "__copy__val);"
                          ,"if (" ++ y ++ "__copy__val != 0) {"
                          ,"while (" ++ y ++ ".nb_read(" ++ y ++ "__copy__val)) {"
                          , x ++ ".write(" ++ y ++ "__copy__val);"
                          ,"if (" ++ y ++ "__copy__val == 0) break;"
                          ,"}"
                          ,"}"]
                       ++ replicate k "}"
      COPYV t x y -> unlines ["for(std::vector<" ++ makeTypePure t ++ " >::iterator " ++ y ++ "__it_ = " ++ y ++ ".begin(); " ++ y ++ "__it_ != " ++ y ++ ".end(); ++" ++ y ++ "__it_) {"
                            , x ++ ".write(*" ++ y ++ "__it_);"
                            ,"}"]
      CLEARV v -> v ++ "__savev.clear();"
      SAVE (x,_) -> unlines [x ++ "__save__val = " ++ x ++ ".read();"
                            ,x ++ "__save.write(" ++ x ++ "__save__val);"
                            ,"if (" ++ x ++ "__save__val != 0) {"
                            ,"while (" ++ x ++ ".nb_read(" ++ x ++ "__save__val)) {"
                            , x ++ "__save.write(" ++ x ++ "__save__val);"
                            ,"if (" ++ x ++ "__save__val == 0) break;"
                            ,"}"
                            ,"}"]
      SAVEV (x,_) -> unlines [x ++ "__savev__val = " ++ x ++ ".read();"
                             ,x ++ "__savev.push_back(" ++ x ++ "__savev__val);"
                             ,"if (" ++ x ++ "__savev__val != 0) {"
                             ,"while (" ++ x ++ ".nb_read(" ++ x ++ "__savev__val)) {"
                             ,x ++ "__savev.push_back(" ++ x ++ "__savev__val);"
                             ,"if (" ++ x ++ "__savev__val == 0) break;"
                             ,"}"
                             ,"}"]
      RESTV x -> x ++ "__savev.erase(" ++ x ++ "__savev.begin());"
      MAKEV _ x y -> unlines [y ++ "__savev__val = " ++ y ++ ".read();"
                             ,x ++ "__savev.push_back(" ++ y ++ "__savev__val);"
                             ,"if (" ++ y ++ "__savev__val != 0) {"
                             ,"while (" ++ y ++ ".nb_read(" ++ y ++ "__savev__val)) {"
                             ,x ++ "__savev.push_back(" ++ y ++ "__savev__val);"
                             ,"if (" ++ y ++ "__savev__val == 0) break;"
                             ,"}"
                             ,"}"]
      PUTSTATE x f -> x ++ "__aux = " ++ f ++ "__aux;"
      GETINPUT (x,_) -> x ++ "__aux = " ++ x ++ ".read();"
      GETSTREAMSAFE (i,f) d (x,_)
        | d == f -> x ++ "__now__" ++ show d ++ " = " ++ x ++ ".read();" ++ replicate (f-i) '}'
        | otherwise -> unlines $
          [x ++ "__now__" ++ show d ++ " = " ++ x ++ ".read();"
          ,"if (" ++ x ++ "__now__" ++ show d ++ " == 0) {"]
          ++ map (\d' -> x ++ "__now__" ++ show d' ++ " = 0;") [(d+1)..f]
          ++ ["} else {"]
      GETSTREAMV d (x,_) -> x ++ "__now__" ++ show d ++ " = *(" ++ x ++ "__savev.begin() + " ++ show (d-1) ++ ");"
      PUTSTREAM d (y,_) x -> y ++ ".write(" ++ x ++ "__now__" ++ show d ++ ");"
      PUTOUTPUTSTREAM d y x -> y ++ ".write(" ++ x ++ "__now__" ++ show d ++ ");"
      PUTOUTPUTSTREAMV _ d y x -> unlines [y ++ "__it = " ++ y ++ ".begin();"
                                          ,y ++ ".insert(" ++ y ++ "__it," ++ x ++ "__now__" ++ show d ++ ");"]
      PUTOUTPUT y x -> y ++ ".write(" ++ x ++ "__aux);"
      LOOP qs -> "while (true) {\n" ++ unlines (map procedureUnitToSystemC qs) ++ "\n}"
      BREAK -> "break;"
      SWITCH (s,_) a b -> s ++ "__now__" ++ show a ++ " = " ++ s ++ "__now__" ++ show b ++ ";"
      GET (x,_) -> x ++ "__aux = " ++ x ++ ".read();"
      PUT (y,_) x -> y ++ ".write(" ++ x ++ "__aux);"
      PUTV (y,_) x -> unlines [y ++ "__it = " ++ y ++ ".begin();"
                              ,y ++ ".insert(" ++ y ++ "__it," ++ x ++ "__aux);"]
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
      DESTROY 0 (s,_) -> unlines
        [s ++ "__destroy = " ++ s ++ ".read();"
        ,"if (" ++ s ++ "__destroy != 0) {"
        ,"while(" ++ s ++ ".nb_read(" ++ s ++ "__destroy)) { if (" ++ s ++"__destroy == 0) break; }"
        ,"}"]
      DESTROY n (s,_) -> unlines
        ["if (" ++ intercalate " && " (map (\i -> s ++ "__now__" ++ show i ++ " != 0") [1..n]) ++ ") {"
        ,s ++ "__destroy = " ++ s ++ ".read();"
        ,"if (" ++ s ++ "__destroy != 0) {"
        ,"while(" ++ s ++ ".nb_read(" ++ s ++ "__destroy)) { if (" ++ s ++"__destroy == 0) break; }"
        ,"}"
        ,"}"]
      DESTROYV (s,_) -> s ++ "__savev.clear();"

procedureSignals :: CProc -> String
procedureSignals = unlines . nub . filter (/="") . concat . map procedureSignal
  where 
    procedureSignal :: CProcUnit -> [String]
    procedureSignal p = case p of
      RESTV x -> []
      BLOB _ -> []
      COPY t x y
        | x == "out" && y == "out" -> [makeTypePure t ++ " " ++ y ++ "__copy__val;"]
        | x == "out" -> [makeTypeSignal t ++ " " ++ y ++ ";"
                        ,makeTypePure t ++ " " ++ y ++ "__copy__val;"]
        | y == "out" -> [makeTypeSignal t ++ " " ++ x ++ ";"
                        ,makeTypePure t ++ " " ++ y ++ "__copy__val;"]
        | otherwise  -> [makeTypeSignal t ++ " " ++ x ++ ";"
                        ,makeTypeSignal t ++ " " ++ y ++ ";"
                        ,makeTypePure t ++ " " ++ y ++ "__copy__val;"]
      PCOPY _ t x y
        | x == "out" -> [makeTypePure t ++ " " ++ y ++ "__copy__val;"]
        | otherwise  -> [makeTypeSignal t ++ " " ++ x ++ ";"
                        ,makeTypePure t ++ " " ++ y ++ "__copy__val;"]
      COPYV t x _
        | x == "out" -> []
        | otherwise  -> [makeTypeSignal t ++ " " ++ x ++ ";"]
      SAVE (x,t) -> [makeTypeSignal t ++ " " ++ x ++ "__save;"
                    ,makeTypePure t ++ " " ++ x ++ "__save__val;"]
      SAVEV (x,t) -> [makeTypeVector t ++ " " ++ x ++ "__savev;"
                     ,makeTypePure t ++ " " ++ x ++ "__savev__val;"]
      MAKEV t x y -> [makeTypeVector t ++ " " ++ x ++ "__savev;"
                     ,makeTypePure t ++ " " ++ y ++ "__savev__val;"
                     ,makeTypeSignal t ++ " " ++ y ++ ";"]
      CLEARV v -> []
      PUTSTATE x f -> []
      LOOP qs -> concat (map procedureSignal qs)
      BREAK -> []
      SWITCH (s,t) a _-> [makeTypePure t ++ " " ++ s ++ "__now__" ++ show a ++ ";"]
      GETSTREAMSAFE _ d (x,t) -> [makeTypePure t ++ " " ++ x ++ "__now__" ++ show d ++ ";"]
      GETSTREAMV d (x,t) -> [makeTypePure t ++ " " ++ x ++ "__now__" ++ show d ++ ";"]
      PUTSTREAM _ (y,t) _ -> [makeTypeSignal t ++ " " ++ y ++ ";"]
      PUTOUTPUTSTREAM _ _ _ -> []
      PUTOUTPUTSTREAMV t _ y _ -> [makeTypeVector t ++ "::iterator " ++ y ++ "__it;"]
      GETINPUT (x,t) -> [makeTypePure t ++ " " ++ x ++ "__aux;"]
      PUTOUTPUT _ _ -> []
      GET (x,t)   -> [makeTypePure t ++ " " ++ x ++ "__aux;"
                     ,makeTypeSignal t ++ " " ++ x ++ ";"]
      PUT (y,t) _
        | y == "out" -> []
        | otherwise  -> [makeTypeSignal t ++ " " ++ y ++ ";"]
      PUTV (y,t) _ -> [makeTypeVector t ++ "::iterator " ++ y ++ "__it;"
                      ,makeTypeVector t ++ " " ++ y ++ ";"]
      COND m _    -> ["sc_lv<" ++ show m ++ "> cond;"]
      IF _ qs     -> concat (map procedureSignal qs)
      ELSEIF _ qs -> concat (map procedureSignal qs)
      ELSE qs     -> concat (map procedureSignal qs)
      DESTROY _ (s,t) -> [makeTypePure t ++ " " ++ s ++ "__destroy;"]
      DESTROYV _ -> []

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

makeConstBinFile fid (NameId name id) cons (out,t) =
  (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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

makeConstHexFile fid (NameId name id) cons (out,t) =
  (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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

makeConstDecFile fid (NameId name id) cons (out, t) =
  (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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

makeConstStrFile fid (NameId name id) stream (out, t) =
  (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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
          ++ unlines (map f stream)
          ++ "}\n"
          ++ "}\n"
          ++ endif
        f fcons = out ++ ".write(" ++ (case fcons of
          FBin (L _ b) -> "\"" ++ b ++ "\""
          FHex (L _ h) -> "\"0x" ++ h ++ "\""
          FDec (L _ d) -> show d
          FForeverWait -> "") ++ ");\n"

makeSpecialFile fid nid@(NameId name id) ins out args = case name of
  "cat"  -> makeSpecialFileCat fid nid ins out

  "equ"  -> makeSpecialFileLogic fid nid ins out

  "and_" -> makeSpecialFileLogic fid nid ins out
  "or_"  -> makeSpecialFileLogic fid nid ins out
  
  "not_" -> makeSpecialFileNot fid nid ins out
  
  "mul"  -> makeSpecialFileArith fid nid ins out
  "add"  -> makeSpecialFileArith fid nid ins out
  "sub"  -> makeSpecialFileArith fid nid ins out

  x | isPrefixOf "mrest" x -> makeSpecialFileMRest fid nid ins out args
  x | isPrefixOf "sli" x -> makeSpecialFileSli fid nid ins out args

  x -> error x

makeSpecialFileMRest fid (NameId name id) [(in1,t1)] (out,t) [a1]
  = (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content = unlines
          [ifndef c
          ,"#include \"systemc.h\""
          ,"SC_MODULE(" ++ c ++ ") {"
          , inputs [(in1,t1)]
          , outputs [(out,t)]
          , makeTypePure t ++ " v;"
          ,"void proc();"
          ,"SC_CTOR(" ++ c ++ ") {"
          ,"SC_THREAD(proc);"
          ,"}"
          ,"};"
          ,"void " ++ c ++ "::proc() {"
          ,"while(true) {"
          , unlines (replicate a1 (in1 ++ ".read();"))
          , "while(" ++ in1 ++ ".nb_read(v)) {"
          , out ++ ".write(v);"
          ,"}"
          ,"}"
          ,"}"
          ,"#endif"]

makeSpecialFileSli fid (NameId name id) [(in1,t1)] (out,t) [a1,a2]
  = (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
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

makeSpecialFileCat fid (NameId name id) [(in1,t1),(in2,t2)] (out,t)
  = (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
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

makeSpecialFileArith fid (NameId name id) [(in1,t1),(in2,t2)] (out, t)
  = (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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

makeSpecialFileLogic fid (NameId name id) [(in1,t1),(in2,t2)] (out, t)
  = (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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

makeSpecialFileNot fid (NameId name id) [(in1,t1)] (out, t)
  = (c ++ ".h", content)
  where c = name ++ show fid ++ "_"
        content
          = ifndef c
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
