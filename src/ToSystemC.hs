module ToSystemC where

import Components
import Data.Map (fromList,toList)
import System.Directory
import Control.Monad
import Data.List (nub)

type FileName = String
type File = (FileName, String)
type SystemC = [File]

------------

selectMain :: [C] -> C
selectMain cs = case filter (nameEqual "main") cs of
  [] -> error "No main"
  [x] -> x
  _  -> error "Multiple main's"

topLevel :: [Int] -> C -> SystemC
topLevel lst (C _ f _insts inps outs _Conns _Proc)
  = [("main.cpp", mainContent)
    ,("top.h", topLevelContent)
    ,("testbench.h",testbenchContent)]
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
          ++ signals inps outs ++ "\n\n"
          ++ "SC_CTOR(top) : m(\"m\"), tb(\"tb\") {\n"
          ++ topConnections inps
          ++ topConnections outs ++ "\n\n"
          ++ "}\n"
          ++ "};"
        topConnections
          = foldl (\a b -> a ++ "\n" ++ b) ""
          . map (\(m,p) -> "m." ++ p ++ "(" ++ p ++ "); "
                           ++ "tb." ++ p ++ "(" ++ p ++ ");")
        
        signals is os
          = foldl (\a b -> a ++ "\n" ++ b) ""
          $ map (\x -> "sc_fifo<int> " ++ x ++ ";") (map snd is ++ map snd os)
        testbenchContent
          = "#include \"systemc.h\"\n\n"
          ++ "SC_MODULE(testbench) {\n"
          ++ inputs outs
          ++ outputs inps
          ++ "\n\nvoid proc();\n\n"
          ++ "SC_CTOR(testbench) {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void testbench::proc() {"
          ++ fillFifo inps ++ "\n\n"
          ++ "}"
        fillFifo = foldl (\a b -> a++"\n"++b) "" . map (fill lst)
        fill ls (_,port)
          = (foldl1 (\a b -> a ++ "\n" ++ seeOut ++ b) (map write ls)) ++ "\n" ++ seeOut
          where write cons
                  = port ++ ".write(" ++ show cons ++ ");"
                seeOut = "cout << out_0.read() << endl;\n"
                
-- faz o test bench e o top level
-- com base nos inputs e outputs da main


---------------

toSystemC :: [Int] -> [C] -> SystemC
toSystemC lst cs = let components
                         = concat $ map (component2SystemC cs) cs
                       main = selectMain cs
                       tl = topLevel lst main
                   in toList (fromList (components ++ tl))

component2SystemC :: [C] -> C -> SystemC
component2SystemC _ Dummy = error "Dummy component error"
component2SystemC cs (C name' _ insts inps outs conns process)
  = (name ++ ".h", content) : filesFromInstances cs insts
  where
    name = if name' == "main" then "mainFunc" else name'
    f x = x
    content
      = includeSystemC ++ "\n"
      ++ includeInstances insts ++ "\n\n"
      ++ scModule name ++ "\n"
      ++ inputs inps 
      ++ outputs outs ++ "\n"
      ++ intermediarySignals name conns ++ "\n"
      ++ instanceDeclaration insts
      ++ processDeclaration process ++ "\n"
      ++ scCtor name insts ++ "\n\n"
      ++ connections name conns
      ++ scThread process
      ++ closingBraces
      ++ voidProc name process


includeSystemC :: String
includeSystemC = "#include \"systemc.h\""

includeInstances :: [Instance] -> String
includeInstances
  = foldl (\a b -> a++"\n"++b) ""
    . map (\x -> "#include \"" ++ x ++ ".h\"")
    . filter (/="")
    . map includeInstance
  where includeInstance :: Instance -> String
        includeInstance ins = case ins of
          DummyInstance -> error "Dummy instance"
          Instance _ id _ _ -> id
          ConsInstance _ id _ -> id
          SpecialInstance _ id _ _ -> id
          Fifo _ _ -> ""

scModule name = "SC_MODULE(" ++ name ++ ") {"

inputs = foldl (\a b -> a ++ "\n" ++ b) "" . map input
  where 
    input (_,inputName) = "sc_fifo_in<int> " ++ inputName ++ ";"

outputs = foldl (\a b -> a++"\n"++b) "" . map output
  where 
    output (_,outputName) = "sc_fifo_out<int> " ++ outputName ++ ";"

instanceDeclaration :: [Instance] -> String
instanceDeclaration
  = foldl (\a b -> a++"\n"++b) ""
    . filter (/="")
    . map declInst
  where
    declInst :: Instance -> String
    declInst ins = case ins of
      DummyInstance -> error "Dummy instance"
      Instance _ id _ _ -> typeAndVar id
      ConsInstance _ id _ -> typeAndVar id
      SpecialInstance _ id _ _ -> typeAndVar id
      Fifo _ _ -> "sc_fifo<int> fifo;"

    typeAndVar s = s ++ " _" ++ s ++ ";"

processDeclaration EndProc = "\n"
processDeclaration _ = "void proc();\n"

scCtor name insts
  = "SC_CTOR(" ++ name ++ ")" ++ initInsts ++ " {"
  where initInsts = case filter (/="") (map initInst insts) of
                      []  -> ""
                      [x] -> " : " ++ x
                      xs  -> " : " ++ foldl1 (\a b -> a++", "++b) xs
        initInst ins = case ins of
          DummyInstance -> error "Dummy instance"
          Instance _ id _ _ -> initVar id
          ConsInstance _ id _ -> initVar id
          SpecialInstance _ id _ _ -> initVar id
          Fifo _ _ -> ""

        initVar s = "_" ++ s ++ "(\"" ++ s ++ "\")"

connections :: String -> Conns -> String
connections name' = foldl1 (\a b -> a++"\n"++b) . map connToString
  where connToString :: ((String,String),(String,String)) -> String
        connToString ((m1,p1),(m2,p2))
          | (m1 == name || m1 == "fifo")
            && (m2 == name || m2 == "fifo") = "idError"
          | m1 == name = "_" ++ m2 ++ "." ++ p2 ++ "(" ++ p1 ++ ");"
          | m2 == name = "_" ++ m1 ++ "." ++ p1 ++ "(" ++ p2 ++ ");"
          | m1 == "fifo" = "_" ++ m2 ++ "." ++ p2 ++ "(fifo);"
          | m2 == "fifo" = "_" ++ m1 ++ "." ++ p1 ++ "(fifo);"
          | otherwise
          = let ps = m1 ++ "_" ++ p1 ++ "_" ++ m2 ++ "_" ++ p2 
            in unlines [
               "_" ++ m1 ++ "." ++ p1 ++ "(" ++ ps ++ ");"
              ,"_" ++ m2 ++ "." ++ p2 ++ "(" ++ ps ++ ");"
              ]
        name = if name' == "mainFunc" then "main" else name'


intermediarySignals :: String -> Conns -> String
intermediarySignals name = foldl (\a b -> a++"\n"++b) "" . filter (/="") . nub . map intSig
  where intSig :: ((String,String),(String,String)) -> String
        intSig ((m1,p1),(m2,p2))
          | (m1 /= name && m1 /= "fifo"
             && m2 /= name && m2 /= "fifo") = "sc_fifo<int> " ++ m1 ++ "_" ++ p1 ++ "_" ++ m2 ++ "_" ++ p2 ++ ";"          
          | otherwise = ""


scThread EndProc = ""
scThread _ = ""

closingBraces = "}\n};"

voidProc name process = ""

filesFromInstances :: [C] -> [Instance] -> SystemC
filesFromInstances cs = concat . map (instanceToFile cs)

instanceToFile :: [C] -> Instance -> SystemC
instanceToFile cs ins = case ins of
  DummyInstance -> error "dummy"
  ConsInstance cons id inp -> [makeConsFile cons id inp]
  SpecialInstance n id inps outs -> [makeSpecFile n id inps outs]
  Fifo _ _ -> []
  Instance name id inps outs -> case filter (nameEqual name) cs of
    []  -> error "no component for instance"
    [x] -> component2SystemC cs x
    xs  -> component2SystemC cs (head xs)
  
nameEqual :: String -> C -> Bool
nameEqual n (C m _ _ _ _ _ _) = n == m

makeConsFile cons c (_,inp) = (c ++ ".h", content)
  where content
          = "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++c++") {\n"
          ++ "sc_fifo_out<int> " ++ inp ++ ";\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++c++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ c ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++ inp ++ ".write(" ++ show cons ++ ");\n"
          ++ "}\n"
          ++ "}"
          
makeSpecFile name id [in1,in2] [out] = (id ++ ".h", content)
  where content
          = "#include \"systemc.h\"\n"
          ++ "SC_MODULE("++id++") {\n"
          ++ inputs [in1,in2]
          ++ outputs [out] ++ "\n\n"
          ++ "void proc();\n"
          ++ "SC_CTOR("++id++") {\n"
          ++ "SC_THREAD(proc);\n"
          ++ "}\n"
          ++ "};\n\n"
          ++ "void " ++ id ++ "::proc() {\n"
          ++ "while(true) {\n"
          ++  snd out ++ ".write(" ++ snd in1 ++ ".read()" ++ symbol ++ snd in2 ++ ".read()" ++ ");\n"
          ++ "}\n"
          ++ "}"

        symbol = case name of
          "mul" -> "*"
          "add" -> "+"
          "sub" -> "-"
          _ -> "&&"

------------------

makeSystemC :: SystemC -> IO ()
makeSystemC files = do 
  doesIt <- doesDirectoryExist "./result"
  case doesIt of
    True -> do
      putStrLn "Directory result already exists"
      return ()
    False -> do
      createDirectory "./result"
      forM_ files $ \(filename, content) ->
        writeFile ("./result/"++filename) content
