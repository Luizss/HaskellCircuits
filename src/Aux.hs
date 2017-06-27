module Aux where

import Types
import LexerCore
import ParserCore

just (Just x) = x
just _ = error "From Justa"

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

may :: Maybe a -> TM (Maybe a)
may = return 

ok :: Monad m => m ()
ok = return ()

mok :: Monad m => m (Maybe ())
mok = return (Just ())

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,c) = (f a,c)

mapSec :: (a -> b) -> (c,a) -> (c,b)
mapSec f (c,a) = (c, f a)

getTypeFromFExpr :: FExpr -> FType
getTypeFromFExpr (FApp _ _ t) = t
getTypeFromFExpr (FAExpr (_, _, t)) = t

fst3 (f,_,_) = f
fst4 (f,_,_,_) = f

equalFType :: FType -> FType -> Bool
equalFType (BitVec _ i1) (BitVec _ i2) = i1 == i2
equalFType (Bit _) (Bit _) = True
equalFType (Nat _ i1) (Nat _ i2) = i1 == i2
equalFType _ _ = False
           
isStreamFunc :: Name -> Bool
isStreamFunc name = elem name ["cons","consR","rest","now"]

mapIndex = mapIndex' 0
mapIndex' _ f [] = []
mapIndex' i f (x:xs) = f i x : mapIndex' (i+1) f xs

indexes :: [Int] -> [a] -> [a]
indexes is xs = map just
                $ filter isJust
                $ mapIndex f xs
  where f i b
          | elem i is = Just b
          | otherwise = Nothing

for l f = map f l
