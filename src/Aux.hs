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

getType :: Ty a -> TypeExpr
getType (Ty x y) = x

getTerm :: Ty a -> a
getTerm (Ty x y) = y

getLocL :: TyL a -> SrcLoc
getLocL = getLoc . getTerm

getTermL :: TyL a -> a
getTermL = getVal . getTerm

getLocFromCons :: FCons -> SrcLoc
getLocFromCons fcons = case fcons of
  FBin tyl -> getLocL tyl
  FHex tyl -> getLocL tyl
  FDec tyl -> getLocL tyl
