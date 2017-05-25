module Aux where

import LexerCore
import Types

fromLow (Low n) = n

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

getType :: Ty a -> Maybe FT
getType (Ty x y) = x

getTerm :: Ty a -> a
getTerm (Ty x y) = y

getLocL :: TyL a -> SrcLoc
getLocL = getLoc . getTerm

getTermL :: TyL a -> a
getTermL = getVal . getTerm

noTy :: a -> Ty a
noTy x = Ty Nothing x

putType :: FT -> Ty a -> Ty a
putType ty (Ty _ x) = Ty (Just ty) x
