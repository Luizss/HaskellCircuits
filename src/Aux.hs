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

