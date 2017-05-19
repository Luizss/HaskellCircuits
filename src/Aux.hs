module Aux where

import Types

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
