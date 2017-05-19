{-# LANGUAGE FlexibleInstances #-}
module Test where

import Control.Monad.Trans 

newtype St s a = St { runSt :: s -> Maybe a -> (Maybe a, s) }

evalStateT :: St s a -> s -> Maybe a
evalStateT m s =
  let (a, _) = runSt m s Nothing in a

execStateT :: St s a -> s -> s
execStateT m s =
    let (_, s') = runSt m s Nothing in s'

instance Functor (St s) where
  fmap f m = St $ \s x ->
    let (a, s') = runSt m s Nothing
    in (fmap f a, s')

instance Applicative (St s) where
    pure a = St $ \s _ -> (pure a, s)
    St mf <*> St mx = St $ \s x ->
      let ~(f,  s') = mf s Nothing
          ~(y, s'') = mx s' Nothing
        in (f <*> y, s'')
    {-# INLINE (<*>) #-}
    (*>) = (>>)

{-instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
    empty = StateT $ \ _ -> mzero
    {-# INLINE empty #-}
    StateT m <|> StateT n = StateT $ \ s -> m s `mplus` n s
    {-# INLINE (<|>) #-}-}

lift' ma = St $ \s x -> (ma, s)

instance Monad (St s) where
    return a = St $ \s _ -> (return a, s)
    St m >>= k  = St $ \s x ->
        let (a, s') = m s Nothing
        in _
    fail = error

{-type TM a = St String Maybe a

exe :: TM a -> String
exe tm = execStateT tm ""

run :: TM a -> Maybe a
run tm = evalStateT tm ""

noReturn :: TM a
noReturn = lift Nothing

put :: Monad m => s -> St s m ()
put s = St $ \_ -> (return (), s)

hey :: TM Int
hey = do
  a <- return 5
  b <- noReturn
  put "hey"
  return $ a + b
-}
