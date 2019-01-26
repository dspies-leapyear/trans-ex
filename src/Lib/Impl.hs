{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Impl where

import           Control.Monad.IO.Class

import           Lib.Class

data RealUniverse = RealUniverse Char Int

instance IsFooUniverse RealUniverse where
  defaultUniverse = RealUniverse 'C' 137

newtype FooT m x = FooT {runFooT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadBar, MonadBaz)
newtype BarT m x = BarT {runBarT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBaz)
newtype BazT m x = BazT {runBazT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBar)

instance MonadIO m => MonadFoo (FooT m) where
  type Universe (FooT m) = RealUniverse
  foo (RealUniverse c n) = liftIO $ putStrLn $ ">>> Foo: " ++ [c] ++ show n ++ "!"
instance MonadIO m => MonadBar (BarT m) where
  bar = liftIO $ putStrLn ">>> Bar!"
instance MonadIO m => MonadBaz (BazT m) where
  baz = liftIO $ putStrLn ">>> Baz!"

newtype App x = App (BazT (BarT (FooT IO)) x)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBar, MonadBaz)

runApp :: App x -> IO x
runApp (App action) = runFooT $ runBarT $ runBazT action
