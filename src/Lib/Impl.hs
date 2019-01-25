{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Impl where

import           Control.Monad.IO.Class

import           Lib.Class


newtype FooT m x = FooT{runFooT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadBar, MonadBaz)
newtype BarT m x = BarT{runBarT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBaz)
newtype BazT m x = BazT{runBazT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBar)

instance MonadIO m => MonadFoo (FooT m) where
  foo = liftIO $ putStrLn ">>> Foo!"
instance MonadIO m => MonadBar (BarT m) where
  bar = liftIO $ putStrLn ">>> Bar!"
instance MonadIO m => MonadBaz (BazT m) where
  baz = liftIO $ putStrLn ">>> Baz!"
