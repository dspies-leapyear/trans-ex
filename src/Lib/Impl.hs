{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Impl where

import           Control.Monad.IO.Class

import           Lib.Class

newtype App a = App {runApp :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFoo App where
  foo = liftIO $ putStrLn ">>> Foo!"
instance MonadBar App where
  bar = liftIO $ putStrLn ">>> Bar!"
instance MonadBaz App where
  baz = liftIO $ putStrLn ">>> Baz!"
