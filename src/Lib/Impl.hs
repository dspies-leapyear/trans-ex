{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Impl where

import           Control.Monad.IO.Class

import           Lib.Class


newtype App x = App {runApp :: IO x}
  deriving (Functor, Applicative, Monad, MonadIO)

data UniverseApp = UniverseApp Char Int

instance IsFooUniverse UniverseApp where
  defaultUniverse = UniverseApp 'C' 137

instance MonadFoo App where
  type Universe App = UniverseApp
  foo (UniverseApp c n) = liftIO $ putStrLn $ ">>> Foo: " ++ [c] ++ show n ++ "!"
instance MonadBar App where
  bar = liftIO $ putStrLn ">>> Bar!"
instance MonadBaz App where
  baz = liftIO $ putStrLn ">>> Baz!"
