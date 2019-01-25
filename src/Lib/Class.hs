module Lib.Class where

class Monad m => MonadFoo m where
  foo :: m ()

class Monad m => MonadBar m where
  bar :: m ()

class Monad m => MonadBaz m where
  baz :: m ()
