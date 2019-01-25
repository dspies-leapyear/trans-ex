{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.Class where

class IsFooUniverse u where
  defaultUniverse :: u

class (Monad m, IsFooUniverse (Universe m)) => MonadFoo m where
  type Universe m
  foo :: Universe m -> m ()

class Monad m => MonadBar m where
  bar :: m ()

class Monad m => MonadBaz m where
  baz :: m ()
