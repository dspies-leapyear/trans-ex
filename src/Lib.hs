module Lib
  ( fooBarBaz
  , module X
  )
where

import           Lib.Class                     as X
import           Lib.Impl                      as X

fooBarBaz :: (MonadFoo m, MonadBar m, MonadBaz m) => m ()
fooBarBaz = do
  foo defaultUniverse
  bar
  baz
