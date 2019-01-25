{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.IO.Class
import           Test.Hspec

import           Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FooT" $ it "should foo" $ do
    runFooT $ runMockBarT $ runMockBazT fooBarBaz
    return () :: Expectation
  describe "BarT" $ it "should bar" $ do
    runMockFooT $ runBarT $ runMockBazT fooBarBaz
    return () :: Expectation
  describe "BazT" $ it "should baz" $ do
    runMockFooT $ runMockBarT $ runBazT fooBarBaz
    return () :: Expectation

newtype MockFooT m x = MockFooT{runMockFooT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadBar, MonadBaz)
newtype MockBarT m x = MockBarT{runMockBarT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBaz)
newtype MockBazT m x = MockBazT{runMockBazT :: m x}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFoo, MonadBar)

instance MonadIO m => MonadFoo (MockFooT m) where
  foo = liftIO $ putStrLn ">>> Mock Foo!"
instance MonadIO m => MonadBar (MockBarT m) where
  bar = liftIO $ putStrLn ">>> Mock Bar!"
instance MonadIO m => MonadBaz (MockBazT m) where
  baz = liftIO $ putStrLn ">>> Mock Baz!"