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
    runMockServices $ runFooT fooBarBaz
    return () :: Expectation
  describe "BarT" $ it "should bar" $ do
    runMockServices $ runBarT fooBarBaz
    return () :: Expectation
  describe "BazT" $ it "should baz" $ do
    runMockServices $ runBazT fooBarBaz
    return () :: Expectation

data MockUniverse = MockUniverse

instance IsFooUniverse MockUniverse where
  defaultUniverse = MockUniverse

newtype MockServices x = MockServices {runMockServices :: IO x}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFoo MockServices where
  type Universe MockServices = MockUniverse
  foo MockUniverse = liftIO $ putStrLn ">>> Mock Foo!"
instance MonadBar MockServices where
  bar = liftIO $ putStrLn ">>> Mock Bar!"
instance MonadBaz MockServices where
  baz = liftIO $ putStrLn ">>> Mock Baz!"
