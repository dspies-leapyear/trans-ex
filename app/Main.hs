module Main where

import Lib

main :: IO ()
main = runFooT $ runBarT $ runBazT fooBarBaz
