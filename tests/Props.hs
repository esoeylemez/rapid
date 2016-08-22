-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

module Main (main) where

import Test.Tasty


subgroups :: [TestTree]
subgroups =
    []


main :: IO ()
main = defaultMain (testGroup "all" subgroups)
