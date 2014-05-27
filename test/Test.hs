module Main where

import Test.Hspec
import Test.QuickCheck

import SuffixArray

main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do
    return ()
  -- HUnit/HSpec  tests.
--  describe "main" $ do
--    it "is main" $ do
--      x <- foobarbaz
--      x `shouldBe` "foobarbaz"

  -- example quickcheck test in hspec.
--  describe "read" $ do
--    it "is inverse to show" $ property $
--      \x -> (read . show) x == (x :: Int)

