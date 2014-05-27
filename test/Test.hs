module Main where

import Test.Hspec
import Test.QuickCheck

import Trahs

main :: IO ()
main = hspec $ describe "Testing Lab 3" $ do

  -- HUnit/HSpec  tests.
  describe "main" $ do
    it "is main" $ do
      x <- trahs
      x `shouldBe` ()

  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)

