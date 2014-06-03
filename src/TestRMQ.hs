module Main (main) where

import Test.Hspec

import SparseTable
import NaiveAlgorithms

main :: IO ()
main = hspec $ describe "Testing RMQ" $ do
	it "test array in order [1..10] btwn 2 and 8" $ do
		do f <- sparseTableRMQ [1..10]
		   val1 <- f 2 3
		   val1 `shouldBe` rmq [1..10] 2 3