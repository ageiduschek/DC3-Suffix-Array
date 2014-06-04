module Main (main) where

import Test.Hspec

import FischerHeun
import NaiveAlgorithms

main :: IO ()
main = hspec $ describe "Testing RMQ"
	it "test array in order [1..10] btwn 2 and 8" $ do
		f <- fischerHeunRMQ [1..10]
		val <- f 2 8
		val `shouldBe` rmq [1..10] 2 8
		