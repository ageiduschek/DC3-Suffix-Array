module Main (main) where

import Test.Hspec

import FischerHeun
import NaiveAlgorithms

main :: IO ()
main = hspec $ describe "Testing RMQ"
	it "test array in order [1..10] btwn 2 and 8" $
		f <- fischerHeunRMQ [1..10] 2 8
		f 2 8 `shouldBe` rmq [1..10] 2 8
	it "test array in order backwards [10..1] btwn 2 and 8" $
		fischerHeunRMQ [10..1] 2 8 `shouldBe` rmq [10..1] 2 8
		