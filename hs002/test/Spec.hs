module Main where

import Test.Hspec
import Lib (greeting)

main :: IO ()
main = hspec $ do
  describe "greeting" $ do
    it "正しい挨拶文を返す" $
      greeting `shouldBe` "Hello, Haskell!"
