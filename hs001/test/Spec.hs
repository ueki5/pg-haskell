module Main where

import Test.Hspec
import UpperCase (uppercaseText)

main :: IO ()
main = hspec $ do
  describe "uppercaseText" $ do
    it "小文字を大文字に変換する" $
      uppercaseText "hello" `shouldBe` "HELLO"
    it "空文字列はそのまま" $
      uppercaseText "" `shouldBe` ""
    it "大文字はそのまま" $
      uppercaseText "ABC" `shouldBe` "ABC"
    it "混在文字列" $
      uppercaseText "Hello World" `shouldBe` "HELLO WORLD"
