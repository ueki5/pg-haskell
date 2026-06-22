module Main where

import Compiler (readSource)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "readSource" $ do
    it "ファイルの内容をそのまま返す" $ do
      let tmpFile = "/tmp/hs003_test.txt"
      writeFile tmpFile "hello\nworld\n"
      result <- readSource tmpFile
      result `shouldBe` "hello\nworld\n"
    it "空ファイルは空文字列を返す" $ do
      let tmpFile = "/tmp/hs003_test_empty.txt"
      writeFile tmpFile ""
      result <- readSource tmpFile
      result `shouldBe` ""
