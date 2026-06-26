module Main where

import Compiler (Expr (..), Token (..), eval, parse, tokenize)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "tokenize" $ do
    it "整数を変換する" $
      tokenize "42" `shouldBe` Right [TInt 42]
    it "空白をスキップする" $
      tokenize "1 + 2" `shouldBe` Right [TInt 1, TPlus, TInt 2]
    it "全演算子を変換する" $
      tokenize "+-*/" `shouldBe` Right [TPlus, TMinus, TStar, TSlash]
    it "括弧を変換する" $
      tokenize "(1)" `shouldBe` Right [TLParen, TInt 1, TRParen]
    it "未知の文字はエラー" $
      tokenize "1 + a" `shouldBe` Left "unexpected character: a"

  describe "parse" $ do
    it "整数リテラル" $
      parse [TInt 5] `shouldBe` Right (Lit 5)
    it "加算" $
      parse [TInt 1, TPlus, TInt 2] `shouldBe` Right (Add (Lit 1) (Lit 2))
    it "減算" $
      parse [TInt 3, TMinus, TInt 1] `shouldBe` Right (Sub (Lit 3) (Lit 1))
    it "乗算が加算より優先される" $
      parse [TInt 1, TPlus, TInt 2, TStar, TInt 3]
        `shouldBe` Right (Add (Lit 1) (Mul (Lit 2) (Lit 3)))
    it "括弧で優先度を変える" $
      parse [TLParen, TInt 1, TPlus, TInt 2, TRParen, TStar, TInt 3]
        `shouldBe` Right (Mul (Add (Lit 1) (Lit 2)) (Lit 3))
    it "単項マイナス" $
      parse [TMinus, TInt 5] `shouldBe` Right (Neg (Lit 5))
    it "空入力はエラー" $
      parse [] `shouldBe` Left "unexpected end of input"

  describe "eval" $ do
    it "リテラルを評価する" $
      eval (Lit 7) `shouldBe` Right 7
    it "加算を評価する" $
      eval (Add (Lit 3) (Lit 4)) `shouldBe` Right 7
    it "乗除算を評価する" $
      eval (Mul (Lit 2) (Div (Lit 6) (Lit 3))) `shouldBe` Right 4
    it "ゼロ除算はエラー" $
      eval (Div (Lit 1) (Lit 0)) `shouldBe` Left "division by zero"
    it "単項マイナスを評価する" $
      eval (Neg (Lit 5)) `shouldBe` Right (-5)
    it "複合式を評価する: 1 + 2 * (3 - 4) = -1" $
      eval (Add (Lit 1) (Mul (Lit 2) (Sub (Lit 3) (Lit 4)))) `shouldBe` Right (-1)
