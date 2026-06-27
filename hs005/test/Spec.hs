module Main where

import Compiler (Expr (..), Instr (..), Token (..), compile, parse, run, tokenize)
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

  describe "compile" $ do
    it "リテラルをPushに変換する" $
      compile (Lit 42) `shouldBe` [Push 42]
    it "加算を命令列に変換する" $
      compile (Add (Lit 1) (Lit 2)) `shouldBe` [Push 1, Push 2, IAdd]
    it "減算を命令列に変換する" $
      compile (Sub (Lit 3) (Lit 1)) `shouldBe` [Push 3, Push 1, ISub]
    it "乗算を命令列に変換する" $
      compile (Mul (Lit 2) (Lit 3)) `shouldBe` [Push 2, Push 3, IMul]
    it "除算を命令列に変換する" $
      compile (Div (Lit 6) (Lit 3)) `shouldBe` [Push 6, Push 3, IDiv]
    it "単項マイナスを命令列に変換する" $
      compile (Neg (Lit 5)) `shouldBe` [Push 5, INeg]
    it "複合式の命令列順序: 1 + 2 * (3 - 4)" $
      compile (Add (Lit 1) (Mul (Lit 2) (Sub (Lit 3) (Lit 4))))
        `shouldBe` [Push 1, Push 2, Push 3, Push 4, ISub, IMul, IAdd]

  describe "run" $ do
    it "Pushで定数を積む" $
      run [Push 42] `shouldBe` Right 42
    it "IAddで加算する" $
      run [Push 3, Push 4, IAdd] `shouldBe` Right 7
    it "ISubで減算する（下 - 上）" $
      run [Push 3, Push 1, ISub] `shouldBe` Right 2
    it "IMulで乗算する" $
      run [Push 2, Push 3, IMul] `shouldBe` Right 6
    it "IDivで除算する" $
      run [Push 6, Push 3, IDiv] `shouldBe` Right 2
    it "ゼロ除算はエラー" $
      run [Push 1, Push 0, IDiv] `shouldBe` Left "division by zero"
    it "INegで符号を反転する" $
      run [Push 5, INeg] `shouldBe` Right (-5)
    it "スタック不足はエラー" $
      run [IAdd] `shouldBe` Left "stack underflow"

  describe "compile + run（結合テスト）" $ do
    it "リテラルを評価する" $
      run (compile (Lit 7)) `shouldBe` Right 7
    it "加算を評価する" $
      run (compile (Add (Lit 3) (Lit 4))) `shouldBe` Right 7
    it "乗除算を評価する" $
      run (compile (Mul (Lit 2) (Div (Lit 6) (Lit 3)))) `shouldBe` Right 4
    it "ゼロ除算はエラー" $
      run (compile (Div (Lit 1) (Lit 0))) `shouldBe` Left "division by zero"
    it "単項マイナスを評価する" $
      run (compile (Neg (Lit 5))) `shouldBe` Right (-5)
    it "複合式を評価する: 1 + 2 * (3 - 4) = -1" $
      run (compile (Add (Lit 1) (Mul (Lit 2) (Sub (Lit 3) (Lit 4))))) `shouldBe` Right (-1)
