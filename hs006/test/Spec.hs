module Main where

import CodeGen (codegen)
import Compiler (Expr (..), Instr (..), Token (..), compile, parse, tokenize)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
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

  describe "codegen" $ do
    it "プロローグにmainラベルを含む" $
      codegen [] `shouldContain` "main:"
    it "Push nをpushq命令に変換する" $
      codegen [Push 42] `shouldContain` "pushq $42"
    it "IAddをaddq命令に変換する" $
      codegen [IAdd] `shouldContain` "addq"
    it "ISubをsubq命令に変換する" $
      codegen [ISub] `shouldContain` "subq"
    it "IMulをimulq命令に変換する" $
      codegen [IMul] `shouldContain` "imulq"
    it "IDivをidivq命令に変換する" $
      codegen [IDiv] `shouldContain` "idivq"
    it "INegをnegq命令に変換する" $
      codegen [INeg] `shouldContain` "negq"
    it "IDivにゼロ除算チェックを含む" $
      codegen [IDiv] `shouldContain` ".Ldiv_zero_error"
    it "エピローグにprintf呼び出しを含む" $
      codegen [] `shouldContain` "call  printf"

  describe "compile + codegen + gcc（結合テスト）" $ do
    it "リテラルを評価する" $ do
      result <- compileAndRun (Lit 7)
      result `shouldBe` "7"
    it "加算を評価する" $ do
      result <- compileAndRun (Add (Lit 3) (Lit 4))
      result `shouldBe` "7"
    it "減算を評価する" $ do
      result <- compileAndRun (Sub (Lit 5) (Lit 3))
      result `shouldBe` "2"
    it "乗算を評価する" $ do
      result <- compileAndRun (Mul (Lit 2) (Lit 6))
      result `shouldBe` "12"
    it "除算を評価する" $ do
      result <- compileAndRun (Div (Lit 6) (Lit 3))
      result `shouldBe` "2"
    it "単項マイナスを評価する" $ do
      result <- compileAndRun (Neg (Lit 5))
      result `shouldBe` "-5"
    it "複合式を評価する: 1 + 2 * (3 - 4) = -1" $ do
      result <- compileAndRun (Add (Lit 1) (Mul (Lit 2) (Sub (Lit 3) (Lit 4))))
      result `shouldBe` "-1"

compileAndRun :: Expr -> IO String
compileAndRun expr =
  withSystemTempDirectory "hs006" $ \tmpDir -> do
    let asmPath = tmpDir </> "out.s"
        binPath = tmpDir </> "out"
    writeFile asmPath (codegen (compile expr))
    _ <- readProcess "gcc" [asmPath, "-o", binPath] ""
    output <- readProcess binPath [] ""
    return (takeWhile (/= '\n') output)
