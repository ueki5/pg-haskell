module Main where

import CodeGen (codegen)
import Compiler (compile, parse, tokenize)
import Options.Applicative
import System.Directory (getTemporaryDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (callProcess)

data Options = Options
  { sourceFile :: FilePath
  , outputFile :: FilePath
  , asmFile    :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "FILE" <> help "Source file to compile")
    <*> option str (long "output" <> short 'o' <> value "out" <> metavar "NAME" <> help "Output file name (default: out)")
    <*> optional (option str (short 'S' <> metavar "ASM" <> help "Assembly output file (default: TMPDIR/out.s)"))

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Compile an arithmetic expression to a native binary"
            <> header "hs006 - x86-64 native code generator"
        )
  source <- readFile (sourceFile opts)
  case tokenize source >>= parse of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right ast -> do
      let asm = codegen (compile ast)
      asmPath <- case asmFile opts of
        Just path -> return path
        Nothing   -> (</> "out.s") <$> getTemporaryDirectory
      writeFile asmPath asm
      callProcess "gcc" [asmPath, "-o", outputFile opts]
