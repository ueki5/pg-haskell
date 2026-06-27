module Main where

import Compiler (compile, parse, run, tokenize)
import Options.Applicative
import System.Exit (exitFailure)

data Options = Options
  { sourceFile :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument str (metavar "FILE" <> help "Source file to compile")

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Compile and run an arithmetic expression"
            <> header "hs005 - bytecode compiler with stack VM"
        )
  source <- readFile (sourceFile opts)
  case tokenize source >>= parse of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right ast -> do
      let instrs = compile ast
      putStrLn ("Instructions: " ++ show instrs)
      case run instrs of
        Left err -> putStrLn ("Error: " ++ err) >> exitFailure
        Right val -> print val
