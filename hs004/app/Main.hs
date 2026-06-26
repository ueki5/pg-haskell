module Main where

import Compiler (eval, parse, tokenize)
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
            <> progDesc "Parse and evaluate an arithmetic expression"
            <> header "hs004 - a simple arithmetic compiler"
        )
  source <- readFile (sourceFile opts)
  case tokenize source >>= parse of
    Left err -> putStrLn ("Error: " ++ err) >> exitFailure
    Right ast -> do
      putStrLn ("AST: " ++ show ast)
      case eval ast of
        Left err -> putStrLn ("Error: " ++ err) >> exitFailure
        Right val -> print val
