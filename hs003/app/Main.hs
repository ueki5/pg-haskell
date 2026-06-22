module Main where

import Compiler (readSource)
import Options.Applicative

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
            <> progDesc "Read and print a source file"
            <> header "hs003 - a simple arithmetic compiler"
        )
  contents <- readSource (sourceFile opts)
  putStr contents
