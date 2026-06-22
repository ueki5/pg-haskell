module Main where

import Data.Char (toUpper)
import Options.Applicative

data Options = Options
  { files :: [FilePath]
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> many
      (argument str (metavar "FILE..." <> help "Input files (default: stdin)"))

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Convert lowercase text to uppercase"
            <> header "hs001 - a text uppercasing tool"
        )
  case files opts of
    [] -> getContents >>= putStr . map toUpper
    fs -> mapM_ processFile fs

processFile :: FilePath -> IO ()
processFile path = readFile path >>= putStr . map toUpper
