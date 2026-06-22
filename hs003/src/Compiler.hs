module Compiler (readSource) where

readSource :: FilePath -> IO String
readSource = readFile
