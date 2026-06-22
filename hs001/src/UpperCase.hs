module UpperCase (uppercaseText) where

import Data.Char (toUpper)

uppercaseText :: String -> String
uppercaseText = map toUpper
