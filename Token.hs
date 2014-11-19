module Token (tokenize) where

import Data.Char

tokenize :: String -> Bool -> [String]
tokenize s stripPunctuation =
  let tokens = words s in
    if stripPunctuation
      then map (filter isAlphaNum) tokens
      else tokens
