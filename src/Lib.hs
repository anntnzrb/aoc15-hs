module Lib (wordsWhen) where

-- | Splits a string into a list of substrings based on a given predicate.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'
