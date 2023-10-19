{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad (forM_)
import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)
import qualified Day03 (part1, part2)
import Text.Printf (printf)

data Aoc = Aoc
  { day :: Int,
    input :: String,
    fns :: [String -> String]
  }

instance Show Aoc where
  show :: Aoc -> String
  show (Aoc d _ _) = "Day " ++ show d

printAoc :: Aoc -> IO ()
printAoc (Aoc d i fs) = do
  file <- readFile i
  forM_ (zip [1 :: Int ..] fs) $ \(p, f) -> do
    printf "Day %d :: Part %d\n=> %s\n\n" d p (f file)

main :: IO ()
main = do
  printAoc (Aoc 1 "inputs/day01.in" [Day01.part1, Day01.part2])
  printAoc (Aoc 2 "inputs/day02.in" [Day02.part1, Day02.part2])
  printAoc (Aoc 3 "inputs/day03.in" [Day03.part1, Day03.part2])