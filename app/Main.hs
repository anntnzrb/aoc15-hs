{-# LANGUAGE InstanceSigs #-}

module Main where

import qualified Day01 (part1, part2)
import qualified Day02 (part1, part2)

data Aoc = Aoc
  { day :: Int,
    part :: Int,
    input :: String,
    fn :: String -> String
  }

instance Show Aoc where
  show :: Aoc -> String
  show (Aoc d p _ _) = "Day " ++ show d ++ " :: Part " ++ show p

printAoc :: Aoc -> IO ()
printAoc (Aoc d p i f) = do
  file <- readFile i
  putStrLn $ show (Aoc d p i f) ++ "\n=> " ++ f file ++ "\n\n"

main :: IO ()
main = do
  printAoc (Aoc 1 1 "inputs/day01.in" Day01.part1) -- 280
  printAoc (Aoc 1 2 "inputs/day01.in" Day01.part2) -- 1797
  --
  printAoc (Aoc 2 1 "inputs/day02.in" Day02.part1) -- 1598415
  printAoc (Aoc 2 2 "inputs/day02.in" Day02.part2) -- 3812909