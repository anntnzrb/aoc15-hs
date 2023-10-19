module Day01 (part1, part2) where

-- | Given a character, returns 1 if it is an opening parenthesis '(' and -1 if
-- it is a closing parenthesis ')'.
countFloors :: (Num a) => Char -> a
countFloors '(' = 1
countFloors _ = -1

-- | Given a string of parentheses, returns the position of the character that
-- causes the count to go into negative territory for the first time.
findFirstBasement :: String -> Int
findFirstBasement = go 0 0
  where
    go :: Int -> Int -> String -> Int
    go _ _ [] = 0
    go i (-1) _ = i
    go i f (x : xs) = go (i + 1) (f + countFloors x) xs

{- ------------------------------------------------------------------------- -}

part1 :: String -> String
part1 =
  show
    . sum
    . map (countFloors :: Char -> Int)

part2 :: String -> String
part2 = show . findFirstBasement