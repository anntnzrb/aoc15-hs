module Day03 (part1, part2) where

import Data.List (nub, scanl')

-- | A type alias for a coordinate, represented as a tuple of two integers.
type Coord = (Int, Int)

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- | Returns the coordinate change corresponding to the given direction
-- character.  '^' moves up, 'v' moves down, '>' moves right, '<' moves left,
-- and any other character results in no movement.
dirToMove :: Char -> Coord
dirToMove c
  | c == '^' = (0, 1)
  | c == 'v' = (0, -1)
  | c == '>' = (1, 0)
  | c == '<' = (-1, 0)
  | otherwise = (0, 0)

-- | Given a list of coordinates, returns a list of positions obtained by adding
-- each coordinate to the previous position.
positions :: [Coord] -> [Coord]
positions = scanl' addCoords (0, 0)

-- | Given two lists of coordinates, returns a tuple of two lists of coordinates.
positions2 :: [Coord] -> [Coord] -> ([Coord], [Coord])
positions2 c1 c2 = (scanl' addCoords (0, 0) c1, scanl' addCoords (0, 0) c2)

-- | Splits a string into two parts by folding over it from right to left.
splitSantaMoves :: String -> (String, String)
splitSantaMoves = foldr (\c (s1, s2) -> (c : s2, s1)) ("", "")

-- | Returns a list of unique coordinates from the input list.
validPositions :: [Coord] -> [Coord]
validPositions = nub

-- | Returns a list of unique coordinates that are present in both input lists.
validPositions2 :: ([Coord], [Coord]) -> [Coord]
validPositions2 (c1, c2) = nub (c1 ++ c2)

part1 :: String -> String
part1 =
  show
    . length
    . nub
    . validPositions
    . positions
    . map dirToMove

part2 :: String -> String
part2 input = show . length . validPositions2 . positions2 coords1 $ coords2
  where
    (input1, input2) = splitSantaMoves input :: (String, String)
    coords1 = map dirToMove input1 :: [Coord]
    coords2 = map dirToMove input2 :: [Coord]