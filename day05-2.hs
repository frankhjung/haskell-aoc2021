{-
== Day 05 : Part 1 - Hydrothermal Venture

=== Input

Test data:

  0,9 -> 5,9
  8,0 -> 0,8
  9,4 -> 3,4
  2,2 -> 2,1
  7,0 -> 7,4
  6,4 -> 2,0
  0,9 -> 2,9
  3,4 -> 1,4
  0,0 -> 8,8
  5,5 -> 8,2

=== Answers

  $ make day05-2

  $ ./day05-2 day05.test
  12

  $ ./day05-2 day05.data


 -}

import           Data.Char                    (isDigit)
import           Data.List                    (group, sort)
import           System.Environment           (getArgs)
import           Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S,
                                               skipSpaces, string)

data Vent = Vent { x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int }
              deriving Show

vent :: ReadP Vent
vent = do
  c1 <- munch1 isDigit
  _ <- char ','
  r1 <- munch1 isDigit
  _ <- skipSpaces
  _ <- string "->"
  _ <- skipSpaces
  c2 <- munch1 isDigit
  _ <- char ','
  r2 <- munch1 isDigit
  return (Vent (read c1) (read r1) (read c2) (read r2))

-- Show number of overlapping horizontal and vertical lines.
solve :: String -> Int
solve contents = overlaps
  where
    -- parse data
    vents = getVents contents
    -- build grid
    grid = concatMap line vents
    -- summarise grid
    overlaps = length (filter (> 1) (map length (group (sort grid))))

-- Get range of index given that start and end not given in numerical order.
range :: Int -> Int -> [Int]
range x y
  | x < y     = [x..y]
  | otherwise = reverse [y..x]

-- Get diagonal indices.
diagonals :: Int -> Int -> Int -> Int -> [(Int, Int)]
diagonals c1 r1 c2 r2 = zip (range c1 c2) (range r1 r2)

-- Build horizontal and vertical lines.
line :: Vent -> [(Int,Int,Int)]
line v
  | x1 v == x2 v || y1 v == y2 v = [(x,y,1) | x <- range (x1 v) (x2 v), y <- range (y1 v) (y2 v)]
  | otherwise = [(x,y,1) | (x,y) <- diagonals (x1 v) (y1 v) (x2 v) (y2 v)]

-- Parse input to retreive the grid location of the vents.
getVents :: String -> [Vent]
getVents raw = map fst $ concatMap (readP_to_S vent) (lines raw) :: [Vent]

-- Get filename from command line argument.
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print (solve contents)
