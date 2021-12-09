{-
== Day 05 : Part 1 - Hydrothermal Venture

=== Explore - parsing by hand

λ> x = "0,9 -> 5,9"
λ> map (splitOn ",") $ (words . filter (`notElem` "->")) x
[["0","9"],["5","9"]]

λ> map read $ concat . map (splitOn ",") $ (words . filter (`notElem` "->")) x :: [Int]
[0,9,5,9]

λ> x = "58,85 -> 917,944"
λ> map read $ concat $ map (splitOn ",") $ (words . filter (`notElem` "->")) x :: [Int]
[58,85,917,944]

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

  $ make day05-1

  $ ./day05-1 day05.test
  5

  $ ./day05-1 day05.data
  5442

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

-- Build horizontal and vertical lines.
line :: Vent -> [(Int,Int,Int)]
line v = [(x,y,1) | x <- range (x1 v) (x2 v), y <- range (y1 v) (y2 v)]

-- Parse input to retreive the grid location of the vents.
-- This ignores any diagonal lines.
getVents :: String -> [Vent]
getVents raw = filter (\(Vent c1 r1 c2 r2) -> c1 == c2 || r1 == r2) vents
  where
    vents = map fst $ concatMap (readP_to_S vent) (lines raw) :: [Vent]

-- Get filename from command line argument.
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print (solve contents)
