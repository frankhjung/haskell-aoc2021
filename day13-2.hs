{-
== Day 13: Part 2 - Transparent Origami

Do remaining folds and retrieve the eight (8) capital letters from folded paper.

== Answer

make check day13-2

cat day13.test | ./day13-2

  #####
  #   #
  #   #
  #   #
  #####

(O)

cat day13.data | ./day13-2

  #  # ####   ## #  #   ## ###   ##    ##
  #  # #       # #  #    # #  # #  #    #
  #### ###     # ####    # #  # #       #
  #  # #       # #  #    # ###  #       #
  #  # #    #  # #  # #  # # #  #  # #  #
  #  # ####  ##  #  #  ##  #  #  ##   ##

(HEJHJRCJ)

-}
import           Data.Bifunctor  (bimap)
import           Data.Bool       (bool)
import           Data.List.Split (splitOn)
import qualified Data.Set        as S (Set, fromList, map, member, toList)

data Axis = XAxis | YAxis deriving (Show, Eq)
type Fold = (Axis, Int)
type Point = (Int, Int)
type Input = (S.Set Point, [Fold])

-- Parse the input:
--  1. part one is list of comma-separated numbers
--  2. part two is list of fold instructions
parse :: String -> Input
parse input = (S.fromList points, folds)
  where
    [pointsStr, foldsStr] = splitOn "\n\n" input  -- split input into two parts
    points = map parsePoint $ lines pointsStr     -- parse points
    folds = map parseFold $ lines foldsStr        -- parse folds

-- Parse a point from a string:
parsePoint :: String -> Point
parsePoint str = (x, y)
  where
    [x, y] = map read (splitOn "," str)

-- Parse a fold instruction from a string:
parseFold :: String -> Fold
parseFold str = bimap parseAxis read (head a, b)
  where
    [a, b] = splitOn "=" . drop 11 $ str  -- crude by effective for puzzle input

-- Parse the fold axis instructions:
parseAxis :: Char -> Axis
parseAxis 'x' = XAxis
parseAxis 'y' = YAxis
parseAxis _   = error "Invalid axis"

-- Fold a point along a given axis.
foldPoint :: Fold -> Point -> Point
foldPoint (XAxis, n) (x, y) = (n - abs(x-n), y)
foldPoint (YAxis, n) (x, y) = (x, n - abs(n-y))

-- Folds all points.
foldAll :: S.Set Point -> Fold -> S.Set Point
foldAll points fold = S.map (foldPoint fold) points

-- Do all folds and return the final set of points.
solve :: Input -> S.Set Point
solve (points, folds) = foldl foldAll points folds

-- For each point, print a '#' if in the set otherwise print a space.
printPoints :: S.Set Point -> [String]
printPoints points =
  let
    ps = S.toList points
    xMax = maximum $ map fst ps
    yMax = maximum $ map snd ps
  in
    [[bool ' ' '#' ((x,y) `S.member` points) | x <- [0 .. xMax]] | y <- [0 .. yMax]]

-- Read from STDIN to solve the puzzle.
main :: IO ()
main = getContents >>= mapM_ putStrLn . printPoints . solve . parse
