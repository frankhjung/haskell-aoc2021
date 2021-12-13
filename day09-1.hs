{-
== Day 09: Part 1 - Smoke Basin

Find and sum low points of input (0 < 9)
 1
2199943210 <- 0
3987894921
9856789892 <- 5
8767896789
9899965678 <- 5

lows = 1, 0, 5, 5

Consider values above & below, left & right.

The risk level of a low point is 1 plus its height. In the above example,
the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk
levels of all low points in the heightmap is therefore 15.

λ> sr = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

λ> ss = lines sr
λ> ss
["2199943210","3987894921","9856789892","8767896789","9899965678"]

λ> si = map (map digitToInt) (lines sr)

λ> si = map (map digitToInt) ss
λ> si
[[2,1,9,9,9,4,3,2,1,0],[3,9,8,7,8,9,4,9,2,1],[9,8,5,6,7,8,9,8,9,2],[8,7,6,7,8,9,6,7,8,9],[9,8,9,9,9,6,5,6,7,8]]

== Answers

make check day09-1

cat day09.test | ./day09-1
15

cat day09.data | ./day09-1
468

-}

import           Data.Char  (digitToInt)
import           Data.Maybe (catMaybes)
import           Maybes     (fromJust)

-- Heights are 0-9
type Height = Int

-- Parse input into 2D array.
parse :: String -> [[Height]]
parse = map (map digitToInt) . lines

-- Put heights into 2D array.
type Location = (Int, Int)

-- get height at a point of list of heights
getHeight :: Location -> [[Height]] -> Maybe Height
getHeight (x, y) grid
  | x < 0 || x > rows = Nothing
  | y < 0 || y > cols = Nothing
  | otherwise = Just (grid !! x !! y)
  where
    rows = pred $ length grid
    cols = pred $ length (head grid)

-- get heights adjacent to a given point
getHeightAdj :: Location -> [[Height]] -> [Height]
getHeightAdj (x, y) grid =
  let left = getHeight (x, y-1) grid
      right = getHeight (x, y+1) grid
      above = getHeight (x-1, y) grid
      below = getHeight (x+1, y) grid
  in  catMaybes [left,right,above,below]

-- Is height the minimum of all adjacent points?
isMin :: Location -> [[Height]] -> Bool
isMin loc grid = fromJust (getHeight loc grid) < minimum (getHeightAdj loc grid)

-- iterate through grid of heights to find all min heights
getLows :: [[Height]] -> [Height]
getLows grid = [ fromJust (getHeight (x,y) grid) | (x,y) <- locs, isMin (x,y) grid ]
  where
    rows = pred $ length grid
    cols = pred $ length (head grid)
    locs = [(x,y) | x <- [0..rows], y <- [0..cols]]

solve :: [[Height]] -> Int
solve = sum . map succ . getLows

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
