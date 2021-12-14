{-
== Day 09: Part 2 - Smoke Basin

Find and product of first 3 largest sized basins.
Size of basin is the number of points in them less than 9.

2199943210
3987894921
9856789892
8767896789
9899965678

λ> sr = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

λ> ss = lines sr
["2199943210","3987894921","9856789892","8767896789","9899965678"]

λ> si = map (map digitToInt) ss

λ> si = map (map digitToInt) (lines sr)
[[2,1,9,9,9,4,3,2,1,0],[3,9,8,7,8,9,4,9,2,1],[9,8,5,6,7,8,9,8,9,2],[8,7,6,7,8,9,6,7,8,9],[9,8,9,9,9,6,5,6,7,8]]

=== test data

sr = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

== Answers

make check day09-2

cat day09.test | ./day09-2
1134                        -- 9 * 14 * 9 = 1134

cat day09.data | ./day09-2
1280496

-}

import           Data.Char  (digitToInt)
import           Data.List  (nub, sortOn)
import           Data.Maybe (catMaybes, isNothing)
import           Data.Ord   (Down (Down))
import           Maybes     (fromJust)

-- 2D Grid is heights at locations.
type Grid = ((Int,Int),[[Int]])

-- Heights are 0-9
type Height = Int

-- Put heights into 2D array.
type Location = (Int, Int)

-- Parse input into 2D array.
parse :: String -> Grid
parse raw = ((rows,cols), map (map digitToInt) (lines raw))
  where
    cols = (pred . length . head . lines) raw
    rows = (pred . length . lines) raw

-- get height at a point of list of heights
getHeight :: Grid -> Location -> Maybe Height
getHeight grid (x, y)
  | x < 0 || x > rows = Nothing
  | y < 0 || y > cols = Nothing
  | otherwise = Just (heights !! x !! y)
  where
    heights = snd grid
    rows = (fst . fst) grid
    cols = (snd . fst) grid

-- get heights adjacent to a given point
getHeightAdj :: Grid -> Location -> [Height]
getHeightAdj grid (x,y) =
  let left = getHeight grid (x,y-1)
      right = getHeight grid (x,y+1)
      above = getHeight grid (x-1,y)
      below = getHeight grid (x+1,y)
  in  catMaybes [left,right,above,below]

-- Is height the minimum of all adjacent points?
isMin :: Grid -> Location -> Bool
isMin grid loc = fromJust (getHeight grid loc) < minimum (getHeightAdj grid loc)

-- Test is location is in a basin.
isInBasin :: Grid -> Location -> Bool
isInBasin grid (x, y)
  | x < 0 || x > rows = False
  | y < 0 || y > cols = False
  | isNothing h = False
  | h == Just 9 = False
  | otherwise   = True
  where
    rows = (fst . fst) grid
    cols = (snd . fst) grid
    h = getHeight grid (x, y)

-- get all unique adjacent locations to a given point
getBasin :: Grid -> Location -> [Location]
getBasin grid loc = getBasin' grid [] [loc]
  where
    -- worker get all unique adjacent locations to a given point
    --           heights  visited       current       result
    getBasin' :: Grid -> [Location] -> [Location] -> [Location]
    getBasin' grid' visited tovisit
      | null tovisit  = visited
      | otherwise     = getBasin' grid' visited' tovisit'
      where
        (x,y) = head tovisit
        visited' = (x,y):visited
        newlocs = filter (isInBasin grid') [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
        tovisit' = filter (`notElem` visited') $ nub (newlocs ++ tail tovisit)

-- iterate through grid of heights to find all min heights
getLows :: Grid -> [Location]
getLows grid = [(x,y) | (x,y) <- locs, isMin grid (x,y)]
  where
    locs = [(x,y) | x <- [0..rows], y <- [0..cols]]
    rows = (fst . fst) grid
    cols = (snd . fst) grid

-- Get size of all basins sourced from the known low points.
getBasins :: Grid -> [Int]
getBasins grid = map (length . getBasin grid) (getLows grid)

-- Solution is product of the sizes of the 3 largest basins.
solve :: Grid -> Int
solve = product . take 3 . sortOn Down . getBasins

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
