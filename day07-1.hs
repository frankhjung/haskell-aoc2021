{-
== Day 7: Part 1 - The Treachery of Whales

=== Explore

Input: crabs = [16,1,2,0,4,2,7,1,2,14]

Minimum fuel (37) if h = 2

A horizontal line is a function f(x) = constant.
Where a good start point for the constant is the mean of the input.

Interestingly the mean for the test data set is not the minimum fuel cost.
It is for the problem data set.

=== Test

make check day07-1

$ time (cat day07.test | ./day07-1)
(2,37)
real  0m0.018s
user  0m0.000s
sys   0m0.016s

$ time (cat day07.data | ./day07-1)
(346,359648)
real  0m0.029s
user  0m0.012s
sys   0m0.015s

-}

import           Data.List       (minimumBy)
import           Data.List.Split (splitOn)
import           Data.Ord        (comparing)

-- Calculate fuel for a given horizonatal line.
fuel :: Int -> [Int] -> Int
fuel m cs = sum $ map (abs . subtract m) cs

-- Recursive function to find the minimum fuel.
--         crabs    line   next            fuel   [(line, fuel)]
minFuel :: [Int] -> Int -> (Int -> Int) -> Int -> [(Int, Int)]
minFuel cs m g f
  | f' <= f = (m, f') : minFuel cs (g m) g f'
  | otherwise = []
  where
    f' = fuel m cs

-- Solve for given input returing sum of differences.
solve :: [Int] -> (Int, Int)
solve crabs = minimumBy (comparing snd) getFuel
  where
    mean = sum crabs `div` length crabs
    current = fuel mean crabs
    getFuel = minFuel crabs mean pred current ++ minFuel crabs mean succ current

-- parse input split on commas
parse :: String -> [Int]
parse = map read . splitOn ","

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
