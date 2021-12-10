{-
== Day 7: Part 1 - The Treachery of Whales

=== Explore

Input: crabs = [16,1,2,0,4,2,7,1,2,14]

Minimum fuel (37) if h = 2

A horizontal line is a function f(x) = constant.
Where a good start point for the constant is the mean of the input.

=== Test

$ time (cat day07.test | ./day07-1)
37
real	0m0.017s
user	0m0.004s
sys	0m0.010s

$ time (cat day07.data | ./day07-1)
359648
real	0m0.015s
user	0m0.005s
sys	0m0.009s

-}

import           Data.List       (minimumBy)
import           Data.List.Split (splitOn)
import           Data.Ord        (comparing)

-- Calculate fuel for a given line.
fuel :: Int -> [Int] -> Int
fuel m cs = sum $ map (abs . subtract m) cs

-- Recursive function to find the minimum fuel.
--         crabs    min    next            fuel   [(min, fuel)]
minFuel :: [Int] -> Int -> (Int -> Int) -> Int -> [(Int, Int)]
minFuel cs m g f
  | f' <= f = (m, f'):minFuel cs (g m) g f'
  | otherwise = []
  where
    f' = fuel m cs

-- Solve for given input returing sum of differences.
solve :: [Int] -> Int
solve crabs = (snd . minimumBy (comparing snd)) getFuel
  where
    getFuel = minFuel crabs mean succ current ++ minFuel crabs mean pred current
    current = fuel mean crabs
    mean = sum crabs `div` length crabs

-- parse input split on commas
parse :: String -> [Int]
parse = map read . splitOn ","

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
