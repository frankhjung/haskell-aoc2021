{-
== Day 7: Part 2 - The Treachery of Whales

Fuel costs incrementally more the further you travel.

i.e.

  cost = sum [1..distance]

=== Test

make check day07-2

$ time (cat day07.test | ./day07-2)
(5,168)
real  0m0.017s
user  0m0.010s
sys   0m0.004s

$ time (cat day07.data | ./day07-2)
(497,100727924)
real  0m0.017s
user  0m0.010s
sys   0m0.008s

-}

import           Data.List       (minimumBy)
import           Data.List.Split (splitOn)
import           Data.Ord        (comparing)

-- Calculate fuel for a given horizonatal line.
fuel :: Int -> [Int] -> Int
fuel m cs = sum $ map cost cs
  where cost x = sum [1..(abs . subtract m) x]

--         crabs    linw   next            fuel   [(line, fuel)]
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
    getFuel = minFuel crabs mean pred current ++ minFuel crabs mean succ current
    current = fuel mean crabs
    mean = sum crabs `div` length crabs

-- parse input split on commas
parse :: String -> [Int]
parse = map read . splitOn ","

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
