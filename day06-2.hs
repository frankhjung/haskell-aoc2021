{-
== Day 6: Part 2 - Lanternfish

=== Explore

Input: fish = [3,4,3,1,2]

After 256 days: 26984457539

Too large to keep as a list, instead create a list of (timer, count)
and work on that. Use a Map so can update in place.

One last trick is to group fish with the same timers. That way you only need to
mulptiply by the number of fish in that group.

=== Test

$ time (cat day06.test | ./day06-2)
26984457539
real	0m0.019s
user	0m0.007s
sys	0m0.011s

$ time (cat day06.data | ./day06-2)
1631647919273
real	0m0.018s
user	0m0.006s
sys	0m0.010s

-}

import           Data.Function   ((&))
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type School = M.Map Int Int -- fish (timer, count)

-- some fish constants
breedFish :: Int
breedFish = negate 1  -- reduce fish timer by -1
spawnFish :: Int
spawnFish = 8         -- new fish timer is set to 8 days
resetFish :: Int
resetFish = 6         -- fish timer is reset to 6 days

-- build a school from a list of fish
buildSchool :: [Int] -> School
buildSchool = foldl addToSchool M.empty

-- add fish day to school of fish
addToSchool :: School -> Int -> School
addToSchool school n = M.insertWith (+) n 1 school

-- age the school by one day
ageSchool :: School -> School
ageSchool school =
  let
    agedSchool = M.mapKeys (subtract 1) school            -- decrement timers
    spawnCount = M.findWithDefault 0 breedFish agedSchool -- count fish to spawn
  in
    M.delete breedFish agedSchool             -- remove bred fish
    & M.insertWith (+) resetFish spawnCount   -- reset timers for bred fish
    & M.insert spawnFish spawnCount           -- add spawned fish

-- breed school of fish for n days
schoolGenerations :: Int -> School -> School
schoolGenerations n school = iterate ageSchool school !! n

-- get size of school
schoolSize :: School -> Int
schoolSize = M.foldl' (+) 0

-- Solve for a 256 days.
solve :: [Int] -> Int
solve = schoolSize . schoolGenerations 256 . buildSchool

-- parse input split on commas
parse :: String -> [Int]
parse = map read . splitOn ","

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
