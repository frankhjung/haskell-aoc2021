{-
== Day 6: Part 1 - Lanternfish

=== Rules

Suppose you have a lanternfish with an internal timer value of 3:

- After one day, its internal timer would become 2.
- After another day, its internal timer would become 1.
- After another day, its internal timer would become 0.
- After another day, its internal timer would reset to 6, and it would
  create a new lanternfish with an internal timer of 8.
- After another day, the first lanternfish would have an internal timer of
  5, and the second lanternfish would have an internal timer of 7.

=== Explore

Input: 3,4,3,1,2

Initial state: 3,4,3,1,2
After  1 day:  2,3,2,0,1
After  2 days: 1,2,1,6,0,8
After  3 days: 0,1,0,5,6,7,8
After  4 days: 6,0,6,4,5,6,7,8,8
After  5 days: 5,6,5,3,4,5,6,7,7,8
After  6 days: 4,5,4,2,3,4,5,6,6,7
After  7 days: 3,4,3,1,2,3,4,5,5,6
After  8 days: 2,3,2,0,1,2,3,4,4,5
After  9 days: 1,2,1,6,0,1,2,3,3,4,8
After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

Solution to give is the count of latern fish after 18 days: 26
After 80 days there are 5934 laternfish.

Given day06.data and internal timer of 8, how many lanternfish would there
be after 80 days?

=== Test

$ cat day06.test | ./day06-1
5934

$ cat day06.data | ./day06-1
359999

-}

import           Data.List.Split (splitOn)

fish :: [Int] -> [Int]
fish [] = []
fish (f:fs)
  | f == 0    = 8:6:fish fs
  | otherwise = f-1:fish fs

-- repeat a function `f` `n` times given initial value `v`.
-- This generates a list of functions `f` of lenght `n`
-- then folds over the input using the function list.
apply :: Int -> (a -> a) -> a -> a
apply n f v = foldl (\s p -> p s) v [f | _ <- [1..n]]

solve :: Int -> [Int] -> Int
solve n xs = length (apply n fish xs)

main :: IO ()
main = interact $ show . solve 80 . map read . splitOn ","
