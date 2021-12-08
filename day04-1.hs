{-
== Day 04 : Part 1 - Giant Squid Bingo

Win bingo if row or column is matched. (Use transpose to get columns)
Final score is last drawn number * sum of all unmarked numbers.
Where unmarked numbers can be obtained via set difference.

=== Example

:m + Data.List

Given bingo game:

λ> bs = [[22,13,17], [8,2,23], [21,9,14]]

Then columns is transpose of bs:

λ> transpose bs
[[22,8,21],[13,2,9],[17,23,14]]

Given a list of drawn numbers:

ds = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

and suppose current drawn number is at index 9 (14).

ds' = [7,4,9,5,11,17,23,2,0,14]

Then the sum of all unmarked numbers is:

sum (concat bs \\ ds')
64

So result is last (last ds') * sum (concat bs \\ ds')

=== Input

Data is read from a file and is similar to:

  7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
  8  2 23  4 24
  21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19

  3 15  0  2 22
  9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
  2  0 12  3  7

== Answers

  $ make day04-1

  $ cat day04.test | ./day04-1
  4512

  $ cat day04.data | ./day04-1
  6592

 -}

import           Data.List       (transpose, (\\))
import           Data.List.Split (chunksOf, splitOn)

-- Return winning bingo card given drawn numbers.
checkCards :: [Int] -> [[[Int]]] -> [[Int]]
checkCards ds = concat . filter (isBingo ds)

-- Call Bingo! if any row or column is full matched.
isBingo :: [Int] -> [[Int]] -> Bool
isBingo ds bs = isMatch ds bs || isMatch ds (transpose bs)
  where isMatch xs ys = null . minimum $ map (\\xs) ys

getWinner :: Int -> [Int] -> [[[Int]]] -> (Int, [[Int]])
getWinner n draws cards
  | null winner = getWinner (n+1) draws cards
  | otherwise    = (n, winner)
  where
    ds = take n draws
    winner = checkCards ds cards

solve :: [Int] -> [[[Int]]] -> Int
solve draws cards = lastDraw * sumUnmatchedOnCard
  where
    (n, card) = getWinner 5 draws cards
    lastDraw  = draws !! (n-1)
    sumUnmatchedOnCard = sum $ concat card \\ take n draws

main :: IO ()
main = do
  -- parse content into draws and bingo games
  raw <- getContents
  let
    -- drawn bingo numbers
    draws = map read $ splitOn "," . head $ lines raw :: [Int]
    -- a list of 5x5 bingo cards
    bingos_raw = map words $ filter (/="") $ tail (lines raw)
    bingos_cards = chunksOf 5 (map (map read) bingos_raw :: [[Int]])
  -- show winning solution
  print $ solve draws bingos_cards
