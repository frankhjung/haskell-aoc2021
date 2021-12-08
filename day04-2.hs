{-
== Day 04 : Part 2 - Giant Squid Bingo

Choose last board to win.

Method:

* start from the last entry
* check for winners
* if winners less than # of boards then the board that is non-empty is the
  last winning board

== Answers

  $ make day04-2

  $ cat day04.test | ./day04-2
  1924

  $ cat day04.data | ./day04-2
  31755

 -}

import           Data.List       (transpose, (\\))
import           Data.List.Split (chunksOf, splitOn)

-- Return winning bingo cards given drawn numbers.
checkCards :: [Int] -> [[[Int]]] -> [[Int]]
checkCards ds = concat . filter (isBingo ds)

-- Call Bingo! if any row or column is full matched.
isBingo :: [Int] -> [[Int]] -> Bool
isBingo ds bs = isMatch ds bs || isMatch ds (transpose bs)
  where isMatch xs ys = null . minimum $ map (\\xs) ys

getLoser :: Int -> [Int] -> [[[Int]]] -> (Int, [[Int]])
getLoser n draws cards
  | length winners == games = getLoser (n-1) draws cards
  | otherwise               = (n, concat cards \\ winners)
  where
    games   = (length . concat) cards
    ds      = take n draws
    winners = checkCards ds cards

solve :: [Int] -> [[[Int]]] -> Int
solve draws cards = lastDraw * sumUnmatchedOnCard
  where
    (n, card) = getLoser (length draws) draws cards
    lastDraw  = draws !! n
    sumUnmatchedOnCard = sum $ concat card \\ take (n+1) draws

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
  -- show "losing" solution
  print $ solve draws bingos_cards
