{-
== Day 08 : Part 1 - Seven Segment Search

Digits:

0: abcefg
1: cf
2: acdeg
3: acdfg
4: bcdf
5: abdfg
6: abdefg
7: acf
8: abcdefg
9: abcdfg

How many times do 1,4,7,8 appear in the signals?

=== Answers

  $ make check day08-1

  $ ./day08-1 day08.test
  26

  $ ./day08-1 day08.data
  352

 -}

import           System.Environment (getArgs)

-- Show number of overlapping horizontal and vertical lines.
-- one = "cf"        -- length 2
-- four = "bcdf"     -- length 4
-- seven = "acf"     -- length 3
-- eight = "abcdefg" -- length 7
solve :: String -> Int
solve = length . filter (`elem` [2,3,4,7]) . parse

-- Parse input to retreive the digits.
parse :: String -> [Int]
parse = concatMap (map length . tail . words . dropWhile ('|' /=)) . lines

-- Get filename from command line argument.
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print $ solve contents
