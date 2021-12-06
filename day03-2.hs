{-
== Day 3 : Part 2 - Life Support Rating

binary input of O2 generator rating & CO2 scrubber rating

life support rating = generator * scrubber

- Keep only numbers selected by the bit criteria for the type of rating
  value for which you are searching. Discard numbers which do not match the
  bit criteria.

- If you only have one number left, stop; this is the rating value for
  which you are searching. Otherwise, repeat the process, considering the
  next bit to the right.

The bit criteria depends on which type of rating value you want to find:

- To find oxygen generator rating, determine the most common value (0 or 1)
  in the current bit position, and keep only numbers with that bit in that
  position. If 0 and 1 are equally common, keep values with a 1 in the
  position being considered.

- To find CO2 scrubber rating, determine the least common value (0 or 1) in
  the current bit position, and keep only numbers with that bit in that
  position. If 0 and 1 are equally common, keep values with a 0 in the
  position being considered.

=== Explore

Convert test data to decimal to test `bintodec`
> [4,30,22,23,21,15,7,28,16,25,2,10]

Modules: Prelude Data.List Data.Ord Control.Applicative

Read in file contents:

λ> raw <- readFile "day03.test"
λ> raw
"00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"

λ> xs = lines raw
["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

Sort and group by most common element by column index:

λ> zs = (take 2 . concatMap bitsfreq ) . transpose ys
["1111111","00000"]

λ> map length zs
[7,5]

λ> allequal $ map length zs
False

== Answers

  $ cat day03.test | runhaskell day03-2
  230

  $ cat day03.data | runhaskell day03-2
  3570354

 -}

import           Data.Char (digitToInt)
import           Data.List (group, sort, sortOn, transpose)
import           Data.Ord  (Down (..))

allequal :: (Eq a) => [a] -> Bool
allequal xs = all (== head xs) (tail xs)

-- Convert string containing binary digits to a decimal value.
bintodec :: String -> Int
bintodec ds = foldl ((+) . (2 *)) 0 (map digitToInt ds)

-- Determine most frequent bits by index.
bitsfreq :: String -> [String]
bitsfreq = sortOn (Down . length) . group . sort

-- Current most frequent bits at the specified index (`n`).
currentBits :: Int -> [String] -> [String]
currentBits n = (take 2 . concatMap bitsfreq ) . drop n . transpose

solve :: [String] -> Int
solve ss = generator 0 ss * scrubber 0 ss
  where
    -- O2 generator rating
    generator :: Int -> [String] -> Int
    generator n xs
      | length xs == 1 = bintodec (head xs)
      | otherwise      = generator (n+1) (filter (\c -> p == c !! n) xs)
        where
          p = gen (currentBits n xs)
    -- if bit frequencies are equal set oxygen rating to '1'
    gen :: [String] -> Char
    gen xs
      | allequal (map length xs) = '1'
      | otherwise                = (head . head) xs
    -- CO2 scrubber rating
    scrubber :: Int -> [String] -> Int
    scrubber n xs
      | length xs == 1 = bintodec (head xs)
      | otherwise      = scrubber (n+1) (filter (\c -> p == c !! n) xs)
        where
          p = scr (currentBits n xs)
    -- if bit frequencies are equal set carbon dioxide rating to '0'
    scr :: [String] -> Char
    scr xs
      | allequal (map length xs) = '0'
      | otherwise                = (head . last) xs

main :: IO ()
main = getContents >>= print . solve . lines
