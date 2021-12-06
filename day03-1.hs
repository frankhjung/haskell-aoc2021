{-
== Day 3 : Part 1 - Binary Diagnostic

binary input of gamma & epsilon

power consumption = gamma * epsilon

gamma rate = most common bit
epsilon rate = least common bit

convert rates from binary to decimal and multiply together

=== Explore

Convert test data to decimal to test `bintodec`
> [4,30,22,23,21,15,7,28,16,25,2,10]

Modules: Prelude Data.List Data.Ord Control.Applicative

Transpose rows and columns:

λ> contents <- readFile "day03.test"
λ> contents
"00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"
λ> xs = lines contents
["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
λ> ys = transpose xs
["011110011100","010001010101","111111110000","011101100011","000111100100"]

Sort and group by most common element by index:

λ> zs = map (sortBy (flip $ comparing length) . group . sort) ys
[["1111111","00000"],["0000000","11111"],["11111111","0000"],["1111111","00000"],["0000000","11111"]]

gamma = (head . head)
epsilon = (head . last)

λ> map gamma zs
"10110"

λ> map epsilon zs
"01001"

λ> liftA2 (,) (map gamma) (map epsilon) zs
("10110","01001")

Then convert to decimal and multiply:

λ> liftA2 (*) (bintodec . read . map gamma) (bintodec . read . map epsilon) zs
198


== Answers

  $ cat day03.test | runhaskell day03-1
  198

  $ cat day03.data | runhaskell day03-1
  4147524

 -}

import           Control.Applicative (liftA2)
import           Data.Char           (digitToInt)
import           Data.List           (group, sort, sortOn, transpose)
import           Data.Ord            (Down (..))

-- calculate gamma
gamma :: [[a]] -> a
gamma = head . head

-- calculate epsilon
epsilon :: [[a]] -> a
epsilon = head . last

-- calculate power consumption
power :: [[String]] -> Int
power = liftA2 (*) (bintodec . map gamma) (bintodec . map epsilon)

-- Convert string containing binary digits to a decimal value.
bintodec :: String -> Int
bintodec ds = foldl ((+) . (2 *)) 0 (map digitToInt ds)

-- Determine most frequent bits
bitsfreq :: String -> [String]
bitsfreq = sortOn (Down . length) . group . sort

solve :: [String] -> Int
solve xs = power zs
  where
    ys = transpose xs
    zs = map bitsfreq ys

main :: IO ()
main = getContents >>= print . solve . lines
