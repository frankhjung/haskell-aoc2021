{-
== Day 6: Part 2 - Lanternfish

=== Explore

Input: 3,4,3,1,2

After 256 days: 26984457539

Too large to keep as a list, instead create a list of (value, count)
and work on that.

=== Test

$ cat day06.test | ./day06-2
26984457539

$ cat day06.data | ./day06-2
?

-}

import           Control.Arrow   ((&&&))
import           Data.List       (group, sort)
import           Data.List.Split (splitOn)

frequency :: [Int] -> [(Int, Int)]
frequency xs = map (head &&& length) $ (group . sort) xs

-- fish days timer -> count of fish
fish :: Int -> Int -> Integer
fish 0 _        = 1
fish days 0     = fish (days - 1) 6 + fish (days - 1) 8
fish days timer = fish (days - 1) (timer - 1)

solve :: Int -> [Int] -> Integer
solve n xs = sum $ map (\(f, d) -> fish n f * fromIntegral d) (frequency xs)

main :: IO ()
main = interact $ show . solve 256 . map read . splitOn ","
