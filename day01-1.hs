{-
== Day 1: Part 1 - Sonar Sweep

https://adventofcode.com/2021/day/1

> cat day01.test | runghc day01-1
> 7

> cat day01.data | runghc day01-1
> 1696

=== Explore

Test data:

xs = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

λ> zipWith (<) xs (drop 1 xs)
[True,True,True,False,True,True,True,False,True]

There are two solutions:

λ> length $ filter (==True) $ zipWith (<) xs (drop 1 xs)
7

λ> sum $ map fromEnum $ zipWith (<) xs (drop 1 xs)
7

-}

solve :: [Int] -> Int
solve xs = sum $ map fromEnum $ zipWith (<) xs (drop 1 xs)

main :: IO ()
main = getContents >>= print . solve . map read . words

{- same as
main :: IO ()
main = do
  xs <- map read . words <$> getContents
  print $ solve xs
-}
