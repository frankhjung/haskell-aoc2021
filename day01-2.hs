{-
== Day 1: Part 2 - Sonar Sweep

https://adventofcode.com/2021/day/1 (part 2)

> cat day01.test | runghc day01-2
> 5

> cat day01.data | runghc day01-2
> 1737

-}

-- sum of there integers
sum3 :: Num a => a -> a -> a -> a
sum3 a b c = a + b + c

-- search 3 deep
solve :: [Int] -> Int
solve xs = sum $ map fromEnum $ zipWith (<) ys (drop 1 ys)
  where ys = zipWith3 sum3 xs (drop 1 xs) (drop 2 xs)

main :: IO ()
main = getContents >>= print . solve . map read . words

{- same as
main :: IO ()
main = do
  xs <- map read . words <$> getContents
  print $ solve xs
-}
