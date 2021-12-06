{-
== Day 2: Part 2 - Dive

With test data this Should return: 200

> cat day02.test | runhaskell day02-2.hs
> 900

> cat day02.data | runhaskell day02-2.hs
> 1685186100

=== Explore

λ> contents <- readFile "day02.test"

λ> ts = map (parse . words) (lines contents)

λ> scanl apply (0,0,0) ts
[(0,0,0),(5,0,0),(5,0,5),(13,40,5),(13,40,2),(13,40,10),(15,60,10)]

λ> foldl apply (0,0,0) ts
(15,60,10)

λ> solve $ foldl apply (0,0,0) ts
900

-}

-- parse input line to prepare initial tuple
parse :: [String] -> (String, Int)
parse [x, y] = (x, read y :: Int)

-- (action, movement) -> (horizontal, depth, aim)
apply :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
apply (height, depth, aim) (action, move)
  | action == "forward" = (height + move, depth + aim * move, aim)
  | action == "down"    = (height, depth, aim + move)
  | action == "up"      = (height, depth, aim - move)
  | otherwise           = (height, depth, aim)

-- [(horizontal, depth)] -> sum_horizontal * sum_depth
solve :: (Int, Int, Int) -> Int
solve (h, d, _) = h * d

main :: IO ()
main = getContents >>= print . solve . foldl apply (0,0,0) . map (parse . words) . lines

{- same as:
main :: IO ()
main = do
  contents <- getContents
  let ts = map (parse . words) (lines contents)
  print $ solve $ foldl apply (0, 0, 0) ts
-}

