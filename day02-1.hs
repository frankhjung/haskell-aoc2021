{-
== Day 2: Part 1 - Dive

With test data this Should return: depth 10 * position 15 = 150

> cat day02.test | runhaskell day02-1.hs
> 150

> cat day02.data | runhaskell day02-1.hs
> 1604850

-}

-- line contains an action and an integer movement
parse :: [String] -> (String, Int)
parse [x,y] = (x, read y :: Int)

-- (action, movement) -> (horizontal, depth)
apply :: (String, Int) -> (Int, Int)
apply (action, move)
  | action == "forward" = (move, 0)
  | action == "down"    = (0, move)
  | action == "up"      = (0, negate move)
  | otherwise           = (0, 0)

-- [(horizontal, depth)] -> sum_horizontal * sum_depth
solve :: [(Int, Int)] -> Int
solve ts = hs * ds
  where
    hs = sum $ map fst ts
    ds = sum $ map snd ts

main :: IO ()
main = getContents >>= print . solve . map (apply . parse . words) . lines
