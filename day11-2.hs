{-
== Day 11: Part 2 - Dumbo Octopus

Return the step when all octopuses have flashed.

=== Example Data

  5483143223
  2745854711
  5264556173
  6141336146
  6357385478
  4167524645
  2176841721
  6882881134
  4846848554
  5283751526

- in this example, all octopuses flash at step 195

=== References

- https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html
- https://hackage.haskell.org/package/base-4.13.0.0/docs/Data-Ix.html

=== Answers

cat day11.test | ./day11-2
195

cat day11.data | ./day11-2
210

-}
import           Data.Array.Unboxed (Array, Ix (inRange, rangeSize), accum,
                                     assocs, bounds, elems, listArray)
import           Data.Char          (digitToInt)

-- Octopus energy levels
data Octopus = Exhausted | Energized | Latent Int deriving (Show, Eq)

-- Cave location of octopuses
type Cave = Array (Int, Int) Octopus

-- increment energy levels of an octopus
incrEnergy :: Octopus -> Octopus
incrEnergy o =  case o of
    Latent n -> if succ n >= 10 then Energized else Latent (succ n)
    _        -> o

-- resets energy levels of an octopus
reset :: Octopus -> Octopus
reset Exhausted = Latent 0
reset o         = o

-- step through the cave octopus energy flashes
step :: Cave -> (Cave, Int)
step c = (c'', flashes)
  where
    c' = incrEnergy <$> c -- increment energy levels
    completed = complete c' -- cascade octopus flashes until all are exhausted
    flashes = (length . filter (== Exhausted) . elems) completed -- count flashes
    c'' = reset <$> completed -- reset energy levels

-- complete the cave, returning the cave with all octopuses at their final energy levels
complete :: Cave -> Cave
complete c =
  case [i | (i, Energized) <- assocs c] of
    [] -> c
    energised ->
      let bs = bounds c
          nearby = filter (inRange bs) $ neighbours =<< energised -- neighbours of each energised cell
          collateral = [(i, incrEnergy) | i <- nearby, i `notElem` energised] -- update nearby cells
          changed = [(i, const Exhausted) | i <- energised] ++ collateral -- reset energised cells
          c' = accum (flip id) c changed -- update cave
      in complete c'

-- get neighbours of an octopus (ignore current octopus)
neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]

-- Parse input into 2D array.
parse :: String -> Cave
parse raw = listArray bs octopuses
  where
    rows = lines raw -- split into rows (as is \n delimited)
    bs = ((0, 0), (length rows - 1, length (head rows) - 1)) -- get row & col bounds
    octopuses = Latent . digitToInt <$> concat rows -- convert to octopuses

-- Count the number of flashes after 100 steps.
solve :: Cave -> Int
solve cave = go cave
  where
    oa = rangeSize (bounds cave) -- number of octopuses in the cave
    go :: Cave -> Int
    go c = case step c of
      (c', f') | f' == oa    -> 1 -- all octopuses have flashed
               | otherwise  -> succ (go c') -- increment step and repeat

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
