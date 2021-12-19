{-
== Day 11: Part 1 - Dumbo Octopus

Count the number of flashes after 100 steps.

This is a slightly modified solution by Alan Malloy.
See https://github.com/amalloy/aoc-2021/blob/main/day11/src/Main.hs

=== Rules

Energy levels: [0..9]

- energy levels increase by 1
- energy levels > 9 flash
  - all adjacent levels increase by 1 (including diagonals)
  - if this causes a level to go above 9, it flashes
    this continues until the level is no longer above 9
  - only one flash per step (10)
  - energy levels reset to 0 after a flash

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

- in this example, after 10 steps there have been 204 flashes

=== References

- https://mmhaskell.com/blog/2019/5/6/making-arrays-mutable
- https://wiki.haskell.org/Arrays#Mutable_IO_arrays_.28module_Data.Array.IO.29
- my hackerrank code using arrays challenge63.hs, challenge68.hs, fp41.hs
- https://youtu.be/55d77RTDvqU

=== Answers

cat day11.test | ./day11-1
1656

cat day11.data | ./day11-1
1634

-}
import           Data.Array.Unboxed (Array, Ix (inRange), accum, assocs, bounds,
                                     elems, listArray)
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
revitalise :: Octopus -> Octopus
revitalise Exhausted = Latent 0
revitalise o         = o

-- step through the cave octpus energy flashes
step :: Cave -> (Cave, Int)
step c = (c'', flashes)
  where
    c' = fmap incrEnergy c
    completed = complete c'
    flashes = length . filter (== Exhausted) . elems $ completed
    c'' = fmap revitalise completed

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
parse raw = listArray ix octopi
  where
    rows = lines raw
    ix = ((0, 0), (length rows - 1, length (head rows) - 1))
    octopi = fmap (Latent . digitToInt) (concat rows)

-- Count the number of flashes after 100 steps.
solve :: Cave -> Int
solve = runCave 100 0
  where
    runCave :: Int -> Int -> Cave -> Int
    runCave 0 flashes _ = flashes
    runCave steps flashes c = runCave (steps - 1) (flashes + flashes') c'
      where (c', flashes') = step c

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
