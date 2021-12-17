{-# LANGUAGE DerivingStrategies #-}
{-
== Day 12: Part 1 - Passage Pathing

Problem:

- Find distinct paths from start to end
- Vist small caves only once
- Can vist big caves multiple times

=== Example

Test input:

  start-A
  start-b
  A-c
  A-b
  b-d
  A-end
  b-end

Paths (10):

  start,A,b,A,c,A,end
  start,A,b,A,end
  start,A,b,end
  start,A,c,A,b,A,end
  start,A,c,A,b,end
  start,A,c,A,end
  start,A,end
  start,b,A,c,A,end
  start,b,A,end
  start,b,end

== Answers

cat day12-1.test | ./day12-2
10

cat day12-2.test | ./day12-2
19

cat day12-3.test | ./day12-2
226

cat day12.data | ./day12-1
4378

-}
import           Control.Monad   (guard)
import           Data.Bool       (bool)
import           Data.Char       (isUpper)
import           Data.List.Split (splitOn)
import           Data.Maybe      (maybeToList)
import           Data.Set        as S (Set, insert, member, singleton)

-- Caves are the input labels for the cave system.
newtype Cave = Cave String deriving stock (Show, Eq, Ord)
-- Big caves we can revist, small caves we can't.
data Size = Big | Small deriving stock (Show, Eq)
-- Paths are the relationships between caves.
data Path = Path Cave Cave deriving Show

start, end :: Cave
start = Cave "start"
end   = Cave "end"

-- A cave is big if the label is all uppercase, and small if it is all lowercase.
size :: Cave -> Size
size (Cave c) = bool Small Big (all isUpper c)

nextPath :: Cave -> Path -> Maybe Cave
nextPath c (Path a b)
  | c == a    = Just b -- path from a -> b
  | c == b    = Just a -- path from b -> a
  | otherwise = Nothing

-- Get all routes going from start to end.
allRoutes :: [Path] -> [[Cave]]
allRoutes paths = filter ((==end) . last) $ (start:) <$> go (S.singleton start) start
  where
    -- Find all valid paths.
    --    visited   current   found paths
    go :: Set Cave -> Cave -> [[Cave]]
    go visited from = pure [] <> do
      p <- paths
      to <- maybeToList $ nextPath from p
      let valid = case size to of
            Big   -> size from == Small         -- don't cycle from Big to Big
            Small -> not $ S.member to visited  -- don't revisit a Small
      guard valid
      (to :) <$> go (S.insert to visited) to

-- sum the display digits from the information in the mangled segments
solve :: [Path] -> Int
solve = length . allRoutes

-- parse input
parse :: String -> [Path]
parse = fmap mkPath . lines

mkPath :: String -> Path
mkPath s = Path (Cave a) (Cave b)
  where [a, b] = splitOn "-" s

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
