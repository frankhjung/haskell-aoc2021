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
36

cat day12-2.test | ./day12-2
103

cat day12-3.test | ./day12-2
3509

cat day12.data | ./day12-2
133621

-}
import           Control.Monad   (guard)
import           Data.Bool       (bool)
import           Data.Char       (isUpper)
import           Data.List.Split (splitOn)
import           Data.Maybe      (maybeToList)
import           Data.Set        as S (Set, empty, insert, member)

-- Caves are the input labels for the cave system.
newtype Cave = Cave String deriving stock (Show, Eq, Ord)
-- Big caves we can revist, small caves we can't.
data Size = Big | Small deriving stock (Show, Eq)
-- Paths are the relationships between caves.
data Path = Path Cave Cave deriving Show
-- Can revist one small cave excluding start and end.
data Revisit = REVISIT | NOREVISIT deriving stock (Show, Eq)

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
allRoutes :: Revisit -> [Path] -> [[Cave]]
allRoutes revist paths = filter ((==end) . last) $ (start:) <$> go S.empty start revist
  where
    -- Find all valid paths.
    --    visited   current   found paths
    go :: Set Cave -> Cave -> Revisit -> [[Cave]]
    go visited from revisit = pure [] <> do
      guard (from /= end)                       -- stop if we're at the end
      p <- paths                                -- read each path
      to <- maybeToList $ nextPath from p       -- get the next cave to visit
      guard (to /= start)                       -- don't go back to start
      let (valid, revisit') = case (size to, S.member to visited, revisit) of
            (Big, _, rv)       -> (size from == Small, rv) -- don't cycle from Big to Big
            (Small, False, rv) -> (True, rv)    -- not yet visited
            (Small, True, rv)  -> (rv == REVISIT, NOREVISIT) -- may be able to revisit small caves
      guard valid                               -- stop if no more valid paths
      (to :) <$> go (S.insert to visited) to revisit' -- continue with the next cave

-- sum the display digits from the information in the mangled segments
solve :: [Path] -> Int
solve = length . allRoutes REVISIT

-- parse input
parse :: String -> [Path]
parse = fmap mkPath . lines

mkPath :: String -> Path
mkPath s = Path (Cave a) (Cave b)
  where [a, b] = splitOn "-" s

-- use interact to read from stdin and solve
main :: IO ()
main = interact $ show . solve . parse
