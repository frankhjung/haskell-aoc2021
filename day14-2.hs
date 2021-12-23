{-# LANGUAGE DerivingStrategies #-}
{-
== Day 14: Part 2 - Extended Polymerization

Growth is exponential, so you want to manage a map of polymer pairs and their
occurrences.

== Answer for 10 steps

$ cat day14.test | ./day14-1
1588

$ cat day14.data | ./day14-1
2435

== Answer for 40 steps

$ cat day14.test | ./day14-2
(1588,2188189693529)

$ cat day14.data | ./day14-2
(2435,2587447599164)

Copied from https://github.com/amalloy/aoc-2021/blob/main/day14/src/Main.hs
though I changed how the input was being parsed so I required fewer external
packages.

-}
import           Control.Arrow   ((&&&))
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

-- A polymer is a sequence of elements.
newtype Element = Element Char deriving stock (Eq, Ord, Show)
-- A pair of elements gives us a new element to insert into the polymer.
data Pair a = Pair a a deriving stock (Eq, Ord, Show)
-- The rules for polymerization: given a pair return the new element to insert.
data Rule a = Rule (Pair a) a deriving (Show)

-- The map of polymers.
type Polymer a = M.Map (Pair a) Integer
-- The parsed input read.
data Input = Input { _elements :: Polymer Element,
                     _rules    :: Pair Element -> Element,
                     _first    :: Element }

-- Parse the input:
--  1. part one is list of comma-separated numbers
--  2. part two is list of fold instructions
parse :: String -> Input
parse input = Input (parseTemplate templateStr) (parseRules rulesStr) first
  where
    [templateStr, rulesStr] = splitOn "\n\n" input  -- split input into two parts
    first = Element (head templateStr)              -- first element of the polymer
                                                    -- needed to correctly count occurrences

parseTemplate :: String -> Polymer Element
parseTemplate str = M.fromListWith (+) $ zipWith polymer str' (tail str')
  where
    str' = head . lines $ str                       -- template polymer on first line
    polymer a b = (Pair (Element a) (Element b), 1) -- polymer is a pair of elements


-- Parse rules from a string to a function that takes an element pair
-- and returns an element.
parseRules :: String -> (Pair Element -> Element)
parseRules str = (M.fromList [ (p, e) | (Rule p e) <- rules ] M.!)
  where
    rules = map parseRule $ lines str
    parseRule :: String -> Rule Element
    parseRule line = Rule (Pair (Element a) (Element b)) (Element c)
      where
        [a:b:_, c:_] = splitOn " -> " line          -- polymer -> new element


-- Run a polymerization step on a polymer.
step :: Input -> Input
step (Input m r f) = Input m' r f
  where
    m' = M.fromListWith (+) $ do                    -- new map of polymers and counts
      (p@(Pair a b), n) <- M.toList m               -- old map of polymers and counts
      let e = r p                                   -- get new element
      [ (Pair a e, n), (Pair e b, n) ]              -- put new elements in between old ones

-- From the final polymer, find the difference in occurrences between the most and the least common elements.
score :: Input -> Integer
score (Input m _ f) = maximum elements - minimum elements
  where
    -- count the second element in the pair as the key and occcurrences as the value
    elements = M.elems (M.insertWith (+) f 1        -- need to count first element as well
      (M.fromListWith (+) $ do                      -- count occurences of each element
        (Pair _ b, n) <- M.toList m                 -- use only 2nd element of pair
        [ (b, n) ]))                                -- (element, count)

-- Solve puzzle for n steps.
solve :: Int -> Input -> Integer
solve n = score . (!! n) . iterate step

-- Part 1 - do for 10 steps
part1 :: Input -> Integer
part1 = solve 10

-- Part 2 - do for 40 steps
part2 :: Input -> Integer
part2 = solve 40

-- Read from STDIN to solve the puzzle.
main :: IO ()
main = interact $ show . (part1 &&& part2) . parse
