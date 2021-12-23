{-# LANGUAGE TupleSections #-}
{-
== Day 14: Part 1 - Extended Polymerization

== Answer

$ cat day14.test | ./day14-1
1588

$ cat day14.data | ./day14-1
2435

-}
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Element = Char
type Polymer = [Element]
type Input = (Polymer, Rules)
type Rule = (Polymer, Element)
type Rules = M.Map Polymer Element

-- Parse the input:
--  1. part one is list of comma-separated numbers
--  2. part two is list of fold instructions
parse :: String -> Input
parse input = (template, rules)
  where
    [templateStr, rulesStr] = splitOn "\n\n" input  -- split input into two parts
    template = (head . lines) templateStr           -- polymer template
    rulesList = map parseRule $ lines rulesStr      -- polymer rules
    rules = M.fromList rulesList

-- Parse a rule from a string:
parseRule :: String -> Rule
parseRule str = (polymer, head element)
  where
    [polymer, element] = splitOn " -> " str

-- intercalate elements to polymer
--      rules    template   polymer
applyRules :: Rules -> Polymer -> Polymer
applyRules _ [] = []
applyRules _ [a] = [a]
applyRules rules (a:b:xs) =
  case M.lookup [a,b] rules of
    Just c  -> a : c : applyRules rules (b:xs)
    Nothing -> error "no rule for " ++ [a,b]

solve :: Input -> Int
solve (template, rules) = result (runApply 10 template)
  where
    -- apply rules to polymer n times
    runApply :: Int -> Polymer -> Polymer
    runApply 0 polymer = polymer
    runApply n polymer = runApply (n-1) (applyRules rules polymer)
    --returns difference of maximum vs minimum occurence of an element in polymer
    result :: Polymer -> Int
    result polymer = maximum counts - minimum counts
      where counts = M.elems (count polymer)

-- count the occurences of characters in a string
count :: String -> M.Map Char Int
count = M.fromListWith (+) . map (, 1)

-- Read from STDIN to solve the puzzle.
main :: IO ()
main = interact $ show . solve . parse
