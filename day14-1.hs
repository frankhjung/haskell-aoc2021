{-
== Day 14: Part 1 - Extended Polymerization

== Answer

$ cat day14.test | ./day14-1
1588

$ cat day14.data | ./day14-1
2435

-}
import qualified Data.Foldable   as S
import           Data.List       (group, sort)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)

type Template = String
type Element = Char
type Polymer = [Element]
type Input = (Template, Rules)
type Rule = (Polymer, Element)
type Rules = M.Map String Element

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
--      rules    template   working    polymer
step :: Rules -> Polymer -> Polymer -> Polymer
step rs (p:ps) [] = step rs ps [p]
step _  []     ts = ts
step rs (p:ps) ts = step rs ps (ts <> [e,p])
  where
    e = if M.member [last ts,p] rs then fromJust $ M.lookup [last ts,p] rs else p

solve :: Input -> Int
solve (template, rules) = result (runApply 10 template)
  where
    -- apply rules to polymer n times
    runApply :: Int -> Template -> Template
    runApply 0 polymer = polymer
    runApply n polymer = runApply (n-1) polymer'
      where
        polymer' = S.toList $ step rules polymer []
    -- calculate result
    result :: Template -> Int
    result polymer = most - least
      where
        polymerLengths = (map length . group . sort) polymer
        most = maximum polymerLengths
        least = minimum polymerLengths

-- Read from STDIN to solve the puzzle.
main :: IO ()
main = interact $ show . solve . parse
