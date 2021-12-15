import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)
{-
== Day 10: Part 1 - Syntax Scoring

- chunks opened with (, it must close with )
- chunks opened with [, it must close with ]
- chunks opened with {, it must close with }
- chunks opened with <, it must close with >

=== Points

- ): 3 points
- ]: 57 points
- }: 1197 points
- >: 25137 points

Result is number of syntax error multiplied by points for each error.

=== Example

{([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead = 1197
[[<[([]))<([[{}[[()]]]   - Expected ], but found ) instead = 3
[{[{({}]{}}([{[{{{}}([]  - Expected ), but found ] instead = 57
[<(<(<(<{}))><([]([]()   - Expected >, but found ) instead = 3
<{([([[(<>()){}]>(<<{{   - Expected ], but found > instead = 25137

Sum of test synatx codes is 1197 + 3 + 57 + 3 + 25137 = 26397

== Answers

make check day10-1

cat day10.test | ./day10-1
26397

cat day10.data | ./day10-1
294195

-}

-- process each line reporting all syntax errors, sum points for syntax errors
solve :: [String] -> Int
solve = sum . map (processCode [])

-- process current line looking for invalid syntax
processCode :: String -> String -> Int
processCode _ [] = 0
processCode processed (c:cs)
  | c `elem` right && null processed       = points c
  | c `elem` right && lc /= head processed = points c
  | c `elem` right && lc == head processed = processCode (tail processed) cs
  | otherwise                              = processCode (c:processed) cs
  where
    left = "([{<"
    right = ")]}>"
    lc = left !! fromJust (elemIndex c right)

-- convert syntax code to points
points :: Char -> Int
points c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = 0

main :: IO ()
main = interact $ show . solve . lines
