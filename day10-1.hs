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
solve = sum . processCode

processCode :: [String] -> [Int]
processCode = map (go [])
  where
    -- process current line looking for invalid syntax
    -- processed    code      score
    go :: String -> String -> Int
    go _ [] = 0
    go processed (c:cs)
      | c `elem` right && null processed           = points c
      | c `elem` right && open c /= head processed = points c
      | c `elem` right && open c == head processed = go (tail processed) cs
      | otherwise                                  = go (c:processed) cs
      where
        right = ")]}>"

-- get matching opening left brace
open :: Char -> Char
open c = case c of
  ')' -> '('
  ']' -> '['
  '}' -> '{'
  '>' -> '<'
  _   -> error (c:" invalid char")

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
