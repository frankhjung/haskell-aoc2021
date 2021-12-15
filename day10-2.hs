{-
== Day 10: Part 2 - Syntax Scoring

- chunks opened with (, it must close with )
- chunks opened with [, it must close with ]
- chunks opened with {, it must close with }
- chunks opened with <, it must close with >

=== Points

- ): 1 point
- ]: 2 points
- }: 3 points
- >: 4 points

1. Start with a total score of 0.
2. For each character, multiply the total score by 5
3. Then increase the total score by the point for the character

=== Example

[({(<(())[]>[[{[]{<()<>>  Complete by adding }}]])})]
[(()[<>])]({[<{<<[]>>(    Complete by adding )}>]})
(((({<>}<{<{<>}{[]{[]{}   Complete by adding }}>}>))))
{<[[]]>}<{[{[{[]{()[[[]   Complete by adding ]]}}]}]}>
<{([{{}}[<[[[<>{}]]]>[]]  Complete by adding ])}>

So, the last completion string above - ])}> - would be scored as follows:

Start with score of 0.
Multiply score by 5 to get 0,   then add the value of ] (2) to get 2
Multiply score by 5 to get 10,  then add the value of ) (1) to get 11
Multiply score by 5 to get 55,  then add the value of } (3) to get 58
Multiply score by 5 to get 290, then add the value of > (4) to get 294.

The example completion strings have total scores as follows:

}}]])})]  - 288957  total points.
)}>]})    - 5566    total points.
}}>}>)))) - 1480781 total points.
]]}}]}]}> - 995444  total points.
])}>      - 294     total points.

The result is middle score after sorting all of the scores. In this example,
the middle score is 288957, because there are the same number of scores smaller
and larger than it.

=== Explore

λ> scores = [288957,5566,1480781,995444,294]
λ> (sort scores) !! (div (length scores)  2)
gives: 288957

== Answers

make check day10-1

cat day10.test | ./day10-2
288957

cat day10.data | ./day10-2
3490802734

-}

import           Control.Monad (liftM2)
import           Data.List     (sort)

-- process each line reporting all syntax errors, sum points for syntax errors
solve :: [String] -> Int
solve = middle . map (score . closeCode . processCode) . filter (not . isCorrupted)

-- get middle score from list of scores (pointfree version)
-- middle scores = sort scores !! div (length scores) 2
middle :: [Int] -> Int
middle = liftM2 (!!) sort (flip div 2 . length)

-- check if line is corrupted
isCorrupted :: String -> Bool
isCorrupted = go []
  where
    go :: String -> String -> Bool
    go _ [] = False
    go processed (c:cs)
      | c `elem` right && null processed           = True
      | c `elem` right && open c /= head processed = True
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

-- Given a list of left and right braces, produce a reversed list of closing braces.
closeCode :: String -> String
closeCode = map close

-- get opening closing brace
close :: Char -> Char
close c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'
  _   -> error (c:" invalid char")

-- process current line looking for invalid syntax
processCode :: String -> String
processCode = go []
  where
    -- worker process current line looking for invalid syntax
    --    processed -> code -> unfinished code
    go :: String -> String -> String
    go code [] = code
    go code (c:cs)
      | c `elem` ")]}>" && open c == head code = go (tail code) cs
      | otherwise                              = go (c:code) cs

-- score resulting code
score :: String -> Int
score = foldl score' 0
  where
    score' :: Int -> Char -> Int
    score' acc c = (5 * acc) + points c

-- convert syntax code to points
points :: Char -> Int
points c
  | c == ')' = 1
  | c == ']' = 2
  | c == '}' = 3
  | c == '>' = 4
  | otherwise = 0

main :: IO ()
main = interact $ show . solve . lines
