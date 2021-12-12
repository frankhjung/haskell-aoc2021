{-
== Day 08 : Part 1 - Seven Segment Search

Digits:

d  code      len comment
------------------------
0: abcefg    6   5: length = 6 & intersect 5 & length = 2
1: cf        2   length = 2
2: acdeg     5   ?: length = 5 & intersect 4 & length = 3
3: acdfg     5   ?: length = 5 & intersect 1 & length = 3
4: bcdf      4   length = 4
5: abdfg     5   2: length = 5 & intersect 2 & length = 2
6: abdefg    6   ?: length = 6 & intersect 7 & length = 4
7: acf       3   length = 3
8: abcdefg   7   length = 7
9: abcdfg    6   ?: length = 6 & intersect 4 & length = 2

Unmangle the scrambled segments to get the original number.

=== Explore

ds = M.fromList [('0',""), ('1',""), ('2',""), ('3',""), ('4',""), ('5',""), ('6',""), ('7',""), ('8',""), ('9',"")]

λ> M.insert '1' "cf" ds
fromList [('0',""),('1',"cf"),('2',""),('3',""),('4',""),('5',""),('6',""),('7',""),('8',""),('9',"")]

λ> M.insert '1' "cf" ds & M.insert '4' "bcdf"
fromList [('0',""),('1',"cf"),('2',""),('3',""),('4',"bcdf"),('5',""),('6',""),('7',""),('8',""),('9',"")]

=== Test

reference digits:
λ> ds = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

λ> f ds k n = map (\d -> d \\ ds !! k) (filter ((==n) . length) ds)
λ> g = f ds
λ> map g [1,4,7,8,9] -- using list of known digits

-- try to decipher 2, 3, 5
λ> filter ((==5) . length) ds
["acdeg","acdfg","abdfg"]

-- try to decipher 0, 6, 9
λ> filter ((==6) . length) ds
["abcefg","abdefg","abcdfg"]

-- this will get us 0
λ> map (\d -> d \\ ds !! 5) (filter ((==6) . length) ds)
λ> map (\d -> length (d \\ ds !! 5) == 2) (filter ((==6) . length) ds)

-- this will get us 1
λ> filter ((==2) . length) ds

-- this will get us 2
λ> map (\d -> d \\ ds !! 4) (filter ((==5) . length) ds)
λ> map (\d -> length (d \\ ds !! 4) == 3) (filter ((==5) . length) ds)

-- this will get us 3
λ> map (\d -> d \\ ds !! 1) (filter ((==5) . length) ds)
λ> map (\d -> length (d \\ ds !! 1) == 3) (filter ((==5) . length) ds)

-- this will get us 4
λ> filter ((==4) . length) ds

-- this will get us 5
λ> map (\d -> d \\ ds !! 2) (filter ((==5) . length) ds)
λ> map (\d -> length (d \\ ds !! 2) == 2) (filter ((==5) . length) ds)

-- this will get us 6
λ> map (\d -> d \\ ds !! 7) (filter ((==6) . length) ds)
λ> map (\d -> length (d \\ ds !! 7) == 4) (filter ((==6) . length) ds)

-- this will get us 7
λ> filter ((==3) . length) ds

-- this will get us 8
λ> filter ((==7) . length) ds

-- this will get us 9
λ> map (\d -> d \\ ds !! 4) (filter ((==6) . length) ds)
λ> map (\d -> length (d \\ ds !! 4) == 2) (filter ((==6) . length) ds)

=== Example data

λ> tr = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
λ> t = parse tr

λ> ts = fst . head $ parse t
λ> td = snd . last $ parse t

λ> ds = map sort $ sequence [zero, one, two, three, four, five, six, seven, eight, nine] ts
["abcdeg","ab","acdfg","abcdf","abef","bcdef","bcdefg","abd","abcdefg","abcdef"]

λ> map (\d -> (intToDigit . head) (elemIndices d ds)) (map sort td)
"5353"

=== Answers

  $ make check day08-1

  $ ./day08-2 day08.test
  61229

  $ ./day08-2 day08.data
  936117

 -}

import           Data.Char          (intToDigit)
import           Data.List          (elemIndices, sort, (\\))
import           Data.List.Split    (splitOn)
import           System.Environment (getArgs)

-- get digit code from a list of segments
getSegments :: Int -> [String] -> [String]
getSegments n = filter ((==n) . length)

-- get digit code for segment "zero"
-- length = 6 & intersect 1 & length = 4
zero :: [String] -> String
zero ds = head $ filter (\d -> length (d \\ five ds) == 2) (getSegments 6 ds)

-- '1' is the segment with length 2
one :: [String] -> String
one = head . getSegments 2

-- '2' is length = 5 & intersect 4 & length = 3
two :: [String] -> String
two ds = head $ filter (\d -> length (d \\ four ds) == 3) (getSegments 5 ds)

-- '3' is length = 5 & intersect 1 & length = 3
three :: [String] -> String
three ds = head $ filter (\d -> length (d \\ one ds) == 3) (getSegments 5 ds)

-- '4' is the segment with length 4
four :: [String] -> String
four = head . getSegments 4

-- '5' is length = 5 & intersect 2 & length = 2
five :: [String] -> String
five ds = head $ filter (\d -> length (d \\ two ds) == 2) (getSegments 5 ds)

-- '6' is length = 6 & intersect 7 & length = 4
six :: [String] -> String
six ds = head $ filter (\d -> length (d \\ seven ds) == 4) (getSegments 6 ds)

-- '7' is the segment with length 3
seven :: [String] -> String
seven = head . getSegments 3

-- '8' is the segment with length 7
eight :: [String] -> String
eight = head . getSegments 7

-- '9' is length = 6 & intersect 4 & length = 2
nine :: [String] -> String
nine ds = head $ filter (\d -> length (d \\ four ds) == 2) (getSegments 6 ds)

translate :: [String] -> [String] -> Int
translate segments display = dd
  where
    -- decode digits in segment
    ds = map sort $ sequence [zero, one, two, three, four, five, six, seven, eight, nine] segments
    -- decode digits in display
    dd = read $ map (intToDigit . head . flip elemIndices ds . sort) display :: Int

-- sum the display digits from the information in the mangled segments
solve :: String -> Int
solve = sum . map (uncurry translate) . parse

-- parse input
parse :: String -> [([String], [String])]
parse = fmap parseLine . lines

-- read signals and display seperated by delimiter
parseLine :: String -> ([String], [String])
parseLine l = (words signal, words display)
  where [signal, display] = splitOn " | " l

-- Get filename from command line argument.
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print $ solve contents
