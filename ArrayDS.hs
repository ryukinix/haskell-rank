-- Mon 08 Jun 2020 08:56:18 AM -03
-- source: https://www.hackerrank.com/challenges/arrays-ds/problem

solution :: String -> String
solution = unwords . map show . reverse . parseInts . words . head . drop 1 . lines
    where parseInts :: [String] -> [Int]
          parseInts = map read


main :: IO()
main = interact solution
