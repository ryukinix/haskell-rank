-- SuperDigit @ HackerRank
-- Date: Thu 13 Jul 2017 08:25:57 PM -03
-- Manoel Vilela

import Data.Char (digitToInt)

-- SumDigits of a string
sumDigits :: String -> Int
sumDigits [] = 0
sumDigits (x:xs) = digitToInt x + sumDigits xs

-- sum digits recursively
superDigit :: String -> Int
superDigit [x] = digitToInt x
superDigit xs = superDigit $ show $ sumDigits xs


main :: IO()
main = do line <- getLine
          let [n, k] = words line -- never concat n k times, just multiply!
          print $ superDigit $ show $ (read k :: Int) * (superDigit n)
