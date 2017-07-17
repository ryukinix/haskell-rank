-- Once in a Tram @ HackerRank Contest W34
-- Date: Mon 17 Jul 2017 09:59:22 AM -03
-- Manoel Vilela

import Data.Char (digitToInt)

sumDigits :: String -> Int
sumDigits [] = 0
sumDigits (x:xs) = digitToInt x + sumDigits xs

luckyNumber :: Int -> Bool
luckyNumber s = let s' = show s
                in sumDigits (take 3 s') == sumDigits (drop 3 s')

nextLucky :: Int -> Int
nextLucky x
  | luckyNumber (x+1) = x + 1
  | otherwise = nextLucky (x + 1)

main :: IO()
main = do n <- readLn
          print $ nextLucky n
