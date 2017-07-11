-- BirthdayCakeCandles HackerRank Problem
-- Solved on 11/07/2017 03:55:57
-- Manoel Vilela

count :: Integer -> [Integer] -> Int
count _ [] = 0
count k (x:xs)
  | k == x = 1 + count k xs
  | otherwise = count k xs


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Integer
    ar_temp <- getLine
    let ar = map read $ words ar_temp :: [Integer]
    print $ count (maximum ar) ar
