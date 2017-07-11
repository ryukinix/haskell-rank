-- Divisible Sum Pairs on HackerRank
-- ... Manoel Vilela 11/07/2017 05:59:28

import Data.List (tails)

sumdiv :: Int -> [Int] -> Int
sumdiv k [] = 0
sumdiv k (x:xs) = (sumdiv k xs) + length divs
  where divs = filter (\y -> (x + y) `mod` k  == 0) xs


main = do n_temp <- getLine
          let n_t = words n_temp
          let n = read $ n_t!!0 :: Int
          let k = read $ n_t!!1 :: Int
          ar_temp <- getLine
          let ar = map read $ words ar_temp :: [Int]
          print $ sumdiv k ar
