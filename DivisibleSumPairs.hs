-- Divisible Sum Pairs @ HackerRank
-- Date: 11/07/2017 05:59:28
-- Manoel Vilela

sumdiv :: Int -> [Int] -> Int
sumdiv k [] = 0
sumdiv k (x:xs) = (sumdiv k xs) + length divs
  where divs = filter (\y -> (x + y) `mod` k  == 0) xs

main :: IO()
main = do n_temp <- getLine
          let n_t = words n_temp
          let k = read $ n_t!!1 :: Int
          ar_temp <- getLine
          let ar = map read $ words ar_temp :: [Int]
          print $ sumdiv k ar
