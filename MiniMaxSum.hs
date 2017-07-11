-- MiniMaxSum Problem on HackerRank Algorithms
-- Solved on 11/07/2017 03:26:42

import Data.List (intercalate)

-- delete element by index
deleteN ::  Int -> [a] -> [a]
deleteN _ [] = []
deleteN k (x:xs)
  | k == 0 = xs
  | otherwise = x : deleteN (k-1) xs

-- generate possible sums of n elements for n-1
sums xs = map (sum . del xs) [0..n - 1]
  where del xs k = deleteN k xs
        n = length xs

main :: IO ()
main = do
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]
    let s = sums arr
    putStrLn $ intercalate " " $ map show $ [f s | f <- [minimum, maximum]]
