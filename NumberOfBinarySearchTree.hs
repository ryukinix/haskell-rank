-- Number of Binary Search Tree @ HackerRank
-- Date: Sun 16 Jul 2017 12:26:09 AM -03
-- Manoel Vilela

import Control.Monad (replicateM)

-- formula source: http://www.geeksforgeeks.org/wp-content/uploads/gate_cs20051.GIF
-- where t(0) = 1 and t(1) = 1

-- basic memoization for function with one parameter
-- O(n*2^n) => optimizing => O(n*log(n))
bstWays :: Int -> Integer
bstWays = (map ways [0..] !!)
  where ways 0 = 1
        ways 1 = 1
        ways n = sum [bstWays(i-1)*bstWays(n-i) | i <- [1..n]]

less :: Integer -> Integer
less n = n `mod` 100000007

main :: IO ()
main = do n <- readLn
          inputs <- replicateM n getLine
          let tests = map read inputs :: [Int]
          mapM_ (putStrLn) $ map (show . less . bstWays) tests
