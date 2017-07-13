-- The Sum of Powers @ HackerRank
-- Manoel Vilela
-- Date: Thu 13 Jul 2017 02:46:47 PM -03


-- algorithm O(2^n) -- awful
-- TODO: use memoization to get O(n) time
powers :: Int -> Int -> Int -> Int
powers x n cur = powers'
  where value = x - cur ^ n
        powers'
          | value == 0 = 1
          | value < 0 = 0
          | otherwise = powers value n (cur+1) + powers x n (cur+1)



main :: IO ()
main = do x <- readLn
          n <- readLn
          print (powers x n 1)
