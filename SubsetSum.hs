-- Subset Sum @ HackerRank
-- Date: Fri 14 Jul 2017 07:31:23 PM -03
-- Manoel Vilela

import Data.List (sortBy)
import Control.Monad (replicateM)

-- O(n)
-- TODO: O(log(n))
minimalSubSetSum :: [Int] -> Int -> Int
minimalSubSetSum _ (-1) = -1
minimalSubSetSum xs n = subSet n xs 0
  where subSet n' _ depth | n' <= 0 = depth
        subSet _ [] _ = -1
        subSet n' (x:xs') depth = let v = n' - x
                                  in subSet v xs' (depth + 1)

-- Just a helper function to filter nums whose
-- are greater than the whole set sum
bigNums :: Int -> Int -> Int
bigNums limit x
  | x > limit = -1
  | otherwise = x

-- TODO: anotate the max sum to skip the bad cases [DONE]
-- TODO: do binary search by preparing the data that way:
--       precompute all the pairs of max-sum depth (max-sum)
--       create a binary tree to do a search
main :: IO()
main = do _ <- getLine
          xs <- getLine
          n <- readLn
          tests <- replicateM n readLn
          let xs' = sortBy (flip compare) $ map read $ words xs :: [Int]
          let limit = sum xs'
          mapM_ print $ map (minimalSubSetSum xs' . (bigNums limit)) tests
