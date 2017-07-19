-- Maximum GCD and Sum @ HackerRank w34
-- Date: Tue 18 Jul 2017 05:55:45 AM -03
-- Manoel Vilela

-- Bad complexity: O(n^2)
-- Passing in only 7 tests of 22, about of 1/3
-- On rest of the tests: timed out or runtime error (memory probably)

import Data.List(maximumBy, sortBy)

pairSum :: (Int, Int) -> Int
pairSum (a,b) = a + b

maximumGcd :: [Int] -> [Int] -> (Int, Int)
maximumGcd = maximum' (1,1)
  where gcdPair (a,b) = gcd a b
        gcdCompare x y
          | gcdPair x > gcdPair y = GT
          | gcdPair x < gcdPair y = LT
          | pairSum x > pairSum y = GT
          | pairSum x < pairSum y = LT
          | otherwise = EQ

        maximum' gcd' _ [] = gcd'
        maximum' gcd' [] _ = gcd'
        maximum' gcd' (x:xs) ys@(y:_)
          | gcdPair gcd' >= x || gcdPair gcd' >= y = gcd'
          | otherwise  = let iterative = maximumBy gcdCompare
                                                   (gcd': [(x,k) | k <- ys])
                             recursion = maximum' iterative xs ys
                         in recursion

reverseSort :: (Ord a) => [a] -> [a]
reverseSort = sortBy (flip compare)


main :: IO()
main = do _ <- getLine
          xs' <- getLine
          ys' <- getLine
          let xs = map (read :: String -> Int) $ words xs'
          let ys = map (read :: String -> Int) $ words ys'
          print $ pairSum $ maximumGcd (reverseSort xs)  (reverseSort ys)
