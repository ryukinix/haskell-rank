-- Sum vs XOR @ HackerRank
-- Sun 20 Aug 2017 06:06:17 PM -03
-- Author: Manoel Vilela
import Data.Bits

solve :: Int -> Int
solve n' = solve' n' 0
  where
    solve' :: Int -> Int -> Int
    solve' n k
          | n == 0 = 2 ^ k
          | n .&. 1 == 0 = solve' (shiftR n 1) (k+1)
          | otherwise = solve' (shiftR n 1) k


main :: IO()
main = do x <- readLn
          print $ solve x
