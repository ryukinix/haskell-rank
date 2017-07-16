-- Filter Element @ HackerRank
-- Date: Thu 11 Jul 2017
-- Manoel Vilela

import qualified Data.Map as Map
import Control.Monad (replicateM)
import Data.Maybe
import Data.List (sortBy, group, sort)

-- TODO: Optimize filterElements, very slow.
-- not passing on all tests because I got due time
-- Problem: nub is O(n^2)
hasAtLeast :: Int -> Int -> [Int] -> Bool
hasAtLeast 0 _ _ = True
hasAtLeast _ _ [] = False
hasAtLeast k x (y:ys) = if x == y then hasAtLeast (k - 1) x ys
                        else hasAtLeast k x ys


-- Lookup Map is just O(log(n))
initialPosition :: Map.Map Int Int -> Int -> Int -> Ordering
initialPosition m a b = lookup'
  where v1 = fromJust $ Map.lookup a m
        v2 = fromJust $ Map.lookup b m
        lookup'
          | v1 > v2 = GT
          | v1 < v2 = LT
          | otherwise = EQ


-- Insertion Map.fromListWithkey is O(n*log(n))
-- is someway, still better than use nub function whose is O(n²)
filterElements :: Int -> [Int] -> [Int]
filterElements k xs = let positions = Map.fromListWithKey (\_ _ v -> v) $ zip xs [0..]
                          sorted = map (\l@(x:_) -> (x,length l))$ group $ sort xs
                      in sortBy (initialPosition positions) $ map fst $ filter (\(x, s) -> s >= k) $ sorted

wrap :: [Int] -> String
wrap [] = "-1"
wrap xs = unwords $ map show xs


readNumbers :: String -> [Int]
readNumbers = map read . words

getKey :: String -> Int
getKey xs = read $ (!! 1) $ words xs

readCase :: IO (Int, [Int])
readCase = do keys <- getLine
              numbers <- getLine
              return (getKey keys, readNumbers numbers)

executeCase :: (Int, [Int]) -> String
executeCase (k, xs) = wrap $ filterElements k xs

main :: IO()
main = do n <- readLn
          cases <- replicateM n readCase
          mapM_ putStrLn $ map executeCase cases
