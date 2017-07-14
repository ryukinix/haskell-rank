-- Missing Numbers (FP)
-- Date: Fri 14 Jul 2017 12:34:22 AM -03
-- Manoel Vilela

module Main where

import Data.List (delete, intercalate, sort, nub)

-- removeList the elements of xs from ys and return
removeList :: (Eq a) => [a] -> [a] -> [a]
removeList [] ys = ys
removeList _ [] = []
removeList (x:xs) ys = removeList xs $ delete x ys

-- helper function to get each line of numbers
getList :: IO [Int]
getList = do _ <- getLine
             x <- getLine
             let x' = map read $ words x :: [Int]
             return x'

main :: IO()
main = do a <- getList
          b <- getList
          putStrLn $ intercalate " " $ map show $ nub $ sort $ removeList a b
