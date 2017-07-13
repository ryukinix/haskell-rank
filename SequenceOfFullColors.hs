-- Full Color Sequence @ HackerRank
-- Date: Thu 13 Jul 2017 06:30:35 PM -03
-- Manoel Vilela

module Main where

import Control.Monad (replicateM)

fullColor :: String -> Bool
fullColor string = fullColor' (0,0,0,0) string
  where fullColor' (r,g,y,b) [] = r == g && y == b && abs (r - g) <= 0 && abs (y - b) <= 0
        fullColor' (r,g,y,b) (char:xs)
          | abs (r - g) >  1 || abs (y - b) >  1 = False
          | otherwise = let color = case char of 'R' -> (r+1,g,y,b)
                                                 'G' -> (r,g+1,y,b)
                                                 'Y' -> (r,g,y+1,b)
                                                 'B' -> (r,g,y,b+1)
                                                 _ -> (r,g,y,b)
                            in fullColor' color xs


main :: IO()
main = do n <- readLn
          colors <- replicateM n getLine
          mapM_ print $ map fullColor colors
