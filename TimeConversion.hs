-- Time Conversion challenge on HackerRank
-- Solved on 11/07/2017 04:24:58
-- Manoel Vilela

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Text.Printf (printf)

time24 time@(h:rest) format
  | format == "AM" && h == 12 = 0 : rest
  | format == "PM" && h == 12 = time
  | format == "PM" = h + 12 : rest
  | format == "AM" = time

main :: IO ()
main = do s <- getLine
          let time = map (read :: String -> Int) $ splitOn ":" $ take 8 s
          let format = drop 8 s
          putStrLn $ intercalate ":" $ map (printf "%02d") $ time24 time format
