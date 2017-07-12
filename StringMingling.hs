-- String Mingling on HackerRank
-- Solved on Wed 12 Jul 2017 04:42:39 AM -03
-- Manoel Vilela

mingling :: String -> String -> String
mingling rs [] = rs
mingling [] ss = ss
mingling (r:rs) (s:ss) = r:s:mingling rs ss


main :: IO()
main = do p <- getLine
          q <- getLine
          putStrLn $ mingling p q
