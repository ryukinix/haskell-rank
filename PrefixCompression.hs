-- PrefixCompression between two strings
-- Solved in Wed 12 Jul 2017 05:08:22 AM -03
-- Manoel Vilela

sharedPrefix :: String -> String -> String
sharedPrefix _ [] = []
sharedPrefix [] _ = []
sharedPrefix (x:xs) (y:ys)
  | x == y    = x:sharedPrefix xs ys
  | otherwise = []


removePrefix :: String -> String -> String
removePrefix [] s = s
removePrefix _ [] = []
removePrefix (p:ps) (s:ss)
  | p == s = removePrefix ps ss
  | otherwise = ss



prefixCompression :: String -> String -> [String]
prefixCompression r s = [showCompression x | let p = sharedPrefix r s
                                                 x' = removePrefix p r
                                                 y' = removePrefix p s,
                                                 x <- [p,x',y']]
  where showCompression x = (show $ length x) ++ " "++ x


main :: IO()
main = do x <- getLine
          y <- getLine
          mapM_ (putStrLn) $ prefixCompression x y
