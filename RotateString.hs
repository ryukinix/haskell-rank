import Control.Monad(replicateM)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

iter :: Int -> [a] -> [[a]]
iter 0 _ = []
iter n m = rotate n m : iter (n-1) m

join :: [String] -> String
join [] = []
join xs = head xs ++ " " ++ join (tail xs)

main = do
  input <- getLine
  inputs <- replicateM (read input) getLine
  mapM_ (putStrLn) $ map (join . (\x -> (reverse $ iter (length x) x))) inputs
