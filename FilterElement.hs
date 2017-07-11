import Data.List(nub, intercalate)
import Control.Monad (replicateM)

-- TODO: Optimize filterElements, very slow.
-- not passing on all tests because I got due time

hasAtLeast :: Int -> Int -> [Int] -> Bool
hasAtLeast 0 x ys = True
hasAtLeast k x [] = False
hasAtLeast k x (y:ys) = if x == y then hasAtLeast (k - 1) x ys
                        else hasAtLeast k x ys

filterElements :: Int -> [Int] -> [Int]
filterElements k xs = filter (\x -> hasAtLeast k x xs) $ nub xs

wrap :: [Int] -> String
wrap [] = "-1"
wrap xs = intercalate " " $ map show xs


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
main = do lines <- getLine
          cases <- replicateM (read lines) readCase
          mapM_ putStrLn $ map executeCase cases
