import Control.Applicative
import Control.Monad
import System.IO
import Data.List (intercalate)


score :: Int -> Int -> [Int]
score x y
  | x == y = [0,0]
  | x > y = [1, 0]
  | x < y = [0, 1]


parseScores :: [Int] -> [Int] -> [Int]
parseScores xs ys = foldr sumPair [0,0] $ zipWith score xs ys
  where sumPair x y = zipWith (+) x y


main :: IO ()
main = do
    a0_temp <- getLine
    let a0_t = words a0_temp
    let a0 = read $ a0_t!!0 :: Int
    let a1 = read $ a0_t!!1 :: Int
    let a2 = read $ a0_t!!2 :: Int
    b0_temp <- getLine
    let b0_t = words b0_temp
    let b0 = read $ b0_t!!0 :: Int
    let b1 = read $ b0_t!!1 :: Int
    let b2 = read $ b0_t!!2 :: Int
    putStrLn $ intercalate " " $ map show $ parseScores [a0,a1,a2] [b0,b1,b2]
