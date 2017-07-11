-- Manoel Vilela @ HackerRank at 11/07/2017 01:47:00
import Control.Applicative
import Control.Monad
import System.IO
import Data.List (transpose)


mainDiag :: [[a]] -> [a]
mainDiag x = zipWith (!!) x [0..]

secDiag :: [[c]] -> [c]
secDiag x = mainDiag $ transpose $ reverse  x


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getMultipleLines n
    let a = map ( map ( read :: String -> Int ) . words ) a_temp
    putStrLn $ show $ abs $ foldr (-) 0 $ map sum [mainDiag a, secDiag a]



getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
