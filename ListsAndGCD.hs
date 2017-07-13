import Data.List (splitAt, intercalate)
import Control.Monad (replicateM)

chunks :: Int -> [Int] -> [[Int]]
chunks n [] = []
chunks n xs = let (ys, zs) = splitAt n xs
                  in ys: chunks n zs

toTuple :: [Int] -> (Int, Int)
toTuple [a,b] = (a,b)

pairs :: [Int] -> [(Int,Int)]
pairs xs = map toTuple $ chunks 2 xs

getValue :: Int -> [(Int, Int)] -> Maybe Int
getValue _ [] = Nothing
getValue k ((x,y):xs)
  | k == x = Just y
  | otherwise = getValue k xs

gcd' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
gcd' [] _ = []
gcd' _ [] = []
gcd' ((prime,xexp):xs) ys = case getValue prime ys
                            of Just yexp -> (prime, min xexp yexp) : gcd' xs ys
                               Nothing -> gcd' xs ys

gcdList :: [[Int]] -> [(Int, Int)]
gcdList = (foldl1 gcd') . (map pairs)

primeList :: [(Int, Int)] -> [Int]
primeList [] = []
primeList ((a,b):xs) = a:b: primeList xs

main :: IO()
main = do q <- getLine
          inputs <- replicateM (read q :: Int) getLine
          let nums = map ((map read) . words) inputs :: [[Int]]
          putStrLn $ intercalate " " $ map show $ primeList $ gcdList nums
