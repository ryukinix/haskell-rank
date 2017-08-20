-- Fibonacci Modified @ HackerRank
-- Date: Mon 17 Jul 2017 02:59:05 PM -03
-- Manoel Vilela

module Main where


modFib :: Int -> Int -> Int -> Integer
modFib f1 f2 = (map fib [0..] !! )
  where fib :: Int -> Integer
        fib 0 = 0
        fib 1 = toInteger f1
        fib 2 = toInteger f2
        fib n = modFib f1 f2 (n - 2) + (modFib f1 f2 (n - 1))^2

main :: IO()
main = do inputs <- getLine
          let [f1, f2, n] = map read $ words inputs ::  [Int]
          print $ modFib f1 f2 n
