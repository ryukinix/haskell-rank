-- Captain Prime @ HackerRank
-- Date: Thu 13 Jul 2017 08:02:21 PM -03
-- Manoel Vilela

module Main where

import Control.Monad (replicateM)

-- using algebraic datatypes just for fun, totally useless
data Person = DEAD | CENTRAL | LEFT | RIGHT

instance Show Person where
  show p = case p of  DEAD -> "DEAD"
                      CENTRAL -> "CENTRAL"
                      LEFT -> "LEFT"
                      RIGHT -> "RIGHT"

-- Get the Integer Square Root
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- Check if n is a prime number
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all id [ n `mod` k /= 0| k <- [2..isqrt n]]

-- check if the number contains a zero
containsZero :: Int -> Bool
containsZero n = '0' `elem` show n

-- Higher-order function to get a prime by shift left or right
-- f can be `take` (right) or `drop` (left)
directionPrime :: (Int -> String -> String) -> Int -> Bool
directionPrime f n = all id [isPrime k | i <- [1..l-1],
                                  let k = read (f i s) :: Int]
  where s = show n
        l = length s


-- check if all right numbers are primes too
rightPrime :: Int -> Bool
rightPrime n = directionPrime take n

-- check if all left numbers are primes too
leftPrime :: Int -> Bool
leftPrime n = directionPrime drop n

-- check if is both left and right prime
centralPrime :: Int -> Bool
centralPrime n = rightPrime n && leftPrime n


checkId :: Int -> Person
checkId n = state
  where zero = containsZero n
        prime = isPrime n
        left = leftPrime n
        right = rightPrime n
        state
          | zero || not prime = DEAD
          | left && right = CENTRAL
          | left = LEFT
          | right = RIGHT
          | otherwise = DEAD


main :: IO()
main = do n <- readLn
          nums <- replicateM n readLn
          mapM_ print $ map checkId nums
