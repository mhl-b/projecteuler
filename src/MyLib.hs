{-# LANGUAGE TypeApplications #-}

module MyLib where

-- problemZero

perfectSquares :: Int -> [Int]
perfectSquares n = [x * x | x <- [1 .. n]]

problemZero :: Int -> Int
problemZero n = sum [x | x <- perfectSquares n, odd x]

-- Multiples of 3 or 5 https://projecteuler.net/problem=1

multiples3or5Brute :: Int -> Int
multiples3or5Brute n = sum [x | x <- [1 .. n - 1], x `mod` 3 == 0 || x `mod` 5 == 0]

arithmeticSeriesAn :: Int -> Int -> Int -> Int
arithmeticSeriesAn n a1 an = n * (an - a1) `div` 2

arithmeticSeriesD :: Int -> Int -> Int -> Int
arithmeticSeriesD n a1 d = n * (2 * a1 + (n - 1) * d) `div` 2

arithmeticSeriesD0 :: Int -> Int -> Int
arithmeticSeriesD0 n d = arithmeticSeriesD n d d

multiples3or5Series :: Int -> Int
multiples3or5Series n = arithmeticSeriesD0 (n' `div` 3) 3 + arithmeticSeriesD0 (n' `div` 5) 5 - arithmeticSeriesD0 (n' `div` 15) 15
    where
        n' = n - 1

-- Even Fibonacci Numbers https://projecteuler.net/problem=2

fibSeq :: [Int]
fibSeq = [y | (_, y) <- iterate (\(x', y') -> (y', x' + y')) (1, 1)]

evenFibSeq :: [Int]
evenFibSeq = [y | (_, y) <- iterate (\(x', y') -> (y', y' * 4 + x')) (0, 2)]

evenFibNumbers :: Int -> Int
evenFibNumbers n = sum (takeWhile (< n) evenFibSeq)

-- Largest Prime Factor https://projecteuler.net/problem=3

intSqrtUp :: Integer -> Integer
intSqrtUp = (ceiling @Double) . sqrt . fromInteger

isFactor :: Integer -> Integer -> Bool
isFactor n f = n `mod` f == 0

isPrimeTrialDivision :: Integer -> Bool
isPrimeTrialDivision n = not (any (isFactor n) (2 : [3, 5 .. (intSqrtUp n)]))

primes :: [Integer]
primes = 2 : [x | x <- [3, 5 ..], isPrimeTrialDivision x]

-- returns power of factor and reminder of N after applying factor power
factorPower :: Integer -> Integer -> (Integer, Integer)
factorPower n f
        | isFactor n f = (1 + p', n')
        | otherwise = (0, n)
    where
        (p', n') = factorPower (n `div` f) f

canonicalFactorsForm :: [Integer] -> Integer -> [(Integer, Integer)]
canonicalFactorsForm [] _ = []
canonicalFactorsForm (f : fs) n
        | n == 1 = []
        | otherwise =
                if p' == 0
                        then canonicalFactorsForm fs n'
                        else (f, p') : canonicalFactorsForm fs n'
    where
        (p', n') = factorPower n f

primeCanonicalFactorsForm :: Integer -> [(Integer, Integer)]
primeCanonicalFactorsForm = canonicalFactorsForm primes

-- Largest Palindrome Product https://projecteuler.net/problem=4
