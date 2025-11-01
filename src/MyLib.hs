{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeApplications #-}

module MyLib where

import Data.List
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

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

intSqrt :: Int -> Int
intSqrt = (ceiling @Float) . sqrt . fromIntegral

isFactor :: Integer -> Integer -> Bool
isFactor n f = n `mod` f == 0

isPrimeTrialDivision :: Integer -> Bool
isPrimeTrialDivision n = not (any (isFactor n) (2 : [3, 5 .. (intSqrtUp n)]))

primes :: [Integer]
primes = 2 : [x | x <- [3, 5 ..], isPrimeTrialDivision x]

-- returns power of factor and reminder of N after applying factor power

type CanonicalFactor = (Integer, Integer)

type Factor = Integer

factorPower :: Integer -> Factor -> CanonicalFactor
factorPower n f
  | isFactor n f = (1 + p', n')
  | otherwise = (0, n)
  where
    (p', n') = factorPower (n `div` f) f

canonicalFactorsForm :: [Factor] -> Integer -> [CanonicalFactor]
canonicalFactorsForm [] _ = []
canonicalFactorsForm (f : fs) n
  | n == 1 = []
  | otherwise =
      if p' == 0
        then canonicalFactorsForm fs n'
        else (f, p') : canonicalFactorsForm fs n'
  where
    (p', n') = factorPower n f

-- returns representation of a number as a list of prime factors and their power
primeCanonicalFactorsForm :: Integer -> [CanonicalFactor]
primeCanonicalFactorsForm = canonicalFactorsForm primes

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = fst (last (primeCanonicalFactorsForm n))

-- Largest Palindrome Product https://projecteuler.net/problem=4

int2Digits :: Int -> [Int]
int2Digits 0 = []
int2Digits n = (n `mod` 10) : int2Digits (n `div` 10)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == reverse a

isIntPalindrome :: Int -> Bool
isIntPalindrome n = isPalindrome (int2Digits n)

largestProduct :: (Int, Int) -> (Int, Int) -> Ordering
largestProduct (a, b) (a', b') = compare (a * b) (a' * b')

productPairs3 :: [(Int, Int)]
productPairs3 = sortBy (flip largestProduct) ([(a, b) | a <- [999, 998 .. 1], b <- [a, a - 1 .. 1]])

largestPalindromeProduct :: [(Int, Int)] -> (Int, Int)
largestPalindromeProduct [] = (0, 0)
largestPalindromeProduct (p : _) = p

largestPalindromeProduct3 :: (Int, Int)
largestPalindromeProduct3 = largestPalindromeProduct (filter (\(a, b) -> isIntPalindrome (a * b)) productPairs3)

-- Smallest Multiple https://projecteuler.net/problem=5

mergeCanonicalFactors :: [CanonicalFactor] -> [CanonicalFactor] -> [CanonicalFactor]
mergeCanonicalFactors [] [] = []
mergeCanonicalFactors [] fs = fs
mergeCanonicalFactors fs [] = fs
mergeCanonicalFactors (f : fs) (f' : fs') =
  case compare f1 f2 of
    LT -> f : mergeCanonicalFactors fs (f' : fs')
    GT -> f' : mergeCanonicalFactors (f : fs) fs'
    EQ -> (f1, max p1 p2) : mergeCanonicalFactors fs fs'
  where
    (f1, p1) = f
    (f2, p2) = f'

mergeCanonicalFactorsForRange :: [Integer] -> [CanonicalFactor]
mergeCanonicalFactorsForRange = foldr (mergeCanonicalFactors . primeCanonicalFactorsForm) []

canonicalFactorsToInteger :: [CanonicalFactor] -> Integer
canonicalFactorsToInteger [] = 1
canonicalFactorsToInteger fs = foldr (\(f, p) a -> a * f ^ p) 1 fs

smallestMultiple :: [Integer] -> Integer
smallestMultiple = canonicalFactorsToInteger . mergeCanonicalFactorsForRange

-- Sum Square Difference https://projecteuler.net/problem=6

sumSquareDifference :: Int -> Int
sumSquareDifference n = sum [2 * a * b | a <- [1 .. n - 1], b <- [a + 1 .. n]]

-- 10 001st Prime https://projecteuler.net/problem=7

prime10001 :: Integer
prime10001 = primes !! 10000

-- Largest Product in a Series https://projecteuler.net/problem=8

problem8Input :: String
problem8Input =
  """
  73167176531330624919225119674426574742355349194934
  96983520312774506326239578318016984801869478851843
  85861560789112949495459501737958331952853208805511
  12540698747158523863050715693290963295227443043557
  66896648950445244523161731856403098711121722383113
  62229893423380308135336276614282806444486645238749
  30358907296290491560440772390713810515859307960866
  70172427121883998797908792274921901699720888093776
  65727333001053367881220235421809751254540594752243
  52584907711670556013604839586446706324415722155397
  53697817977846174064955149290862569321978468622482
  83972241375657056057490261407972968652414535100474
  82166370484403199890008895243450658541227588666881
  16427171479924442928230863465674813919123162824586
  17866458359124566529476545682848912883142607690042
  24219022671055626321111109370544217506941658960408
  07198403850962455444362981230987879927244284909188
  84580156166097919133875499200524063689912560717606
  05886116467109405077541002256983155200055935729725
  71636269561882670428252483600823257530420752963450
  """

problem8Digits :: [Integer]
problem8Digits = mapMaybe (readMaybe . (: [])) problem8Input

adjacentDigits :: Int -> [Integer] -> [[Integer]]
adjacentDigits w ds
  | length ds < w = []
  | otherwise = take w ds : adjacentDigits w (drop 1 ds)

largestProductInASeries :: Int -> Integer
largestProductInASeries w = maximum (map product (adjacentDigits w problem8Digits))

-- Special Pythagorean Triplet https://projecteuler.net/problem=9

type Triplet = [Int]

isSquare :: Int -> Bool
isSquare n = sq * sq == n
  where
    sq = floor $ sqrt (fromIntegral n :: Double)

tripletSquare :: Int -> Int -> Triplet
tripletSquare a b = [a', b', a' + b']
  where
    a' = a ^ (2 :: Int)
    b' = b ^ (2 :: Int)

tripletSquares :: Int -> [Triplet]
tripletSquares n = [tripletSquare a b | a <- [1 .. n - 1], b <- [a + 1 .. n]]

pythTriplets :: Int -> [Triplet]
pythTriplets n = filter (\t -> isSquare (t !! 2)) (tripletSquares n)

specialPythTriplet :: Int -> [Triplet]
specialPythTriplet n = map (map intSqrt) (filter (\t -> sum (map intSqrt t) == n) (pythTriplets n'))
  where
    n' = n `div` 2

specialPythTripletProduct :: Int -> [Int]
specialPythTripletProduct n = map product (specialPythTriplet n)

-- Summation of Primes https://projecteuler.net/problem=10

summationOfPrimes :: Integer
summationOfPrimes = sum (takeWhile (< 2000000) primes)

-- Largest Product in a Grid https://projecteuler.net/problem=11

problem11Input :: String
problem11Input =
  """
  08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
  32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
  67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
  24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
  21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
  """

problem11Nums :: [Int]
problem11Nums = mapMaybe readMaybe (words problem11Input)

chunks :: Int -> [a] -> [[a]]
chunks w xs
  | length xs <= w = [xs]
  | otherwise = take w xs : chunks w (drop w xs)

type M20 = [[Int]]

type M4 = [[Int]]

grid20 :: M20
grid20 = chunks 20 problem11Nums

m4FromM20 :: Int -> Int -> M20 -> M4
m4FromM20 x y m20 = [[m20 !! y' !! x' | x' <- [x .. x + 3]] | y' <- [y .. y + 3]]

grid20Split :: [M4]
grid20Split = [m4FromM20 x y grid20 | x <- [0 .. 15], y <- [0 .. 15]]

m4Diags :: M4 -> [[Int]]
m4Diags m4 = [[m4 !! x !! x | x <- [0 .. 3]], [m4 !! x !! (3 - x) | x <- [0 .. 3]]]

m4Strips :: M4 -> [[Int]]
m4Strips m4 = m4 ++ transpose m4 ++ m4Diags m4

largestProductInGrid :: Int
largestProductInGrid = maximum (map product (concatMap m4Strips grid20Split))
