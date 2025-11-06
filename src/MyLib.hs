{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeApplications #-}

module MyLib where

import Data.Function
import Data.Int
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.List
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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

arithmeticSeriesD :: Integer -> Integer -> Integer -> Integer
arithmeticSeriesD n a1 d = n * (2 * a1 + (n - 1) * d) `div` 2

arithmeticSeriesD0 :: Integer -> Integer -> Integer
arithmeticSeriesD0 n d = arithmeticSeriesD n d d

arithmeticSeries :: Integer -> Integer
arithmeticSeries n = arithmeticSeriesD0 n 1

multiples3or5Series :: Integer -> Integer
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

isPrimeTrialDivisionSieve :: [Integer] -> Integer -> Bool
isPrimeTrialDivisionSieve sieve n = not (any (isFactor n) (takeWhile (<= intSqrtUp n) sieve))

nextPrime :: [Integer] -> Integer -> Integer
nextPrime [] _ = 2
nextPrime ps lp = fromMaybe undefined (listToMaybe [x | x <- [lp + 1 ..], isPrimeTrialDivisionSieve ps x])

primes :: [Integer]
primes = go [] 0
  where
    go ps lp = p : go (ps ++ [p]) p
      where
        p = nextPrime ps lp

-- returns power of factor and reminder of N after applying factor power

type CanonicalFactor = (Integer, Integer)

type Factor = Integer

-- returns power of factor and reminder of N after applying factor^power
factorPower :: Integer -> Factor -> (Integer, Integer)
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

splitWith :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitWith p xs = case break p (dropWhile p xs) of
  ([], []) -> []
  (s', []) -> [s']
  ([], xs') -> [xs']
  (s', xs') -> s' : splitWith p xs'

problem8Digits :: [Integer]
problem8Digits = mapMaybe (readMaybe . (: [])) problem8Input

adjacentDigits :: Int -> [Integer] -> [[Integer]]
adjacentDigits w ds
  | length ds < w = []
  | otherwise = take w ds : adjacentDigits w (drop 1 ds)

largestProductInASeries :: Int -> Integer
largestProductInASeries w = maximum (map product (adjacentDigits w problem8Digits))

largestProductInASeries2 :: Int -> Integer
largestProductInASeries2 w = maximum (map product subSeries)
  where
    nonZeroSeries = splitWith (== 0) problem8Digits
    subSeries = concatMap (takeWhile (\ds -> length ds == w) . adjacentDigits w) nonZeroSeries

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

readIntsSpaceDilim :: String -> [Int]
readIntsSpaceDilim s = mapMaybe readMaybe (words s)

problem11Nums :: [Int]
problem11Nums = readIntsSpaceDilim problem11Input

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

-- Highly Divisible Triangular Number https://projecteuler.net/problem=12

triangleNumbers :: [Integer]
triangleNumbers = [arithmeticSeries x | x <- [1 ..]]

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

combination :: Integer -> Integer -> Integer
combination _ 0 = 0
combination n k = factorial n `div` (factorial k * factorial (n - k))

divisorsBrute :: Integer -> [Integer]
divisorsBrute n = [x | x <- [1 .. n], n `mod` x == 0]

numberOfDivisorsBrute :: Integer -> Integer
numberOfDivisorsBrute = fromIntegral . length . divisorsBrute

numberOfDivisorsFromCanonical :: [CanonicalFactor] -> Integer
numberOfDivisorsFromCanonical = product . map ((+ 1) . snd)

divisorsProduct :: [Integer] -> [Integer] -> [Integer]
divisorsProduct a b = a ++ b ++ [a' * b' | a' <- a, b' <- b]

factorDivisors :: CanonicalFactor -> [Integer]
factorDivisors (b, f) = [b ^ f' | f' <- [1 .. f]]

canonicalFromToDivisors :: [CanonicalFactor] -> [Integer]
canonicalFromToDivisors fs = 1 : foldr (divisorsProduct . factorDivisors) [] fs

divisorsCanonical :: Integer -> [Integer]
divisorsCanonical = canonicalFromToDivisors . primeCanonicalFactorsForm

numberOfDivisors :: Integer -> Integer
numberOfDivisors = numberOfDivisorsFromCanonical . primeCanonicalFactorsForm

highlyDivisbleTriangularNumber :: Integer -> Integer
highlyDivisbleTriangularNumber divisors =
  fromMaybe
    undefined
    (find (\n -> numberOfDivisors n > divisors) triangleNumbers)

-- Large Sum https://projecteuler.net/problem=13

problem13Input :: String
problem13Input =
  """
  37107287533902102798797998220837590246510135740250
  46376937677490009712648124896970078050417018260538
  74324986199524741059474233309513058123726617309629
  91942213363574161572522430563301811072406154908250
  23067588207539346171171980310421047513778063246676
  89261670696623633820136378418383684178734361726757
  28112879812849979408065481931592621691275889832738
  44274228917432520321923589422876796487670272189318
  47451445736001306439091167216856844588711603153276
  70386486105843025439939619828917593665686757934951
  62176457141856560629502157223196586755079324193331
  64906352462741904929101432445813822663347944758178
  92575867718337217661963751590579239728245598838407
  58203565325359399008402633568948830189458628227828
  80181199384826282014278194139940567587151170094390
  35398664372827112653829987240784473053190104293586
  86515506006295864861532075273371959191420517255829
  71693888707715466499115593487603532921714970056938
  54370070576826684624621495650076471787294438377604
  53282654108756828443191190634694037855217779295145
  36123272525000296071075082563815656710885258350721
  45876576172410976447339110607218265236877223636045
  17423706905851860660448207621209813287860733969412
  81142660418086830619328460811191061556940512689692
  51934325451728388641918047049293215058642563049483
  62467221648435076201727918039944693004732956340691
  15732444386908125794514089057706229429197107928209
  55037687525678773091862540744969844508330393682126
  18336384825330154686196124348767681297534375946515
  80386287592878490201521685554828717201219257766954
  78182833757993103614740356856449095527097864797581
  16726320100436897842553539920931837441497806860984
  48403098129077791799088218795327364475675590848030
  87086987551392711854517078544161852424320693150332
  59959406895756536782107074926966537676326235447210
  69793950679652694742597709739166693763042633987085
  41052684708299085211399427365734116182760315001271
  65378607361501080857009149939512557028198746004375
  35829035317434717326932123578154982629742552737307
  94953759765105305946966067683156574377167401875275
  88902802571733229619176668713819931811048770190271
  25267680276078003013678680992525463401061632866526
  36270218540497705585629946580636237993140746255962
  24074486908231174977792365466257246923322810917141
  91430288197103288597806669760892938638285025333403
  34413065578016127815921815005561868836468420090470
  23053081172816430487623791969842487255036638784583
  11487696932154902810424020138335124462181441773470
  63783299490636259666498587618221225225512486764533
  67720186971698544312419572409913959008952310058822
  95548255300263520781532296796249481641953868218774
  76085327132285723110424803456124867697064507995236
  37774242535411291684276865538926205024910326572967
  23701913275725675285653248258265463092207058596522
  29798860272258331913126375147341994889534765745501
  18495701454879288984856827726077713721403798879715
  38298203783031473527721580348144513491373226651381
  34829543829199918180278916522431027392251122869539
  40957953066405232632538044100059654939159879593635
  29746152185502371307642255121183693803580388584903
  41698116222072977186158236678424689157993532961922
  62467957194401269043877107275048102390895523597457
  23189706772547915061505504953922979530901129967519
  86188088225875314529584099251203829009407770775672
  11306739708304724483816533873502340845647058077308
  82959174767140363198008187129011875491310547126581
  97623331044818386269515456334926366572897563400500
  42846280183517070527831839425882145521227251250327
  55121603546981200581762165212827652751691296897789
  32238195734329339946437501907836945765883352399886
  75506164965184775180738168837861091527357929701337
  62177842752192623401942399639168044983993173312731
  32924185707147349566916674687634660915035914677504
  99518671430235219628894890102423325116913619626622
  73267460800591547471830798392868535206946944540724
  76841822524674417161514036427982273348055556214818
  97142617910342598647204516893989422179826088076852
  87783646182799346313767754307809363333018982642090
  10848802521674670883215120185883543223812876952786
  71329612474782464538636993009049310363619763878039
  62184073572399794223406235393808339651327408011116
  66627891981488087797941876876144230030984490851411
  60661826293682836764744779239180335110989069790714
  85786944089552990653640447425576083659976645795096
  66024396409905389607120198219976047599490197230297
  64913982680032973156037120041377903785566085089252
  16730939319872750275468906903707539413042652315011
  94809377245048795150954100921645863754710598436791
  78639167021187492431995700641917969777599028300699
  15368713711936614952811305876380278410754449733078
  40789923115535562561142322423255033685442488917353
  44889911501440648020369068063960672322193204149535
  41503128880339536053299340368006977710650566631954
  81234880673210146739058568557934581403627822703280
  82616570773948327592232845941706525094512325230608
  22918802058777319719839450180888072429661980811197
  77158542502016545090413245809786882778948721859617
  72107838435069186155435662884062257473692284509516
  20849603980134001723930671666823555245252804609722
  53503534226472524250874054075591789781264330331690
  """

type LargeNum = [Digit]

type Digit = Int8

readLargeNum :: String -> LargeNum
readLargeNum = reverse . mapMaybe (readMaybe . (: []))

readLargeNums :: String -> [LargeNum]
readLargeNums = map readLargeNum . words

showLargeNum :: LargeNum -> String
showLargeNum = concatMap show . reverse

problem13Nums :: [LargeNum]
problem13Nums = readLargeNums problem13Input

largeNumAdd :: LargeNum -> LargeNum -> LargeNum
largeNumAdd = addWithCarry 0
  where
    carryFowrard d carry ds1 ds2 = (d + carry) `mod` 10 : addWithCarry ((d + carry) `div` 10) ds1 ds2
    addWithCarry 0 a [] = a
    addWithCarry 0 [] b = b
    addWithCarry 1 [] [] = [1]
    addWithCarry 1 (d : ds) [] = carryFowrard d 1 ds []
    addWithCarry 1 [] (d : ds) = carryFowrard d 1 ds []
    addWithCarry r (d1 : ds1) (d2 : ds2) = carryFowrard (d1 + d2) r ds1 ds2
    addWithCarry _ _ _ = undefined

largeSum :: [LargeNum] -> String
largeSum = take 10 . showLargeNum . foldr largeNumAdd []

-- Longest Collatz Sequence https://projecteuler.net/problem=14

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = n * 3 + 1

collatzSeq :: Int -> [Int]
collatzSeq n = takeWhile (> 1) (iterate collatz n) ++ [1]

longestCollatzSeq :: Int -> (Int, [Int])
longestCollatzSeq lim = maximumBy (compare `on` fst) [(length s, s) | n <- [lim, lim - 1 .. 1], let s = collatzSeq n]

type CollatzMem = IntMap Int

updateCollatzMem :: CollatzMem -> Int -> CollatzMem
updateCollatzMem mem n =
  let cq = collatzSeq n
      vals = takeWhile (`IntMap.notMember` mem) cq
      len = length vals
      lastKnown = case drop len cq of
        [] -> 0
        (n' : _) -> mem IntMap.! n'
      off = len + lastKnown
   in case vals of
        [] -> mem
        xs -> IntMap.union mem (IntMap.fromList (zip xs [off, off - 1 .. 1]))

longestCollatzSeqMem :: Int -> Int
longestCollatzSeqMem lim = maximum (IntMap.elems (foldr (flip updateCollatzMem) IntMap.empty [1 .. lim]))
