import Test.QuickCheck
-- Part 1
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)
-- It uses k + 1 "steps"
--
--
-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product [ n | x <- [1..k]]

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n * n) (div k 2)
           | otherwise = n * power2 n (k - 1)

-- Part 4
{- A.
 - The n's we'll use are [-3..3] to check both positive and negative integers
 - as well as zero.
 - The k's we'll use are:
 - Zero is good to start with to test the base-case.
 - One, two and three are good test-cases for small numbers (even and odd)
 - Two big numbers, one odd and one even (e.g. 37953 and 32768)
-}
testCases = [ (n,k) | n <- [(-3)..3], k <- [0, 1, 2, 3, 37953, 32768] ]

-- B.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k && power1 n k == power2 n k

-- C.
checkPowers :: Bool
checkPowers = and [ prop_powers n k | (n,k) <- testCases ] 

-- D.
-- It failed for negative k's :(
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)
-- It now works :)

