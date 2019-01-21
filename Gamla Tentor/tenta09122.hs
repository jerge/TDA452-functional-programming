import Test.QuickCheck

sales :: Int -> Int
sales n = undefined

zeroWeeks1 :: Int -> Int
zeroWeeks1 n = length [x | x <- [0..n], sales x == 0]

zeroWeeks2 :: Int -> Int
zeroWeeks2 n = length (filter ((0 ==) . sales) [0..n])

zeroWeeks3 :: Int -> Int
zeroWeeks3 n = zw n
    where zw (-1) = 0
          zw n | sales n == 0 = 1 + zw (n-1)
               | otherwise = zw (n-1)


