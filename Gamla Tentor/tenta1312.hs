import Test.QuickCheck

import Data.List

rainfall :: Int -> Float
rainfall n = undefined

maxRainfall :: Int -> Float
maxRainfall n = foldr maxrain 0 [1..n]
    where maxrain i f = max (rainfall i) f

maxRainfall' :: Int -> Float
maxRainfall' n = maxR (n-1) (rainfall n)
    where maxR n f | n <= 0 = f
                   | otherwise = max (rainfall n) (maxR (n-1) f)

maxWeeks :: Int -> [Int]
maxWeeks n = [x | x <- [1..n], rainfall x == maximum [rainfall j | j <- [1..n]]]

-- fa :: (a->Bool) -> (a->Bool) -> a -> Bool
-- fb :: Num a => a -> Maybe a
-- fc :: (Ord a, Num b) => [a] -> b
-- fd :: Eq a => [[a] -> [a] -> Bool
-- fe :: Monad m => m b -> [b] -> m [b]

fg a tb tc td =
    do x1 <- lookup a tb
       x2 <- lookup x1 tc
       x3 <- lookup x2 td
       Just [x1,x2,x3]

data Bag a = EmptyBag | Node a Int (Bag a) (Bag a)

bcount :: Ord a => a -> Bag a -> Int
bcount _ EmptyBag = 0
bcount a (Node x n b1 b2) | a == x = n
                          | a < x = bcount a b1
                          | a > x = bcount a b2

bagToList :: B
ag a -> [a]
bagToList EmptyBag = []
bagToList (Node x n b1 b2) = bagToList b1 ++ replicate n x ++ bagToList b2

prop_Bag :: Ord a => Bag a -> Bool
prop_Bag b = list == sort list
    where list = bagToList b
          
