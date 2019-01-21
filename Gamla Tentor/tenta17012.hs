import Test.QuickCheck
import Data.Char

findIndices :: (a->Bool) -> [a] -> [Int]
findIndices f xs = map fst (filter (f . snd) (zip [0..] xs))

prop_findIndices :: (a->Bool) -> [a] -> Bool
prop_findIndices f xs = and (map (f . (xs !!)) (findIndices f xs))

split :: [a] -> ([a],[a])
split (x1:x2:xs) = (x1:l,x2:r)
    where (l,r) = split xs
split xs = (xs,[])

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort l) (mergeSort r)
    where (l,r) = split xs

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)
{-
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Branch x t1 t2) = (Branch f x (fmap f t1) (fmap f t2)) 

doubleTree :: Num a => Tree a -> Tree a
doubleTree = fmap (*2)
-}
-- fa :: a -> (a,a)
-- fb :: (Ord a, Num a) => a -> a -> Bool
-- fc :: (a -> b -> c) -> (a,b) -> c
{-
vectorOf_i :: Int -> Gen a -> Gen [a]
vectorOf_i n g = sequence (replicate n g)

vectorOf_ii :: Int -> Gen a -> Gen [a]
vectorOf_ii 0 g = return ()
vectorOf_ii n g = do x <- g
                     xs <- vectorOf_ii (n-1) g
                     return (x:xs)
-}
--6

validCard :: Integer -> Bool
validCard n = (sum (doubleAndSub (intToInts n))) `mod` 10 == 0

intToInts :: Integer -> [Integer]
intToInts 0 = []
intToInts n = n `mod` 10: (intToInts (n `div` 10))

doubleAndSub :: [Integer] -> [Integer]
doubleAndSub (x1:x2:xs) | x2 > 4    = x1:x2*2-9:(doubleAndSub xs)
                        | otherwise = x1:x2*2:(doubleAndSub xs)
doubleAndSub xs = xs

