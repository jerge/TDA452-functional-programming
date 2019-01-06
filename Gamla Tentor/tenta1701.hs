import Test.QuickCheck

findIndices :: (a->Bool) -> [a] -> [Int]
findIndices test xs = [i | (x,i) <- zip xs [0..], test x]

prop_findIndices :: (a->Bool) -> [a] -> Bool
prop_findIndices test xs = null (filter test (map (xs !!) (findIndices test xs)))

split :: [a] -> ([a],[a])
split xs | length xs == 0 = ([],[])
         | length xs == 1 = (xs,[])
         | otherwise      = (head xs:fst p,head (tail xs):snd p)
            where p = split (drop 2 xs)

split' :: [a] -> ([a],[a])
split' [] = ([],[])
split' (x:[]) = ([x],[])
split' (x1:x2:xs) = (x1:l,x2:r)
    where (l,r) = split' xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs1 [] = xs1
merge [] xs2 = xs2
merge (x1:xs1) (x2:xs2) | x1 < x2   = x1:(merge xs1 (x2:xs2))
                        | otherwise = x2:(merge (x1:xs1) xs2)

mergeSort :: Ord a => [a] -> [a]
mergeSort []        = []
mergeSort (x:[])    = [x]
mergeSort xs        = merge (mergeSort l) (mergeSort r)
    where (l,r) = split' xs

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Eq,Show)


instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Branch v l r) = Branch (f v) (fmap f l) (fmap f r) 

doubleTree :: Num a => Tree a -> Tree a
doubleTree t = fmap (*2) t

-- fa :: a -> (a,a)
-- fb :: Ord a, Num b => a -> b -> Bool
-- fc :: Nej
{-type Parser a = undefined

parse :: Parser a -> String -> Maybe (a,String)
parse = undefined-}

vectorOf_i,vectorOf_ii :: Int -> Gen a -> Gen [a]
vectorOf_i n g = sequence (replicate n g)

vectorOf_ii 0 g = return []
vectorOf_ii n g = do x <- g
                     xs <- vectorOf_ii (n-1) g
                     return (x:xs)

validCard :: Integer -> Bool
validCard x = doubleAddSub (intToDigits x) `mod` 10 == 0

intToDigits :: Integer -> [Integer]
intToDigits 0 = []
intToDigits x = x `mod` 10 : (intToDigits (x `div` 10))

doubleAddSub :: [Integer] -> Integer
doubleAddSub [] = 0
doubleAddSub (x1:[]) = x1
doubleAddSub (x1:x2:xs) | x2 > 4 = x1 + x2*2 - 9 + doubleAddSub xs
                        | otherwise = x1 + x2*2 + doubleAddSub xs