import Prelude hiding ((++), null, reverse, take, drop, splitAt, zip, unzip)
import qualified Prelude
import Test.QuickCheck

-- Welcome to dis liicture
-- Sequence, Choice and repetition are different ways to structure data types

{-
data Suit = asd | afsad | asd -- Choice
data Rank = Numeri Int | Jack | Queen
data Card = Card Rank Suit
data Hand = Empty | Add Card Hand -- Recursive Datatype
-}
-- Lists

-- i    Datatype of elements must be the same
-- ii   Order matters

{- One type of list definition
data List a = Nil | Cons a (List a)
data [a] = [] | a: [a]
-}

-- 5 : ( 6 : (3 : [])) == 5 : 6 : 3 : [] == [5,6,3]

-- "type" keyword means another name for an existing type, for example String is [Char]
-- Functions for lists: length, (++), concat, take, zip, map, filter

-- Overloaded: sum, product, elem (means find), sort
-- "=>" is type specific while "->" is generic

null :: [a] -> Bool
null []     = True
null _      = False
--null (_:_)  = False

(++) :: [a] -> [a] -> [a]
[]      ++ ys = ys
(x:xs)  ++ ys = x : (xs ++ ys)

cons x xs = x:xs

snoc :: [a] -> a -> [a]
-- snoc xs x = xs:x -- Typer Error
snoc xs x = xs ++ [x]

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

take :: Int -> [a] -> [a]
take n _ | n <= 0  = []
take _ []           = []
take n (x:xs)       = x:take (n-1) xs

-- n >= 0 ==> sets precondition
prop_take n xs = n >= 0 ==> length (take n xs) == min n (length xs)

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

--prop_take_drop :: Int -> [Int] -> Bool
--prop_take_drop n xs = take n xs ++ drop n xs == xs
prop_take_drop :: Int -> [Int] -> Property
prop_take_drop n xs = 
                    classify(n<=0 || n>length xs) "extreme" 
                    (take n xs ++ drop n xs == xs)

-- :set -XNoExtendedDefaultRules
nonprop_take_drop :: Int -> [Int] -> Bool
nonprop_take_drop n xs = drop n xs ++ take n xs == xs


--splitAt :: Int -> [a] -> ([a], [a])


zip :: [a] -> [b] -> [(a,b)]
{-zip []      []       = []
zip (x:xs)  []       = []
zip []      (y:ys    = []
zip (x:xs)  (y:ys)   = (x,y) : zip xs ys-}
zip (x:xs)  (y:ys)   = (x,y) : zip xs ys
zip _       _        = []

unzip :: [(a,b)] -> ([a],[b])
-- unzip xys = ([x | (x,y) <- xys], [y | (x,y) <- xys])
unzip [] = ([],[])
unzip ((x,y) : xys) = (x:xs,y:ys)
    where (xs,ys) = unzip xys

prop_zip_unzip :: [(Bool, Int)] -> Bool
prop_zip_unzip xys = zip xs ys == xys
    where (xs,ys) = unzip xys

prop_unzip_zip :: [Bool] -> [Int] -> Bool
prop_unzip_zip xs ys = unzip (zip xs ys) == (take n xs, take n ys)
    where n = min (length xs) (length ys)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
    where
        smaller = [x' | x' <- xs, x' <= x]
        bigger = [x' | x' <- xs, x' > x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (x':xs) | x <= x' = x:x' : xs
                 | otherwise = x' : insert x xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

prop_qsort :: [Int] -> Bool
prop_qsort xs = qsort xs == isort xs

isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)


prop_insert :: Int -> [Int] -> Property -- Because of ==>
prop_insert x xs = isSorted xs ==> isSorted (insert x xs)