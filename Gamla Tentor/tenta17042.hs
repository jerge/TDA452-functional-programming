import Test.QuickCheck
import Data.List hiding (splitAt)
import Prelude hiding (splitAt)
import Data.Maybe


--1

data Tree a = Leaf a | Branch (Tree a) (Tree a)  deriving (Eq,Show)

t = Branch (Leaf 5) (Branch (Leaf 1) (Leaf 10)) -- an example tree

opTree ::(a -> a -> a) -> Tree a -> a
opTree _ (Leaf n) = n
opTree op (Branch t1 t2) = op (opTree op t1) (opTree op t2)

sumTree' :: Num a => Tree a -> a   -- Example: sumTree t == 16
sumTree' t = opTree (+) t

sumTree :: Num a => Tree a -> a   -- Example: sumTree t == 16
sumTree (Leaf n) = n
sumTree (Branch t1 t2) = sumTree t1 + sumTree t2

minTree' :: Ord a => Tree a -> a   -- Example: minTree t == 1
minTree' t = opTree (min) t

minTree :: Ord a => Tree a -> a   -- Example: minTree t == 1
minTree (Leaf n) = n
minTree (Branch t1 t2) = min (minTree t1) (minTree t2)

splitAt :: Int -> [a] -> ([a],[a])
splitAt _ []                 = ([],[])
splitAt n (x:xs) | n <= 0    = ([],(x:xs))
                 | otherwise = (x:l,r)
    where (l,r) = splitAt (n-1) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs | n >= length xs = [xs]
              | otherwise      = l : (chunksOf n r)
    where (l,r) = splitAt n xs

prop_lengthOfChunksOf :: Int -> [a] -> Property
prop_lengthOfChunksOf n xs = n > 0 ==> length (chunksOf n xs) == length xs+(n-1) `div` n

--3
--fa :: a -> [a]
--fb :: Num a =>  Bool -> a -> a
--fc :: ((a->b),(c->d)) -> (a,c) -> (b,d)

--4
{-
rHand :: Gen Hand
rHand = do size <- choose (1,6)
           cards <- vectorOf size rCard
           return (toHand (nub cards))
    where toHand :: [Card] -> Hand
          toHand [] = Empty
          toHand (x:xs) = Add x (toHand xs)

-}

--5
printFramed :: String -> IO ()
printFramed s = putStr (framed s)

framed :: String -> String
framed s = stars ++ "\n* " ++ s ++ " *\n" ++ stars
    where stars = replicate (length s + 4) '*'

type TagName = String

data XML = Text String | Elem TagName [XML] deriving (Eq,Show)

tableToXML :: [[String]] -> XML
tableToXML xss = Elem "table" (map tableRow xss)

tableRow :: [String] -> XML
tableRow xs = Elem "tr" (map tableData xs)

tableData :: String -> XML
tableData s = Elem "td" [Text s]

showXML :: XML -> String
showXML (Text s) = replace s
showXML (Elem tag xs) = "<" ++ tag ++ ">" ++ (concatMap showXML xs) ++ "</" ++ tag ++ ">"

replace :: String -> String
replace s = concatMap replaceChar s
    where replaceChar :: Char -> String
          replaceChar c | c == '<' = "&lt;"
                        | c == '&' = "&amp;"
                        | otherwise = [c]

renderTable :: Show a => [[a]] -> String
renderTable = showXML . tableToXML . map (map show)

