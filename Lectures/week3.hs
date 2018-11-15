import Prelude hiding (map, filter,sum,product,concat,foldr,takeWhile,dropWhile,lines)
import Data.Char(isSpace)
import Data.List(sort, group)

evens = filter even [1..10]

squares = map square [1..10]
    where square x = x*x


haha = map (take 2) ["Hallo", "Haskell"]

--prefixes = map prefix [1..7]
 --   where prefix n = take n "Haskell"

--prefixes = map (flip take "Haskell") [1..7]
-- flip applies a function (arguments in the opposite order)
-- take 2 "Haskell" === flip take "Haskell" 2
prefixes = map (`take` "Haskell") [1..7]

m3 = map (*3) [1..10]

lt3 = filter (<3) [1..10]
-- Can choose order of the operators
gt3 = filter (3<) [1..10]

between = filter p [1..10]
    where p n = 3 < n && n < 6

-- Function composition (f ring g from math)
removeSpaces s = filter (not . isSpace) s

-- Anonymous function
squares' = map (\x->x*x) [1..10]

between' = filter (\n -> 3 < n && n < 6) [1..10]

{-
1. Int -> Int -> Int    takes 2 arguments
2. Int -> (Int -> Int)  take one argument and returns a function that takes one argument
3. (Int -> Int) -> Int
1 is the same as 2
-}
{- Curried functions
1. Int -> Int -> Int
2. (Int,Int) -> Int
You can use curry and uncurry to switch between the forms of 1 and 2
map (uncurry take) [(2, "Haskell"),(3,"Hallo")]
-}

-- ** How to define higher order functions map and filter
map :: (a -> b) -> [a] -> [b]
map f []    = []
map f (x:xs)= f x:map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) | p x       = x:filter p xs
                | otherwise = filter p xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x+sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x*sum xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

foldr :: ( a->b->b ) -> b -> [a] -> b
foldr op b []       = b
foldr op b (x:xs)   = x `op` foldr op b xs

sum'        xs = foldr (+) 0 xs
product'    xs = foldr (*) 1 xs
concat'     xss = foldr (++) [] xss
and'        xs = foldr (&&) True xs
else'        xs = foldr (||) False xs

weather = "June is warm\nJuly is warm\nJanuary is cold\n"

takeLine "" = ""
takeLine (c:cs) | c == '\n' = ""
                | otherwise = c:takeLine cs

takeWord "" = ""
takeWord (c:cs) | isSpace c = "" -- Space character  can be ' ', '\n', '.' etc.
                | otherwise = c:takeWord cs

-- Higher order function
takeWhile p [] = []
takeWhile p (c:cs)  | p c = c:takeWhile p cs
                    | otherwise = []

takeLine' s = takeWhile (/='\n') s

takeWord' s = takeWhile (not . isSpace) s

-- takeWhile (\c->'a'<=c && c<='f') "absdsadnnbkhajsdömawdlk"

dropWhile p [] = []
dropWhile p (x:xs)  | p x = dropWhile p xs
                    | otherwise = x:xs

--dropLine s = dropWhile (/='\n') s
--dropLine s = tail (dropWhile (/='\n') s)
dropLine s = drop 1 (dropWhile (/='\n') s)

lines :: String -> [String]
lines [] = []
lines s = takeLine s:lines (dropLine s)

segments :: (a->Bool) -> [a] -> [[a]]
segments p [] = []
segments p xs = takeWhile p xs: segments p (drop 1 (dropWhile p xs))

wordCounts :: String -> String
wordCounts = unlines
            . map (\(n,w) -> w++": "++show n)
            . reverse
            . sort
            . map (\ws -> (length ws, head ws))
            . group 
            . sort 
            . words