import Test.QuickCheck
import Data.List
--1
--fa :: No Type
--fb :: [a] -> [a]
--fc :: Show a => [a] -> Int -> String
--fd :: [(Bool,a)] -> [(Bool,a)]

--2
scramble :: [a] -> [a]
scramble [] = []
scramble (x:[]) = [x]
scramble (x:xs) = even (x:xs) ++ even xs
    where
        even [] = []
        even (x:[]) = [x]
        even (x1:x2:xs) = x1 : even xs

{-scramble xs = map snd (filter (even . fst) boolList ++ filter (odd . fst) boolList)
        where boolList = zip [0..] xs
-}

unscramble :: [a] -> [a]
unscramble [] = []
unscramble xs = alternate even odd
    where
        (even,odd) = splitAt (length xs - length xs `div` 2) xs
        
alternate :: [a] -> [a] -> [a]
alternate (x:xs) ys = x:alternate ys xs
alternate _ _ = []

iter :: Int -> (a->a) -> a -> a
iter 0 _ x = x
iter n f x = iter (n-1) f (f x)

-- Change a if you want proper checks
prop_unscrambleScramble :: Eq a => Int -> [a] -> Property
prop_unscrambleScramble n xs = n >= 0 ==> iter n unscramble (iter n scramble xs) == xs

--3
rVowel, rConsonant :: Gen Char
rVowel = elements "aeiouy"
rConsonant = elements ((\\) ['a'..'z'] "aeiouy")

randomPassword :: Gen String
randomPassword = do n <- elements [8..10]
                    let half = n `div` 2
                    cons <- vectorOf (n-half) rConsonant
                    vow <- vectorOf (half) rVowel
                    return (alternate cons vow)

-- 4

loadDictionary :: IO (String->[String],String->Bool)
loadDictionary = undefined

checkSpelling :: FilePath -> IO [(String,Int)]
checkSpelling path = do (toWord,check) <- loadDictionary
                        file <- readFile path
                        let words = zip (toWord file) [0..]
                        return (filter (check . fst) words)

type Name = String

data Expr = Add Expr Expr | Mul Expr Expr | Var Name | Const Double
        deriving Show

valueOfExpr :: [(Name,Double)] -> Expr -> Double
valueOfExpr dic (Var n) | m == Nothing  = error "Unknown variable"
                        | otherwise     = Just m
                        where m = lookup n dic
valueOfExpr dic (Const x) = x
valueOfExpr dic (Add x1 x2) = valueOfExpr x1 + valueOfExpr x2
valueOfExpr dic (Mul x1 x2) = valueOfExpr x1 * valueOfExpr x2

valuesOfDefinitions :: [(Name,Expr)] -> [(Name,Double)]
valuesOfDefinitions xs = values
    where values = [(x,valueOfExpr values e) | (x,e) <- xs]