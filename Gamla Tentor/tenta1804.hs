import Data.List
import Test.QuickCheck
import Prelude hiding (FilePath)
import Data.Maybe

retrieve :: [a] -> [a]
retrieve [] = []
retrieve (c:[]) = [c]
retrieve (b:_:ds) = b:retrieve ds

scramble :: [a] -> [a]
scramble [] = []
scramble (x:xs) = es ++ os
        where 
            es = retrieve (x:xs)
            os = retrieve (xs)

unscramble :: [a] -> [a]
unscramble xs = alternate fs ls
    where
        (fs,ls) = splitAt middle xs
        middle = length xs - ((length xs) `div` 2)

alternate :: [a] -> [a] -> [a]
alternate (f:fs) ls = f:(alternate ls fs)
alternate _ _ = []

iter :: Int -> (a->a) -> a -> a
iter 0 _ x = x
iter t f x = iter (t-1) f (f x)

prop_scrambling :: Eq a => [a] -> NonNegative Int -> Bool
prop_scrambling xs (NonNegative n) = iter n unscramble (iter n scramble xs) == xs

prop_scramble :: Eq a => Int -> [a] -> Bool
prop_scramble n xs = iter p unscramble (iter p scramble xs) == xs
    where p = abs n

rVowel :: Gen Char
rVowel = elements "aeiouy"

rConsonant :: Gen Char
rConsonant = elements (['a'..'z']\\"aeiouy")

randomPassword :: Gen String
randomPassword = do n <- choose (8,10)
                    let a = n `div` 2
                    let b = n-a
                    cons <- vectorOf b rConsonant
                    vow <- vectorOf a rVowel
                    return (alternate cons vow)

-- The argument is the name of the file to check. The result is a list of misspelled words
-- paired with the line number of the line they appeared on.
checkSpelling :: FilePath -> IO [(String, Int)]
checkSpelling path = do (split,wordCheck) <- loadDictionary
                        file <- readFile path
                        let words = split file
                        let wordN = zip words [1..]
                        let wrongs = filter (checkSpelling' wordCheck) wordN
                        return wrongs
                        
checkSpelling' :: (String -> Bool) -> (String, Int) -> Bool
checkSpelling' wordCheck (word,n) = not (wordCheck word) 

type FilePath = String

loadDictionary :: IO (String->[String],String->Bool)
loadDictionary = undefined

type Name = String

data Expr = Const Double | Var Name | Add Expr Expr | Mul Expr Expr
            deriving (Eq, Show)

valueOfExpr :: [(Name,Double)] -> Expr -> Double
valueOfExpr _ (Const d)                  = d
valueOfExpr dict (Var name) | isJust val = fromJust val
                            | otherwise  = error "Unknown variable"
        where val = lookup name dict
valueOfExpr dict (Add e1 e2)                = valueOfExpr dict e1 + valueOfExpr dict e2
valueOfExpr dict (Mul e1 e2)                = valueOfExpr dict e1 * valueOfExpr dict e2

e1, e2, e3 :: Expr
e1 = Add (Var "x") (Const 1)
e2 = Add (Var "y") (Mul (Const 2) (Var "x"))
e3 = Const 3

exprTest = [("y",3), ("z", 2), ("x",5)]

eqns :: [(Name, Expr)]
eqns = [("y",e1), ("z", e2), ("x",e3)]

--- Ehm, no