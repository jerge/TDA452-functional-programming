import Test.QuickCheck
import Data.List hiding (lookup)
import Data.Maybe
import System.Directory
import Prelude hiding (lookup)

--1 4/7
lookupAll :: Eq key => key -> [(key,value)] -> [value]
lookupAll key dic = [v | (k,v) <- dic, k == key]

lookup :: Eq key => key -> [(key,value)] -> Maybe value
lookup k dic = listToMaybe (lookupAll k dic)

update :: Eq key => key -> value -> [(key,value)] -> [(key,value)]
update k v dic = (k,v):[ (k',v') | (k',v') <- dic, k' /= k]

--2 5/8
data Expr  = X | Num Int | Op BinOp Expr Expr  deriving (Eq,Show)
data BinOp = Add | Mul | Subtract              deriving (Eq,Show)

ex1 = Op Subtract (Num 100) X                  -- 100 - X
ex2 = Op Add (Num 100) (Op Mul (Num (-1)) X)   -- 100 + (-1)*X

eval :: Expr -> Int -> Int
eval (Num n) _ = n
eval (Op Add ex1 ex2) x = eval ex1 x + eval ex2 x
eval (Op Subtract ex1 ex2) x = eval ex1 x - eval ex2 x
eval (Op Mul ex1 ex2) x = eval ex1 x * eval ex2 x
eval _ x = x

removeSub :: Expr -> Expr
removeSub (Op Subtract ex1 ex2) = Op Add (removeSub ex1) (Op Mul (Num (-1)) (removeSub ex2))
removeSub (Op bo ex1 ex2) = Op bo (removeSub ex1) (removeSub ex2)
removeSub ex1 = ex2


-- 3 6/8
-- fa :: Bool -> Bool -> Bool
-- fb :: (a -> b -> Bool) -> a -> b -> Bool
-- fc :: Fractional a => a -> a -> a
-- fd :: [[a]] -> [a]

-- 4

data Grid a = Grid [[a]] 
    deriving (Eq,Show)

g1,g2 :: Grid Int
g1 = Grid [[1,2],[3,4],[5,6]]
g2 = Grid [[5,3,1],[6,4,2]]

mapGrid :: (a->b) -> Grid a -> Grid b
mapGrid f (Grid g) = Grid (map (map f) g)

rotateGrid :: Grid a -> Grid a
rotateGrid (Grid gss) = Grid (map reverse (transpose gss))

instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = do w <- choose (1,100)
                   h <- choose (1,100)
                   xss <- vectorOf h (vectorOf w arbitrary)
                   return (Grid xss)

prop_rotateGrid4 :: Eq a => Grid a -> Bool
prop_rotateGrid4 g = r (r (r (r g))) == g
    where r = rotateGrid

checkHaskellFiles :: IO ()
checkHaskellFiles = do files <- listDirectory "."
                       let fs = [f | f <- files, ".hs" `isSuffixOf` f]
                       ss <- mapM readFile fs
                       mapM_ printFile (zip fs ss)
    where
        wrongLines ss = length (filter ((78 <) . length) (lines ss))
        wrongString n | n == 1    = "one line is"
                      | otherwise = show n ++ " lines are"
        printFile (fs,ss) = putStrLn (fs ++ ": " ++ (wrongString (wrongLines ss)) ++ " too long")
    