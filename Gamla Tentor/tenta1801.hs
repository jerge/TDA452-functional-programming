import Data.Maybe
import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Test.QuickCheck
import System.Directory


lookupAll :: Eq key => key -> [(key,value)] -> [value]
lookupAll k dict = [val | (key,val) <- dict, k == key]

lookup :: Eq key => key -> [(key,value)] -> Maybe value
lookup k dict = listToMaybe (lookupAll k dict)

--lookup key = listToMaybe . lookupAll key
{-
update :: Eq key => key -> value -> [(key,value)] -> [(key,value)]
update k v dict | null entries =  (k,v) : dict
                | otherwise = (k,v) : [(delete (k, head entries) dict)]
                where entries = lookupAll k dict
-}

data Expr  = X | Num Int | Op BinOp Expr Expr  deriving (Eq,Show)
data BinOp = Add | Mul | Subtract              deriving (Eq,Show)

ex1 = Op Subtract (Num 100) X                  -- 100 - X
ex2 = Op Add (Num 100) (Op Mul (Num (-1)) X)   -- 100 + (-1)*X

eval :: Expr -> Int -> Int
eval X x = x
eval (Num n) _ = n
eval (Op operator e1 e2) x | operator == Add      = eval e1 x + eval e2 x
                           | operator == Mul      = eval e1 x * eval e2 x
                           | operator == Subtract = eval e1 x - eval e2 x

removeSub :: Expr -> Expr
removeSub (Op Subtract e1 e2) = Op Add (removeSub e1) (Op Mul (Num (-1)) (removeSub e2))
removeSub (Op nonsub e1 e2) = Op nonsub (removeSub e1) (removeSub e2)
removeSub expr = expr

--fa :: Bool -> Bool -> Bool
--fb :: (a -> b -> Bool) -> a -> b -> Bool
--fc :: Fractional a => a -> a -> a
--fd :: [[a]] -> [a]

data Grid a = Grid [[a]]  deriving (Eq,Show)

g1,g2 :: Grid Int    -- Example grids
g1 = Grid [[1,2],
           [3,4],
           [5,6]]
g2 = Grid [[5,3,1],
           [6,4,2]]

mapGrid :: (a->b) -> Grid a -> Grid b
mapGrid f (Grid g) = Grid (map (map f) g)

rotateGrid :: Grid a -> Grid a    -- Example: rotateGrid g1 == g2
rotateGrid (Grid g1) = (Grid . map reverse . transpose) g1

-- No matter what I do in here the generated type will always be the unit type ()
instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = do r <- choose (1,100)
                   c <- choose (1,100)
                   grid <- vectorOf r (vectorOf c arbitrary)
                   return (Grid grid)

prop_rotateGrid :: Eq a => Grid a -> Bool
prop_rotateGrid g = (rotateGrid . rotateGrid . rotateGrid . rotateGrid) g == g
{-
checkHaskellFiles :: IO ()
checkHaskellFiles = do  files <- listDirectory "."
                        let files = chooseHaskellFiles files
                        let readFiles = map reading files
                        let z = zip files readFiles
                        mapM_ ()
    where 
        reading = do r <- readFile
                     return r
        printing (f,s) = do if checkLongLines s <= 0
                            then 

                        -}
                        
                        
                        

chooseHaskellFiles :: [FilePath] -> [FilePath]
chooseHaskellFiles []   = []
chooseHaskellFiles (f:fs) | fileEnding == ".hs" = f: (chooseHaskellFiles fs)
                          | otherwise             = chooseHaskellFiles fs
        where fileEnding = drop (length f-3) f

checkLongLines :: String -> Int
checkLongLines file = length $ filter (78 < length) (lines file)

