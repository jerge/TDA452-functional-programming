import Test.QuickCheck
import Data.Char

xmas :: Int -> IO()
xmas n = do mapM_ putStrLn (xmasLines n)
    
xmasLines :: Int -> [String]
xmasLines n = [concat (replicate (n - m) " " ++ replicate m " *") | m <- [1..n]]

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p [] = []
splitWhen p (x:[]) = [[x]]
splitWhen p xs = l : (splitWhen p rs)
    where (l,r:rs) = span (not . p) xs

prop_splitWhen :: (a-> Bool) -> [a] -> Bool
prop_splitWhen p xs = length (splitWhen p xs) == length (filter p xs) + 1
{-
fa :: Eq a => [a] -> a -> [b] -> Maybe b
fb :: [(a -> a)] -> a -> a
fc :: Eq a => [a] -> [[a]] -> Bool
fd :: a -> ([[a]] -> [[a]])
--fd 1 [[3,4],[5,6]] == [[1,3,4],[1,5,6]]
-}
data Sudoku = Sudoku [[Int]]

ex = Sudoku
    [[3,6,0,0,7,1,2,0,0],[0,5,0,0,0,0,1,8,0],[0,0,9,2,0,4,7,0,0],
    [0,0,0,0,1,3,0,2,8],[4,0,0,5,0,2,0,0,9],[2,7,0,4,6,0,0,0,0],
    [0,0,5,3,0,8,9,0,0],[0,8,3,0,0,0,0,6,0],[0,0,7,6,9,0,0,4,3]]

showSudoku :: Sudoku -> String
showSudoku (Sudoku (xs:[])) = showLine (map intToDigit xs)
showSudoku (Sudoku (xs:xss)) = showLine (map intToDigit xs) ++ replicate 17 '-' ++ "\n" ++ showSudoku (Sudoku xss)

showLine :: [Char] -> String
showLine (x:[]) | x == '0' = " \n"
                | otherwise = (x:"\n")
showLine (x:xs) | x == '0' = " |" ++ showLine xs
                | otherwise = (x:"|") ++ showLine xs

block :: (Int, Int) -> Sudoku -> [[Int]]
block (c,r) (Sudoku xss) = map (take 3 . (drop (c*3))) rows
    where rows = take 3 (drop (r*3) xss)

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show

exTree = Node 2 (Node 3 (leafNode 1) Leaf) (Node 1 (leafNode 1) (leafNode 0))
    where leafNode n = Node n Leaf Leaf

hBalanced :: Tree a -> (Int,Bool)
hBalanced Leaf = (0,True)
hBalanced (Node x l r) = ((max li ri)+1, and [lb,rb,(abs(li-ri) < 2)])
            where 
                (li,lb) = hBalanced l
                (ri,rb) = hBalanced r

allPaths :: Tree a -> [[a]]
allPaths Leaf = [[]]
allPaths (Node x l r) = map (x:) (allPaths l) ++ map (x:) (allPaths r)

balTree :: Gen (Tree Bool)
balTree = sized bTree

bTree :: Int -> Gen (Tree Bool)
bTree height | height <= 0 = return Leaf
             | otherwise   = do 
                  n <- elements [False, True]
                  (lh,rh) <- elements [(height-1,height),(height,height),(height,height-1)]
                  lt <- bTree (lh-1)
                  rt <- bTree (rh-1)
                  return (Node n lt rt)
