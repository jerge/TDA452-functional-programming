import Test.QuickCheck
import Data.List

xmas :: Int -> IO()
xmas n = putStr (xmasTree n)

xmasTree :: Int -> String
xmasTree n = [s | k <- [1..n], s <- (stars k)]
    where 
        stars :: Int -> String
        stars x = replicate (n-x) ' ' ++ concat (replicate x "* ") ++ "\n"

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f [] = []
splitWhen f xs | null l = [(r:rs)]
               | otherwise = l:(splitWhen f rs)
    where (l,(r:rs)) = span (not . f) xs

prop_splitWhen :: (a -> Bool) -> [a] -> Bool
prop_splitWhen f xs = length (splitWhen f xs) == length (filter f xs) + 1

-- fa :: Eq a => [a] -> a -> [b] -> Maybe b
-- fb :: [(a->a)] -> a -> a
-- fc :: Eq a => [a] -> [[a]] -> Bool
-- fd :: a -> ([[a]] -> [[a]])
-- fd 1 [1,2,3,4] = [[1,1],[1,2],[1,3],[1,4]]

data Sudoku = Sudoku [[Int]]

ex = Sudoku
    [[3,6,0,0,7,1,2,0,0],[0,5,0,0,0,0,1,8,0],[0,0,9,2,0,4,7,0,0],
    [0,0,0,0,1,3,0,2,8],[4,0,0,5,0,2,0,0,9],[2,7,0,4,6,0,0,0,0],
    [0,0,5,3,0,8,9,0,0],[0,8,3,0,0,0,0,6,0],[0,0,7,6,9,0,0,4,3]]

showSudoku :: Sudoku -> String
showSudoku (Sudoku s) = concat (intersperse ("\n" ++ replicate 17 '-' ++ "\n") (map (concat . (intersperse "|") . (map replaceZero)) s))
        where replaceZero n | n == 0 = " "
                            | otherwise = show n

block :: (Int,Int) -> Sudoku -> [[Int]]
block (c,r) (Sudoku s) = cols
    where rows = take 3 (drop (3*r) s)
          cols = map ((take 3) . (drop (3*c))) rows

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show

exTree = Node 2 (leafNode 1) (Node 1 (leafNode 1) (leafNode 0))
    where leafNode n = Node n Leaf Leaf

hBalanced :: Tree a -> (Int,Bool)
hBalanced Leaf = (0,True)
hBalanced (Node _ t1 t2) = ((1 + max h1 h2),(b1 && b2 && (abs (h1-h2) <= 1)))
        where (h1,b1) = hBalanced t1
              (h2,b2) = hBalanced t2

allPaths :: Tree a -> [[a]]
allPaths Leaf = [[]]
allPaths (Node a t1 t2) = fd a (allPaths t1) ++ fd a (allPaths t2)

fd :: a -> [[a]] -> [[a]]
fd x = map (x:)

balTree :: Gen (Tree Bool)
balTree = sized bTree

bTree :: Int -> Gen (Tree Bool)
bTree 0 = do return (Leaf)
bTree n = do a <- elements [True, False]
             lHeight <- choose (1,n-1) -- Should specify max diff 1
             lT <- bTree (lHeight)
             rHeight <- choose (1,n-1) -- Should specify max diff 1
             rT <- bTree (rHeight)
             return (Node a lT rT)

