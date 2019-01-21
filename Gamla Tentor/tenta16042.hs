import Test.QuickCheck hiding (shuffle)
import Data.List

range :: Ord a => [a] -> (a,a)
range (a:as) = r (a,a) as

r :: Ord a => (a,a) -> [a] -> (a,a)
r (small,large) [] = (small,large)
r (small,large) (x:xs) = ((min x mi),(max x ma))
    where (mi,ma) = r (small,large) xs

splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf as [] = [[]]
splitOneOf [] as = [as]
splitOneOf as bs = l : (splitOneOf as rs)
    where (l,(r:rs)) = span (ne as) bs
          ne xs x = notElem x xs

prop_splitOneOf :: [Int] -> [Int] -> Bool
prop_splitOneOf as bs = length (splitOneOf as bs) == length (filter (`elem` as) bs) +1

-- fa :: Eq a => a -> [b] -> [a] -> Maybe b
-- fb :: [(a->b)] -> [[a] -> [b]]
-- fc :: Eq a => [a] -> [a] -> Bool

fb' = map map
fc' as bs = and (zipWith (/=) as bs)

type Sudoku = [[Int]]

ex :: Sudoku
ex = [[3,6,0,0,7,1,2,0,0],
    [0,5,0,0,0,0,1,8,0],
    [0,0,9,2,0,4,7,0,0],
    [0,0,0,0,1,3,0,2,8],
    [4,0,0,5,0,2,0,0,9],
    [2,7,0,4,6,0,0,0,0],
    [0,0,5,3,0,8,9,0,0],
    [0,8,3,0,0,0,0,6,0],
    [0,0,7,6,9,0,0,4,3]]

--Fail
allBlanks :: Sudoku -> [(Int,Int)]
allBlanks s = [(y,x) | (row,y) <- (zip s [0..8]), (col,x) <- (zip row [0..8]), x == 0]

allBlanks' s =
    [(x,y) | (y,row) <- zip [0..] s,
    (x,0  ) <- zip [0..] row  ]

updateWith :: Int -> (Int,Int) -> Sudoku -> Sudoku
updateWith x (c,r) s = take (r) s ++ updatedRow : (drop (r +1) s)
    where 
        row = s !! r    
        updatedRow = take (c) row ++ x : drop (c + 1) row

arbPuzzle :: Sudoku -> Int -> Gen Sudoku
arbPuzzle s n = do let nonBlank = [(x,y) | x <- [0..8], y <- [0..8]] \\ allBlanks' s
                   shuffled <- (shuffle nonBlank)
                   let toUpdate = take n shuffled
                   return $ foldr (updateWith 0) s toUpdate

shuffle :: [(Int,Int)] -> Gen [(Int,Int)]
shuffle [] = return []
shuffle xs = do
    a <- elements xs
    as <- shuffle (delete a xs)
    return $ a:as

type VarName = String

data BinaryOp  = Add | Mul | Sub | Power
data UnaryOp   = Sin | Cos | Tan | Abs
data CompOp    = GT  | LT  | GEQ | LEQ -- (>, <, >=, <=)
data LogicOp   = And | Or

data AExpr = Const Double | Var VarName 
            | BOP BinaryOp AExpr AExpr 
            | UOP UnaryOp AExpr 
            | IF BExpr AExpr AExpr

data BExpr = COP CompOp AExpr AExpr | LOP LogicOp BExpr BExpr

vars :: AExpr -> [VarName]
vars (Var n) = [n]
vars (BOP _ e1 e2) = nub (vars e1 ++ vars e2)
vars (UOP _ e1) = vars e1
vars (IF b e1 e2) = nub (bvars b ++ vars e1 ++ vars e2)
vars _ = []

bvars :: BExpr -> [VarName]
bvars (COP _ e1 e2) = nub (vars e1 ++ vars e2)
bvars (LOP _ b1 b2) = nub (bvars b1 ++ bvars b2)