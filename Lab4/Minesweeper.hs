-- This is a haskell file
module Minesweeper where

import Test.QuickCheck

data Tile = Unknown | Flag | Num Int | Mine | Empty
    deriving (Eq, Show)

-- The Idea is to have two minefields, one which represents the actual state (Mines or empties)
-- and one to represent what the user knows (Unknown, Flag, Num Int, Mine or Empties)
newtype Minefield = Minefield { rows :: [[Tile]]}
    deriving (Eq, Show)

instance Arbitrary Tile where
    arbitrary =
        do n <- choose(1, 8)
           elements [Unknown, Empty, Num n, Mine, Flag]

cell :: Int -> Int -> Gen Tile
cell mines tiles = frequency [(mines, return Mine),
                              (tiles, return Empty)]

instance Arbitrary Minefield where
    arbitrary =
        do height <- choose (1,20)  --Set height at atleast one row
           width <- choose (2,30)   --Set width so that there is atleast 2 cells and at most 20*30
           mines <- choose (1, width*height-1) -- Probability to get mines
           mf <- vectorOf height (vectorOf width (cell mines (width*height)))
           return (Minefield mf)

-- w 14 h 7
exampleMinefield :: Minefield
exampleMinefield = 
    Minefield [
        [e,e,m,e,e,e,m,m,m,e,e,e,m,e],
        [e,e,m,e,e,e,m,e,e,e,e,e,m,e], 
        [e,e,e,e,e,e,e,e,e,e,m,m,e,e], 
        [e,e,e,e,e,e,e,m,m,e,m,e,m,e], 
        [e,e,e,e,e,e,e,m,m,m,e,e,e,e], 
        [e,e,e,m,e,m,e,e,m,e,m,m,e,e], 
        [e,e,m,e,e,e,e,e,m,e,e,e,e,e] 
    ]
    where 
        m = Mine
        e = Empty

allUnknowns :: Minefield
allUnknowns = Minefield [[Unknown | x <- [1..14]] | x <- [1..7]]

allEmpties :: Minefield
allEmpties = Minefield [[Empty | x <- [1..14]] | x <- [1..7]]

amountTile :: Minefield -> Tile -> Int
amountTile (Minefield m) t = length $ filter (== t) (concat m)

isMinefield :: Minefield -> Bool
isMinefield (Minefield (m:ms)) = all (isMinefield' (length m)) ms

isMinefield' :: Int -> [Tile] -> Bool
isMinefield' l ts = length ts == l

isMine :: Tile -> Bool
isMine Mine = True
isMine _ = False

isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty _ = False

isUnknown :: Tile -> Bool
isUnknown Unknown = True
isUnknown _      = False

isNum :: Tile -> Bool
isNum (Num _) = True
isNum _ = False

type Pos = (Int, Int)
