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

cell :: Int -> Int -> Gen (Tile)
cell mines tiles = frequency [(mines, return Mine),
                              (tiles, return Empty)]

instance Arbitrary Minefield where
    arbitrary =
        do height <- choose (1,20)  --Set height at atleast one row
           width <- choose (2,30)   --Set width so that there is atleast 2 cells and at most 20*30
           mines <- choose (1, width*height-1)
           mf <- vectorOf height (vectorOf width (cell mines (width*height)))
           return (Minefield mf)

-- 7*14
exampleMinefield :: Minefield
exampleMinefield = 
    Minefield [
        [e,e,e,e,m,e,e,m,m,e,e,e,m,e],
        [e,m,e,e,e,e,m,e,e,e,e,e,m,e], 
        [m,m,m,e,e,e,e,e,e,e,m,m,e,e], 
        [m,m,e,e,e,m,e,m,m,e,m,e,m,e], 
        [m,e,m,e,m,e,e,m,m,m,e,e,e,e], 
        [e,e,e,m,e,m,e,e,m,e,m,m,e,e], 
        [e,e,m,e,e,e,e,e,m,e,e,e,e,e] 
    ]
    where 
        m = Mine
        e = Empty

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

type Pos = (Int, Int)

revealTile :: Minefield -> Minefield -> Pos -> (Minefield, Minefield)
revealTile userMinefield minefield (x,y) = update 
