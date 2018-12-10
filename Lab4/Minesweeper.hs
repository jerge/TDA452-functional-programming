-- This is a haskell file
module Minesweeper where

import Test.QuickCheck
import Data.Char

data Tile = Unknown | Flag | Num Int | Mine | Empty
    deriving (Eq, Show)

-- The Idea is to have two minefields, one which represents the actual state (Mines or empties)
-- and one to represent what the user knows (Unknown, Flag, Num Int, Mine or Empties)
data Minefield = Minefield { rows :: [[Tile]]}
    deriving (Eq, Show)

instance Arbitrary Tile where
    arbitrary =
        do n <- choose(1, 8)
           tile <- elements [Unknown, Empty, (Num n), Mine, Flag]
           return tile
{-
instance Arbitrary Minefield where
    arbitrary =
        do height <- choose (1,20)  --Set height at atleast one row
           width <- choose (2,30)   --Set width so that there is atleast 2 cells and at most 20*30
           mines <- choose (1, width*height-1)
           rand <- chooseAny

makeMinefield :: Int -> Int -> Int -> StdGen -> Minefield
makeMinefield w h m r = 
-}

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

changeCoord :: (Pos,Pos) -> Pos
changeCoord ((x,y),(cx,cy)) = (x+cx,y+cy)

validPos :: Int -> Int -> Pos -> Bool
validPos w h (x,y) = x >= 0 && y >= 0 && x < w && y < h

type Pos = (Int, Int)

getTileAtPos :: Minefield -> Pos -> Tile
getTileAtPos (Minefield m) (r,c) = (m !! r) !! c

empties :: Minefield -> [Pos]
empties (Minefield rows) =
    [ (rowNum, colNum) | (rowNum, row) <- zip [0..] rows, 
                        (colNum, cell) <- zip [0..] row
                        , isEmpty cell ]

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | null xs
            || length xs <= i 
            || i < 0     = error "Index out of bounds"
             | otherwise = l++(y:rs)
    where (l,r:rs) = splitAt i xs
-- Work here \/
calcAllNeighbourCount :: Minefield -> Minefield
calcAllNeighbourCount (Minefield m) = map (cal m) (empties m)

cal :: Minefield -> Pos -> Minefield
cal m p = update m p (calcNeighbourCount m width height p)
    where width = length (m !! 0)
          height = length m
-- work here /\
update :: Minefield -> Pos -> Tile -> Minefield
update (Minefield m) (r,c) y = Minefield (m !!= (r,(m !! r) !!= (c,y)))

calcNeighbourCount :: Minefield -> Int -> Int -> Pos -> Tile
calcNeighbourCount m w h p | n == 0 = Empty
                           | otherwise = Num n
    where n = length (filter isMine (map (getTileAtPos m) (neighbourPos p w h)))

-- Temporary function to return all 8 neighbouring positions
allNeighbourPos :: [Pos]
allNeighbourPos = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,0), (0,1), (1,-1), (1,0), (1,1)]

neighbourPos :: Int -> Int -> Pos -> [Pos]
neighbourPos w h p = filter (validPos w h) (map changeCoord (zip (replicate 8 p) allNeighbourPos))