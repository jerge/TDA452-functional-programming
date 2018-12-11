module Utils where

import Minesweeper

import Data.Char


changeCoord :: (Pos,Pos) -> Pos
changeCoord ((x,y),(cx,cy)) = (x+cx,y+cy)

validPos :: Int -> Int -> Pos -> Bool
validPos w h (x,y) = x >= 0 && y >= 0 && x < w && y < h

getTileAtPos :: Minefield -> Pos -> Tile
getTileAtPos (Minefield m) (r,c) = (m !! r) !! c
{- -- Returns an array of all empty positions
empties :: Minefield -> [Pos]
empties (Minefield rows) =
    [ (rowNum, colNum) | (rowNum, row) <- zip [0..] rows, 
                        (colNum, cell) <- zip [0..] row
                        , isEmpty cell ]
-}
-- Updates the value of an index in a list
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | null xs
            || length xs <= i 
            || i < 0     = error "Index out of bounds"
             | otherwise = l++(y:rs)
    where (l,r:rs) = splitAt i xs

-- Updates at the specified location how many neighbouring mines there are
updateNumberAtPos :: Minefield -> Pos -> Minefield
updateNumberAtPos m p = update m p (calcNeighbourCount m width height p)
    where width = length ((rows m) !! 0)
          height = length (rows m)

-- Update a minefield at a position with a specified tile
update :: Minefield -> Pos -> Tile -> Minefield
update (Minefield m) (r,c) y = Minefield (m !!= (r,(m !! r) !!= (c,y)))

-- Calculates how many mines there are neighbouring the specified position
calcNeighbourCount :: Minefield -> Int -> Int -> Pos -> Tile
calcNeighbourCount m w h p | n == 0 = Empty
                           | otherwise = Num n
    where n = length (filter isMine (map (getTileAtPos m) (neighbourPos w h p)))

-- Temporary function to return all 8 neighbouring positions
allNeighbourPos :: [Pos]
allNeighbourPos = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,0), (0,1), (1,-1), (1,0), (1,1)]

-- Returns all valid neighbouring positions of a position
neighbourPos :: Int -> Int -> Pos -> [Pos]
neighbourPos w h p = filter (validPos w h) (map changeCoord (zip (replicate 8 p) allNeighbourPos))