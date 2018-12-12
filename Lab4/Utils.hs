module Utils where

import Minesweeper

import Data.Char

-- Adds the elements of a tuple of tuples into new tuples
addPos :: (Pos,Pos) -> Pos
addPos ((x,y),(cx,cy)) = (x+cx,y+cy)

-- Checks if a position is inside the height and width,
-- as well as being larger than 0
validPos :: Pos -> Pos -> Bool
validPos (h,w) (r,c) = r >= 0 && c >= 0 && c < w && r < h

-- Returns the tile at the position
getTileAtPos :: Minefield -> Pos -> Tile
getTileAtPos (Minefield m) (r,c) = (m !! r) !! c

-- Updates the value of an index in a list
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | null xs
            || length xs <= i 
            || i < 0     = error "Index out of bounds"
             | otherwise = l++(y:rs)
    where (l,r:rs) = splitAt i xs

-- Updates at the specified location how many neighbouring mines there are
updateNumberAtPos :: Minefield -> Pos -> Minefield
updateNumberAtPos m p = update m p (calcNeighbourCount m (height,width) p)
    where width = length (head (rows m))
          height = length (rows m)

-- Update a minefield at a position with a specified tile
update :: Minefield -> Pos -> Tile -> Minefield
update (Minefield m) (r,c) y = Minefield (m !!= (r,(m !! r) !!= (c,y)))

-- Calculates how many mines there are neighbouring the specified position
calcNeighbourCount :: Minefield -> Pos -> Pos -> Tile
calcNeighbourCount m (h,w) p | n == 0 = Empty
                             | otherwise = Num n
    where n = length (filter isMine (map (getTileAtPos m) (neighbourPos (h,w) p)))

-- Temporary function to return all 8 neighbouring positions
allNeighbourPos :: [Pos]
allNeighbourPos = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

-- Returns all valid neighbouring positions of a position
neighbourPos :: Pos -> Pos -> [Pos]
neighbourPos (h,w) p = filter (validPos (h,w)) (zipWith (curry addPos) (replicate 8 p) allNeighbourPos)


---------------------- Unused ------------------------

{- -- Returns an array of all empty positions
empties :: Minefield -> [Pos]
empties (Minefield rows) =
    [ (rowNum, colNum) | (rowNum, row) <- zip [0..] rows, 
                        (colNum, cell) <- zip [0..] row
                        , isEmpty cell ]
-}