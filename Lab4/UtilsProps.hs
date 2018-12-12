module UtilsProps where

import Utils
import Minesweeper

import Test.QuickCheck

-- Generates a position with a height of 0 to (height-1)
-- and width of 0 to (width-1) restriction
genPos :: (Int, Int) -> Gen (Int, Int)
genPos (h,w) = do r <- choose (0,h-1)
                  c <- choose (0,w-1)
                  return (r,c)

-- Chooses an index within the range of the list and checks if
-- the !!= operator really changes the value at the index
prop_bangBangEquals :: Integral a => [a] -> a -> Property
prop_bangBangEquals xs y = length xs > 0 ==> 
                        forAll (choose (0,length xs - 1)) 
                               (prop_bangBangEquals' xs y)

prop_bangBangEquals' :: Eq a => [a] -> a -> Int -> Bool
prop_bangBangEquals' xs y i = xs !!= (i,y) !! i == y

-- Generates a position within the range of the minefield and checks if
-- the update method really changes the value at the position
prop_updateUpdated :: Minefield -> Tile -> Property
prop_updateUpdated m y = forAll (genPos (h,w)) 
                                (prop_updateUpdated' m y)
        where h = length (rows m)
              w = length (head (rows m))

prop_updateUpdated' :: Minefield -> Tile -> Pos -> Bool
prop_updateUpdated' m y p = getTileAtPos (update m p y) p == y

-- Generates a position within the range of the minefield and checks if
-- the neighbourPos method only returns valid positions at that position
prop_neighbourPos :: Minefield -> Property
prop_neighbourPos (Minefield m) = forAll (genPos (h,w)) (prop_neighbourPos' (h,w))
        where h = length m
              w = length (head m)

-- Checks if all positions generated by neighbourPos are valid
prop_neighbourPos' :: Pos -> Pos -> Bool
prop_neighbourPos' restrictions p = all (validPos restrictions) 
                                    (neighbourPos restrictions p)