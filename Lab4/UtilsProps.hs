module UtilsProps where

import Utils
import Minesweeper

import Test.QuickCheck

prop_bangBangEquals :: Integral a => [a] -> a -> Property
prop_bangBangEquals xs y = length xs > 0 ==> forAll (choose (0,length xs - 1)) 
                                (prop_bangBangEquals' xs y)

prop_bangBangEquals' :: Eq a => [a] -> a -> Int -> Bool
prop_bangBangEquals' xs y i = xs !!= (i,y) !! i == y

-- TODO Do this with forAll
prop_updateUpdated :: Minefield -> Pos -> Tile -> Property
prop_updateUpdated (Minefield m) (r,c) y = validPos (h,w) (r,c) ==> xs !! r !! c == y
        where h = length m
              w = length (head m)
              (Minefield xs) = update (Minefield m) (r,c) y

