-- definition of a datatype
data Suit = Spades | Hearts | Diamonds | Clubs
            deriving Show -- Makes a toString function for the datatype
data Colour = Black | Red
            deriving Show

colour :: Suit -> Colour
--colour c | c == Spades || c == Clubs = Black
--         | otherwise c = Red
colour Spades = Black
colour Clubs = Black
colour anythingElse = Red -- The variable name can be anything

-- colour suit = 
--    case suit of
--      Spades -> Black etc.
--      Hearts -> Red etc.

-- Om inte hela definitionsmängden har definierat värde kastas error om man försöker använda de andra
-- "set -Wincomplete-patterns" can be used to see if errors will be thrown

data Rank = Numeric Int | Jack | Queen | King | Ace -- However now we can be outside of 2-10
            deriving (Show,Eq,Ord) -- The rank is defined by the order it was written

--data Rank' = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King | Ace
--            deriving Show

-- Seems that you can also do a function which checks if the number is in the range

all_ranks :: [Rank]
all_ranks = [Numeric n|n <- [2..10]] ++ [Jack, Queen, King, Ace]

{-
rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False -- Nothing beats an ace
rankBeats Ace _ = True -- An ace beats everything else
rankBeats _ King = False 
rankBeats King _ = True 
rankBeats _ Queen = False 
rankBeats Queen _ = True 
rankBeats _ Jack = False 
rankBeats Jack _ = True 
rankBeats (Numeric m) (Numeric n) | n > m
-}

rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

-- (Ace, Hearts) -- Can use Tuples instead, but let's not

{-
data Card = Card Rank Suit
            deriving Show
-}

data Card = Card { rank:: Rank, suit::Suit } -- Creates a kind of verbose "Show"
            deriving Show

{-
rank :: Card -> Rank
rank (Card r s) = r

suit :: Card -> Suit
suit (Card r s) = s
-}

example_card_1 = Card Ace Hearts
example_card_2 = Card {rank = King, suit = Spades } -- Can change order of the parameters

cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1==s2 && rankBeats r1 r2
{-cardBeats' card1 card2 =   suit card1 == suit card2 
                        && rankBeats (rank card1) (rank card2)-} -- Means same thing as above row