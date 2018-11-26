
-- | Examples of type classes and overloading
-- Functional Programming course 2018.
-- Thomas Hallgren

{-
This is just a skeleton, the definitions will be filled in
during the lecture.
-}

module Overloading where

    --------------------------------------------------------------------------------
    -- * Defining your own Eq instances
    
    data TrafficLight = Red | Yellow | Green deriving (Show,Enum,Bounded)
    
    instance Eq TrafficLight where
      Red    == Red    = True
      Yellow == Yellow = True
      Green  == Green  = True
      _      == _      = False
    
    
    -------------------------------------------------------------------------------
    -- * The Bounded class
    
    -- | If a type is in both Bounded and Enum,
    -- it's easy to enumerate all the values in the type
    enumAll :: (Bounded a,Enum a) => [a]
    enumAll = [minBound .. maxBound]
    
    
    --------------------------------------------------------------------------------
    -- * Writing your own Show instances
    
    -- | Every card has a suit  â™  â™¥ â™¦ â™£
    data Suit = Spades | Hearts | Diamonds | Clubs
                deriving (Eq,Bounded,Enum)
    
    
    instance Show Suit where
        show Spades = "s"
        show Hearts = "h"
        show Clubs = "c"
        show Diamonds = "d"
    
    -- | ANSI color escape sequences
    red = "\ESC[31m"
    normal = "\ESC[m"
    
    
    -- | Cards have ranks: 2, 3 .. 10, J, Q, K, A
    data Rank = Numeric Int | Jack | Queen | King | Ace
                deriving (Eq,Ord)
    
    all_ranks = [Numeric n|n<-[2..10]]++[Jack,Queen,King,Ace]
    
    rankBeats :: Rank -> Rank -> Bool
    rankBeats r1 r2 = r1>r2

    instance Show Rank where
        show (Numeric n) = show n
        show Jack = "J"
        show Queen = "Q"
        show King = "K"
        show Ace = "A"
    
    -- | A Card has a Rank and a Suit
    data Card = Card {rank::Rank, suit::Suit}
                deriving (Eq)
    
    cardBeats :: Card -> Card -> Bool
    cardBeats (Card r1 s1) (Card r2 s2) = s1==s2 && rankBeats r1 r2
    
    example_card_1 = Card King Clubs
    example_card_2 = Card {rank=Ace, suit=Spades}
    
    
    instance Show Card where
        show (Card r s) = show r ++ show s
    
    
    -- | A hand contains zero or more cards
    data Hand = Empty | Add Card Hand
    
    example_hand_0 = Empty
    
    example_hand_1 = Add example_card_1 Empty
    
    example_hand_2 = Add example_card_2 example_hand_1
    
    example_hand_3 = Add (Card (Numeric 5) Hearts) example_hand_2
    
    instance Show Hand where
        show (Empty) = ""
        show (Add c h) = show c ++ " " ++ show h
    
    
    --------------------------------------------------------------------------------
    -- * Defining your own class
    
    -- | The class of "small" types, e.g. types for which we can enumerate
    -- all values
    --class Small
    
    class Small a where
        values :: [a]
    
    instance Small ()       where values = [()]
    instance Small Bool     where values = [False, True]-- enumAll
    instance Small Char     where values = enumAll
    instance Small Int      where values = enumAll
    
    instance (Small a, Small b) => Small (a,b) where
        values = [(x,y) | x <- values, y <- values]
    
    smallCheck1 :: Small a => (a -> Bool) -> Bool
    smallCheck1 p = and [p x | x <- values]
    
    --prop_Ace c = not (c `cardBeats` Card Ace Spades)
    -- values = [False, True]-- enumAll
    
    
    --------------------------------------------------------------------------------
    -- * Small instances
    
    --instance Small ()  
    --instance Small Bool
    --instance Small Char
    --instance Small Int
    
    
    -- | We consider pairs of small types to be small too
    --instance Small (a,b) where
    
    
    --instance Small Suit where
    --instance Small Rank where
    --instance Small Card where
    
    
    --------------------------------------------------------------------------------
    -- * A function for exhaustive testing properties with one small argument
    
    --smallCheck1 :: Small a => (a->Bool) -> Bool
    --smallCheck1 p = 
    
    
    --------------------------------------------------------------------------------
    -- * Properties that can be tested exhaustively
    
    -- | There is no card that can beat an Ace
    prop_Ace c = not (c `cardBeats` Card Ace Spades)
    
    -- | Any face card beats any numeric card of the same suit
    --prop_Face fc nc = (isFace fc && isNumeric nc && suit fc==suit nc)
                       -- ==> (fc `cardBeats` nc)
    
    isFace c = rank c>=Jack
    isNumeric c = rank c<Jack
    
    
    -- | Logical implication
    --infixr 0 ==>
--    h ==> c = not h || c
    
    
    --------------------------------------------------------------------------------
    -- * The class for exhaustive test of properties with zero or more
    -- "small" arguments
    
    -- | The class of properties that can be tested exhaustively
    --class SmallCheck prop where ...
    
    
    -- | Bool is testable (base case)
    --instance ...
    
    -- | Functions with any number of small arguments are testable (inductive step)
    --instance ...
    
    
    --------------------------------------------------------------------------------
    -- * Ambiguity and defaulting
    
    --f s = show (read s)         -- defaulting does not kick in
    
    g s = show (10+read s)        -- with Num constraints, defaulting kicks in
    
    
    --Defaulting, monomorphism restriction
    answer1 = 6*7
    
    -- Avoiding the defaulting caused by the monomorphism restriction
    answer2 :: Num a => a
    answer2 = 6*7
    