module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
-- A0 
-- size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)) =
-- 1 + size (Add (Card Jack Spades) Empty) =
-- 1 + 1 + size (Empty) =
-- 1 + 1 + 0 =
-- 2

-- A1
-- A function that returns an empty hand
-- This function is however never used, since we use a Capital letter for Empty instead..
empty :: Hand
empty = Empty

-- A2
-- Returns the value of a hand if it's below or equal to 21
-- Otherwise it returns the value of a hand as if Ace is 11 
-- but removes 10 for each ace in the hand (i.e. Ace has value 1)
value :: Hand -> Integer
value hand = if initialValue hand <= 21
             then initialValue hand
             else (initialValue hand - (numberOfAces hand * 10))

-- Calculates the amount of Aces in a hand recursively
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add c h) = numberOfAces h

-- Returns the value of a hand with Ace always being 11
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueCard c + initialValue h

-- Function that returns the value of a Card's rank
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- Function that returns the value for a rank
-- Always returns 11 for Aces
-- Return the number given a Numeric
-- otherwise returns 10
valueRank :: Rank -> Integer
valueRank Ace = 11
valueRank (Numeric a) = a
valueRank _ = 10 

exampleHandBust = (Add (Card Jack Spades)   --10
              (Add (Card Ace Hearts)        --10+1
              (Add (Card (Numeric 2) Spades)--10+1+2
              (Add (Card Queen Spades)      --10+1+2+10
              Empty))))

exampleHand13 = (Add (Card Ace Hearts)      --1
              (Add (Card (Numeric 2) Spades)--1+2
              (Add (Card Queen Spades)      --1+2+10
              Empty)))

exampleHand17 = (Add (Card King Hearts)     --10
              (Add (Card (Numeric 7) Spades)--10+7
              Empty))
-- A3
-- Returns true if the hand's value is above 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4
-- Returns the Winner depending on the hand taking two arguments
-- 1. the Guest's Hand, 2. the Bank's hand
-- Starts by checking if the player is bust, which always means Bank won
-- Afterwards checks if the Bank is bust, which means that Guest won
-- Finally checks if the Bank has a higher or equal value to the Guest
winner :: Hand -> Hand -> Player
winner guest bank   | gameOver guest = Bank
                    | gameOver bank = Guest
                    | value bank >= value guest = Bank
                    | otherwise = Guest
