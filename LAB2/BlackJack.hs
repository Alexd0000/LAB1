module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- #########################################################################
-- ############################ LAB 2 - PART A #############################
-- #########################################################################

-- ================================ Task 3.2 ===============================

-- In this task, we had to write out what happens when we apply the size
-- function on hand2.
-- hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)

-- size hand2
--    = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--    = 1 + size (Add (Card Jack Spades) Empty)
--    = 1 + 1 + size Empty
--    = 1 + 1 + 0
--    = 2

-- =========================================================================

-- ================================ Task 3.3 ===============================

-- In this task (for part A), we had to implement the functions "empty",
-- "value", "gameOver", and "winner".


--  Function that returns an empty hand.
empty :: Hand
empty = Empty



-- Function that calculates the value of a Rank.
-- We use default value 11 for aces.
valueRank :: Rank -> Integer
valueRank rank =
 case rank of

		Numeric m	-> m
		Jack -> 10
		Queen -> 10
		King -> 10
		Ace -> 11

-- Function that calculates the value of a Card.
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- Function that calculates the number of aces in a given hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) = case (rank card) of
	Ace -> 1 + numberOfAces hand
	otherwise -> numberOfAces hand

-- Function that calculates the value of the hand where all aces are 11.
valueHelp :: Hand -> Integer
valueHelp Empty = 0
valueHelp (Add card hand) = valueCard card + valueHelp hand

-- Function that calculates the true value of the hand putting the aces at
-- 1 if the value of the hand is larger than 21.
value :: Hand -> Integer
value hand | valueHelp hand <= 21 = valueHelp hand
           | otherwise = valueHelp hand - 10*numberOfAces hand

-- Function that calculates if a hand has a value larger than 21. If it is
-- the case the player holding this hand has lost.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- Function that determines who is the winner between a Guest and the Bank
-- given their hands and according to Black Jack rules.
winner :: Hand -> Hand -> Player
winner handGuest handBank | value handGuest > value handBank
														&& not (gameOver handGuest) = Guest
                          | not (gameOver handGuest) && gameOver handBank = Guest
	                      | otherwise = Bank

-- function that given two hands puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
(Add card hand1) <+ hand2   = Add card (hand1 <+ hand2)

--QuickCheck properties for onTopOf function
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = size hand1 + size hand2 == size (hand1 <+ hand2)

--function that given a hand and a suit adds all numerical cards with that suit
--works only if initial given set is empty -> should only be used by suitCards
suitCardsNum :: Suit -> Hand -> Hand
suitCardsNum suit hand | size hand < 9 = suitCardsNum suit (Add (Card (Numeric (size hand+2)) suit) hand)
suitCardsNum suit hand | otherwise = hand

--function that given a suit returns a hand consisting of all cards in that suit
suitCards :: Suit -> Hand
suitCards suit = Add (Card Ace suit) (Add (Card King suit) (Add (Card Queen suit)
                (Add (Card Jack suit) (suitCardsNum suit Empty))))

-- TEST TEST
oneSuitCardsNum :: Suit -> Hand
oneSuitCardsNum suit = foldr (<+) Empty ([Add (Card (Numeric n) suit) Empty | n<-[2..10]] ++ [Add (Card f suit) Empty | f<-[Queen,King,Jack,Ace]])

fullDeckBis :: Hand
fullDeckBis = foldr (<+) Empty ([oneSuitCardsNum s | s<-[Spades,Hearts,Clubs,Diamonds]])

-- =====


-- function that returns a full deck of cards
fullDeck :: Hand
fullDeck = suitCards Hearts <+ suitCards Spades <+ suitCards Diamonds <+ suitCards Clubs

--function that draws one card from a deck to a hand and returns both
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)

--helper function that takes the first from pair
first :: (a, b) -> a
first (x,y) = x

--helper function that draws card from deck to hand
playBank' :: Hand -> Hand -> (Hand, Hand)
playBank' deck bankHand = (deck',bankHand')
  where (deck',bankHand') = draw deck bankHand

--function that plays for the bank according to the rules
--Which deck to use???
--playBank :: Hand -> Hand
--playBank hand | value hand < 16 = playBank (first (playBank' deck hand))
--              | otherwise = hand


-- =========================================================================