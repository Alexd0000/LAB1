module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

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

-- Function that calculates the true value of the hand putting the aces at
-- 1 if the value of the hand is larger than 21.
value :: Hand -> Integer
value hand | valueHelp hand <= 21 = valueHelp hand
           | otherwise = valueHelp hand - 10*numberOfAces hand
  where
    -- Function that calculates the value of the hand where all aces are 11.
    valueHelp :: Hand -> Integer
    valueHelp Empty = 0
    valueHelp (Add card hand) = valueCard card + valueHelp hand

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


--function that given a suit returns a hand consisting of all cards in that suit
suitCardsNum :: Suit -> Hand
suitCardsNum suit = foldr (<+) Empty ([Add (Card (Numeric n) suit) Empty | n<-[2..10]] ++ [Add (Card f suit) Empty | f<-[Queen,King,Jack,Ace]])

-- function that returns a full deck of cards
fullDeck :: Hand
fullDeck = foldr (<+) Empty ([suitCardsNum s | s<-[Spades,Hearts,Clubs,Diamonds]])


--function that draws one card from a deck to a hand and returns both
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)



--function that plays for the bank according to the rules
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty
  where
  --helper function that draws card from deck to hand
  playBank' :: Hand -> Hand -> Hand
  playBank' deck bankHand | value bankHand >= 16 = bankHand
                          | otherwise = playBank' deck' bankHand'
    where (deck',bankHand') = draw deck bankHand

--function that turns a hand into a list of hands with just one card each
handToList :: Hand -> [Hand]
handToList Empty = []
handToList (Add card hand) = [Add card Empty] ++ handToList hand



-- Function that moves the nth card of an hand
-- It returns a tuple containing the deck withouth the nth card
-- and the card removed
removeNthCard :: Hand -> Hand -> Integer -> (Hand, Card)
removeNthCard (Add card part1Deck) part2Deck 1 = (part1Deck<+part2Deck,card)
removeNthCard (Add card part1Deck) part2Deck nth = removeNthCard part1Deck (Add card part2Deck) (nth-1)

--function that shuffles a hand
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g hand = Add cardRemoved (BlackJack.shuffle g1 tmpHand)
   where
    (nth,g1) = randomR (1, size hand) g
    (tmpHand,cardRemoved)= removeNthCard hand Empty nth

--Test wether a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

--Test whether shuffling returns the same cards
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` BlackJack.shuffle g h

--Test whether the size is preserved during shuffling
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (BlackJack.shuffle g hand)



implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = BlackJack.shuffle
  }

main :: IO ()
main = runGame implementation
-- =========================================================================
