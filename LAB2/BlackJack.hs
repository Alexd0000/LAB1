module BlackJack where
import Cards
import RunGame

-- Part A: Task 3.1 (reading the document), Task 3.2 (writing out what happens to the size function), and part of Task 3.3, namely: implement the functions "empty", "value", "gameOver", and "winner".


--  Function that returns an empty hand
empty :: Hand
empty = Empty



-- Function that calculates the value of a Rank
-- Use default value 11 for aces
valueRank :: Rank -> Integer
valueRank rank =
	case rank of
		Jack -> 10
		Queen -> 10
		King -> 10
		Numeric m -> m
		Ace -> 11

-- Function that calculates the value of a Card
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- That calculates the number of aces in a given hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) = case (rank card) of
	Ace -> 1 + numberOfAces hand
	otherwise -> numberOfAces hand

-- Function that calculates the value of the hand where all aces are valued 11
valueHelp :: Hand -> Integer
valueHelp Empty = 0
valueHelp (Add card hand) = valueCard card + value hand

-- Function that calculates the value of the hand
value :: Hand -> Integer
value hand | (valueHelp hand) > 21 = (valueHelp hand) - 10*(numberOfAces hand)
           | otherwise = valueHelp hand 

-- Given a hand, is the player bust?
gameOver :: Hand -> Bool
gameOver hand = (value hand > 21)

-- Given one hand for the guest and one for the bank (in that order), which player has won?
winner :: Hand -> Hand -> Player
winner handGuest handBank | (value handGuest > value handBank) && (not (gameOver handGuest)) && (not (gameOver handBank)) = Guest
                          | (not (gameOver handGuest)) && gameOver handBank = Guest
	                      | otherwise = Bank