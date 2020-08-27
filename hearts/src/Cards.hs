module Cards where

import qualified Data.Set as Set
import Data.Set (Set)

data Suit = Diamonds | Clubs | Spades | Hearts
  deriving (Show, Eq, Ord)

-- |list of all suits
allSuits :: [Suit]
allSuits = [Spades, Hearts, Diamonds, Clubs]

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord)

-- |rankBeats r1 r2 returns True, if r1 beats r2
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

-- |list of all ranks
allRanks :: [Rank]
allRanks = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]

-- |playing cards
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq, Ord)

-- |cardBeats c1 c2 returns True, if c1 beats c2: they have the same suit and c1's rank is higher
cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c
                        && rankBeats (rank givenCard)
                                     (rank c)

-- |full deck of all cards
deck :: [Card]
deck = [Card suit' rank' | rank' <- allRanks, suit' <- allSuits]

-- |during the game, a hand contains at least one card
type Hand = Set Card

-- |does a hand contain zero cards?
isHandEmpty :: Hand -> Bool
isHandEmpty hand = Set.null hand

-- |does a hand contain a specific card?
containsCard :: Card -> Hand -> Bool
containsCard card hand = Set.member card hand

-- |remove a card from a hand
removeCard :: Card -> Hand -> Hand
removeCard card hand = Set.delete card hand

-- |empty hand
emptyHand :: Hand
emptyHand = Set.empty

-- |pretty-print card rank
prettyRank :: Rank -> String
prettyRank (Numeric i) = show i
prettyRank r = show r

-- |pretty-print card suit
prettySuit :: Suit -> String
prettySuit s = show s

-- |pretty-print card
prettyCard :: Card -> String
prettyCard c = prettyRank (rank c) ++ " of " ++ prettySuit (suit c)

-- |pretty-print list of cards
prettyCards :: [Card] -> String
prettyCards [] = ""
prettyCards [x] = prettyCard x
prettyCards (x:xs) = prettyCard x ++ " and\n" ++ prettyCards xs


