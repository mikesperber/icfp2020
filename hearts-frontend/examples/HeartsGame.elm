module HeartsGame exposing (GameState, GameCommand(..), GameEvent(..), Player, PlayerId,
                            Card, Hand, Trick, Rank(..), Suit(..),
                            emptyHand, emptyGameState, deck, cardColor, prettyCard,
                            processGameEvent)

import Dict exposing (Dict)

type Suit = Diamonds | Clubs | Spades | Hearts

-- |list of all suits
allSuits : List Suit
allSuits = [Spades, Hearts, Diamonds, Clubs]

type Rank = Numeric Int | Jack | Queen | King | Ace

-- |list of all ranks
allRanks : List Rank
allRanks = (List.map Numeric [2, 3, 4, 5, 6, 7, 8, 9, 10]) ++ [Jack, Queen, King, Ace]

-- |playing cards
type alias Card = { suit : Suit, rank : Rank }

-- |full deck of all cards
deck : List Card
deck = List.concat (List.map (\ rank -> List.map (\ suit -> Card suit rank) allSuits) allRanks)

-- |during the game, a hand contains at least one card
type alias Hand = List Card

-- |remove a card from a hand
removeCard : Card -> Hand -> Hand
removeCard card hand = List.filter (\ card0 -> card /= card0) hand

-- |empty hand
emptyHand : Hand
emptyHand = []

-- |pretty-print card rank
prettyRank : Rank -> String
prettyRank rank =
  case rank of
    (Numeric i) -> String.fromInt i
    Jack -> "Jack"
    Queen -> "Queen"
    King -> "King"
    Ace -> "Ace"

-- |pretty-print card suit
prettySuit : Suit -> String
prettySuit suit =
  case suit of
    Diamonds -> String.fromChar '\u{1F0A1}'
    Clubs -> String.fromChar '\u{2665}'
    Spades -> String.fromChar '\u{2665}'
    Hearts -> String.fromChar '\u{2665}'

cardColor : Card -> String
cardColor c =
    case c.suit of
        Spades -> "black"
        Clubs -> "black"
        Hearts -> "red"
        Diamonds -> "red"

-- |pretty-print card
prettyCard : Card -> String
prettyCard c =
  String.fromChar
    (case (c.suit, c.rank) of
          (Spades, Numeric 2) -> '\u{1F0A2}'
          (Spades, Numeric 3) -> '\u{1F0A3}'
          (Spades, Numeric 4) -> '\u{1F0A4}'
          (Spades, Numeric 5) -> '\u{1F0A5}'
          (Spades, Numeric 6) -> '\u{1F0A6}'
          (Spades, Numeric 7) -> '\u{1F0A7}'
          (Spades, Numeric 8) -> '\u{1F0A8}'
          (Spades, Numeric 9) -> '\u{1F0A9}'
          (Spades, Numeric 10) -> '\u{1F0AA}'
          (Spades, Jack) -> '\u{1F0AB}'
          (Spades, Queen) -> '\u{1F0AD}'
          (Spades, King) -> '\u{1F0AE}'
          (Spades, Ace) -> '\u{1F0A1}'

          (Hearts, Numeric 2) -> '\u{1F0B2}'
          (Hearts, Numeric 3) -> '\u{1F0B3}'
          (Hearts, Numeric 4) -> '\u{1F0B4}'
          (Hearts, Numeric 5) -> '\u{1F0B5}'
          (Hearts, Numeric 6) -> '\u{1F0B6}'
          (Hearts, Numeric 7) -> '\u{1F0B7}'
          (Hearts, Numeric 8) -> '\u{1F0B8}'
          (Hearts, Numeric 9) -> '\u{1F0B9}'
          (Hearts, Numeric 10) -> '\u{1F0BA}'
          (Hearts, Jack) -> '\u{1F0BB}'
          (Hearts, Queen) -> '\u{1F0BD}'
          (Hearts, King) -> '\u{1F0BE}'
          (Hearts, Ace) -> '\u{1F0B1}'

          (Diamonds, Numeric 2) -> '\u{1F0C2}'
          (Diamonds, Numeric 3) -> '\u{1F0C3}'
          (Diamonds, Numeric 4) -> '\u{1F0C4}'
          (Diamonds, Numeric 5) -> '\u{1F0C5}'
          (Diamonds, Numeric 6) -> '\u{1F0C6}'
          (Diamonds, Numeric 7) -> '\u{1F0C7}'
          (Diamonds, Numeric 8) -> '\u{1F0C8}'
          (Diamonds, Numeric 9) -> '\u{1F0C9}'
          (Diamonds, Numeric 10) -> '\u{1F0CA}'
          (Diamonds, Jack) -> '\u{1F0CB}'
          (Diamonds, Queen) -> '\u{1F0CD}'
          (Diamonds, King) -> '\u{1F0CE}'
          (Diamonds, Ace) -> '\u{1F0C1}'

          (Clubs, Numeric 2) -> '\u{1F0D2}'
          (Clubs, Numeric 3) -> '\u{1F0D3}'
          (Clubs, Numeric 4) -> '\u{1F0D4}'
          (Clubs, Numeric 5) -> '\u{1F0D5}'
          (Clubs, Numeric 6) -> '\u{1F0D6}'
          (Clubs, Numeric 7) -> '\u{1F0D7}'
          (Clubs, Numeric 8) -> '\u{1F0D8}'
          (Clubs, Numeric 9) -> '\u{1F0D9}'
          (Clubs, Numeric 10) -> '\u{1F0DA}'
          (Clubs, Jack) -> '\u{1F0DB}'
          (Clubs, Queen) -> '\u{1F0DD}'
          (Clubs, King) -> '\u{1F0DE}'
          (Clubs, Ace) -> '\u{1F0D1}'
          _ -> Debug.todo "Crash")

-- |pretty-print list of cards
prettyCards : List Card -> String
prettyCards cards =
  case cards of
    [] -> ""
    [x] -> prettyCard x
    (x::xs) -> prettyCard x ++ " and\n" ++ prettyCards xs

type alias PlayerId = String
type alias PlayerName = String
type alias Player = { id : PlayerId, name : PlayerName }

type alias PlayerStacks = Dict PlayerId (List Card)
type alias PlayerHands  = Dict PlayerId Hand

type alias Trick = List (Player, Card)

emptyTrick : Trick
emptyTrick = []

type alias GameState =
  { players : List Player
  , hands : PlayerHands
  , stacks : PlayerStacks
  , trick : Trick
  , winner : Maybe Player
  }

emptyPlayerHands : PlayerHands
emptyPlayerHands = Dict.empty

emptyPlayerStacks : List Player -> PlayerStacks
emptyPlayerStacks players =
    Dict.fromList (List.map (\ player -> (player.id, [])) players)

emptyGameState : List Player -> GameState
emptyGameState players =
  { players = players
  , hands = emptyPlayerHands
  , stacks = emptyPlayerStacks players
  , trick = emptyTrick
  , winner = Nothing
  }

type GameEvent =
    HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardPlayed Player Card
  | GameEnded Player

type GameCommand =
    DealHands PlayerHands
  | PlayCard Player Card

processGameEvent : GameEvent -> GameState -> GameState
-- processGameEvent event state | trace ("processGameEvent " ++ show state ++ " " ++ show event) False = undefined
processGameEvent event state =
  case event of
    HandDealt player hand ->
      { state | hands = Dict.insert player.id hand state.hands,
                trick = emptyTrick }
-- exercise: leave out cases
    PlayerTurnChanged player ->
      { state | players = rotateTo player state.players }
    LegalCardPlayed player card ->
      { state | players = rotate (rotateTo player state.players),
                hands = takeCard state.hands player card,
                stacks = state.stacks,
                trick = addToTrick player card state.trick }
    TrickTaken player trick ->
      { state | stacks = addToStack state.stacks player (cardsOfTrick trick),
                trick = emptyTrick }
    IllegalCardPlayed player card -> state
    GameEnded player ->
      { state | winner = Just player }

-- |rotate assumes length of input > 0
rotate : List a -> List a
rotate l =
  case l of
    (x :: xs) -> xs ++ [x]
    [] -> Debug.todo "bug"

-- |rotateTo assumes target exists in input of length > 0
rotateTo : a -> List a -> List a
rotateTo y xs =
  case xs of
    x :: xsR -> if x == y
                then xs
                else rotateTo y (xsR ++ [x])
    [] -> Debug.todo "bug"

takeCard : PlayerHands -> Player -> Card -> PlayerHands
takeCard playerHand player card =
  Dict.update player.id (Maybe.map (removeCard card)) playerHand

addToTrick : Player -> Card -> Trick -> Trick
addToTrick player card trick = (player, card) :: trick

addToStack : PlayerStacks -> Player -> List Card -> PlayerStacks
addToStack playerStacks player cards =
  let updateStack m = Maybe.map (\ stack -> stack ++ cards) m
  in Dict.update player.id updateStack playerStacks

cardsOfTrick : Trick -> List Card
cardsOfTrick trick = List.map (\ (_, card) -> card) trick
