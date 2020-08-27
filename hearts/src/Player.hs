{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Player where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Cards
import Game

import Polysemy
import qualified Polysemy.State as State
import Polysemy.State (State)

import qualified Teletype
import Teletype (Teletype)

-- import Polysemy.Bundle

-- ... macht aus Events Commands
type EventProcessor effects event command = event -> Sem effects [command]

type Strategy effects = Player -> EventProcessor effects GameEvent GameCommand

type Chooser player effects =
  Player -> PlayerState player -> Sem effects Card

-- Das hier ist schade, weil es bedeutet, daß PlayerState player bereits
-- in den Effekten des Chooser stehen muß.
-- Aber sonst wird es später schwierig, die entstehenden Strategien
-- in einer Monade zu kombinieren.
chooserStrategy ::
  Member (State (PlayerState player)) effects => Chooser player effects -> Strategy effects
chooserStrategy choose =
  \ player event ->
    do playerProcessGameEventM player event
       playerState <- State.get
       case event of
         HandDealt player' hand ->
           if (player == player') && (containsCard twoOfClubs hand)
           then return [PlayCard player twoOfClubs]
           else return []
         PlayerTurnChanged player' ->
           if player == player'
           then do card <- choose player playerState
                   return [PlayCard player card]
           else return []
         LegalCardPlayed player' card -> return []
         IllegalCardPlayed player' card -> return []
         TrickTaken player' trick -> return []
         GameEnded winner -> return []

-- Strategie für Roboterspieler
chooseAlong :: Chooser player effects
chooseAlong _ playerState =
  case playerStateTrick playerState of
    [] -> return (Set.findMin (playerStateHand playerState))       -- leine erste Karte
    trick ->
      let hand = playerStateHand playerState
          (_, firstCard) = last trick
          firstSuit = suit firstCard
          followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
      in case Set.lookupMin followingCardsOnHand of
           Nothing ->
             return (Set.findMax hand) -- wir haben nix passendes, nimm große Karte
           Just card ->
             return card           -- sonst kleine passende

alongStrategy :: forall player effects . Member (State (PlayerState player)) effects => Strategy effects
alongStrategy player event = -- braucht Eta-Expansion
  chooserStrategy chooseAlong player event

-- newtype InteractiveEffect player monad a = InteractiveEffect (Bundle '[State (PlayerState player), Teletype] monad a)

-- interaktiver Spieler
chooseInteractive :: forall player effects . Member Teletype effects => Chooser player effects
chooseInteractive player playerState =
  do Teletype.writeTTY ("Your turn, player " ++ (playerName player))
     case playerStateTrick playerState of
       [] ->
         Teletype.writeTTY "You lead the next trick."
       trick ->
         Teletype.writeTTY ("Cards on table: " ++ show (reverse trick))
     let hand = playerStateHand playerState
         handList = Set.elems hand
         ncards = Set.size hand
     Teletype.writeTTY ("Your hand: " ++ prettyCards handList)
     Teletype.writeTTY ("Pick a card (1-" ++ show ncards ++ ")")
     selected <- getNumber (1,ncards)
     return (handList !! (selected - 1))

-- Zahl einlesen
getNumber :: (Num a, Ord a, Read a, Show a, Member Teletype effects) => (a, a) -> Sem effects a
getNumber (lo, hi) = do
  s <- Teletype.readTTY
  let input = read s
  if lo <= input && input <= hi
  then return input
  else
    do Teletype.writeTTY ("Input must be between " ++ (show lo) ++ " and " ++ (show hi) ++ ". Try again")
       getNumber (lo, hi)

interactiveStrategy :: forall player effects . (Member Teletype effects, Member (State (PlayerState player)) effects) => Strategy effects
interactiveStrategy player event =
  chooserStrategy chooseInteractive player event

-- Spieler kennt das, was er sieht
data PlayerState player =
  PlayerState { playerStateHand  :: Hand,
                playerStateTrick :: Trick,
                playerStateStack :: [Card] }
  deriving Show

emptyPlayerState = PlayerState emptyHand [] []

-- Spielevent "sehen"
playerProcessGameEvent :: Player -> GameEvent -> PlayerState player -> PlayerState player
playerProcessGameEvent player (HandDealt player' hand) state
  | player == player' =
    PlayerState { playerStateHand = hand,
                  playerStateTrick = emptyTrick,
                  playerStateStack = [] }
  | otherwise = state
playerProcessGameEvent player (PlayerTurnChanged player') state = state
playerProcessGameEvent player (LegalCardPlayed player' card) state
  | player' == player =
    state { playerStateHand = removeCard card (playerStateHand state),
            playerStateTrick = addToTrick player' card (playerStateTrick state) }
  | otherwise =
    state { playerStateTrick = addToTrick player' card (playerStateTrick state) }
playerProcessGameEvent player (TrickTaken player' trick) state
  | player' == player =
    state { playerStateTrick = emptyTrick,
            playerStateStack = (cardsOfTrick trick) ++ (playerStateStack state) }
  | otherwise =
    state { playerStateTrick = emptyTrick }
playerProcessGameEvent player (IllegalCardPlayed player' card) state = state
playerProcessGameEvent player (GameEnded winner) state = state

-- monadische Version
playerProcessGameEventM ::
  Member (State (PlayerState player)) effects => Player -> GameEvent -> Sem effects ()
playerProcessGameEventM player event =
  do playerState <- State.get
     let playerState' = playerProcessGameEvent player event playerState
     State.put playerState'

