{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
{-# LANGUAGE LambdaCase, BlockArguments, TypeApplications, ScopedTypeVariables, ConstraintKinds #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Sync where

import Debug.Trace (trace)

import qualified Control.Monad as Monad

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Foldable as Foldable

import Cards
import Game
import Player
import EventSourcing
import Shuffle

import Polysemy
import qualified Polysemy.State as State
import Polysemy.State (State)

import qualified Teletype
import Teletype (Teletype)

-- synchroner Satz Spieler
type Players effects = Map Player (Strategy effects)

emptyPlayers :: Players effects
emptyPlayers = Map.empty

-- Spieler zu Spielersatz hinzufügen

addPlayer :: Players effects -> Player -> Strategy effects -> Players effects
addPlayer players player strategy =
  Map.insert player strategy players

mike = Player "1" "Mike"
bianca = Player "2" "Bianca"

strategy1 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Bianca"), State (PlayerState "Fredo"), State (PlayerState "Connie"), Teletype]
strategy1 = interactiveStrategy @"Mike"
strategy2 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Bianca"), State (PlayerState "Fredo"), State (PlayerState "Connie"), Teletype]
strategy2 = interactiveStrategy @"Bianca"
strategy3 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Bianca"), State (PlayerState "Fredo"), State (PlayerState "Connie"), Teletype]
strategy3 = interactiveStrategy @"Fredo"
strategy4 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Bianca"), State (PlayerState "Fredo"), State (PlayerState "Connie"), Teletype]
strategy4 = interactiveStrategy @"Connie"

event :: GameEvent
event = undefined

both =
  do commands1 <- strategy1 mike event
     commands2 <- strategy2 bianca event
     return ()


-- ein Event von den Spielern verarbeiten lassen
playEvent :: Players effects -> GameEvent -> Sem effects [GameCommand]
-- playEvent players gameEvent | trace ("playEvent " ++ show gameEvent) False = undefined
playEvent players gameEvent =
  Monad.foldM (\ gameCommands (player, playerStrategy) ->
                do gameCommands' <- playerStrategy player gameEvent
                   return (gameCommands ++ gameCommands'))
              []
              (Map.toList players)


-- Befehle ausführen bis zum bitteren Ende
playCommand :: Players effects -> GameCommand -> Sem (GameEventSourcing ': effects) ()
playCommand players gameCommand | trace ("playCommand " ++ show gameCommand) False = undefined
playCommand players gameCommand =
  do events <- gameCommandEventsM gameCommand
     gameOver <- gameOverM
     if gameOver
     then return ()
     else
       do gameCommandss <- raise (mapM (\ gameEvent -> playEvent players gameEvent) events)
          let gameCommands = Monad.join gameCommandss
          mapM_ (playCommand players) gameCommands
          return ()

-- das Spiel spielen
playGame :: Players effects -> [Card] -> Sem (GameEventSourcing ': effects) ()
playGame players shuffledCards = do
  let playerList = Map.keys players
      hands = Map.fromList (zip playerList (map Set.fromList (distribute (length playerList) shuffledCards)))
  playCommand players (DealHands hands)


-- Spiel mit automatischen Spielern spielen
gameAlong :: IO [GameEvent]
gameAlong =
  do let player1 = Player "1" "Mike"
         strategy1 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette")]
         strategy1 = alongStrategy @"Mike"
         player2 = Player "2" "Peter"
         strategy2 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette")]
         strategy2 = alongStrategy @"Peter"
         player3 = Player "3" "Nicole"
         strategy3 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette")]
         strategy3 = alongStrategy @"Nicole"
         player4 = Player "4" "Annette"
         strategy4 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette")]
         strategy4 = alongStrategy @"Annette"
     let players1 = addPlayer emptyPlayers player1 strategy1
         players2 = addPlayer players1 player2 strategy2
         players3 = addPlayer players2 player3 strategy3
         players4 = addPlayer players3 player4 strategy4
     let playerNames = Map.keys players4
     shuffledDeck <- shuffle deck
     let rwt = runEventSourcing (playGame players4 shuffledDeck) (emptyGameState playerNames)
     let (playerState4, (playerState3, (playerState2, (playerState1, ((), events, gameState))))) =
           run (State.runState emptyPlayerState (State.runState emptyPlayerState (State.runState emptyPlayerState (State.runState emptyPlayerState rwt))))
     return events

gameInteractive :: IO ()
gameInteractive =
  do let player1 = Player "1" "Mike"
         strategy1 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette"), Teletype]
         strategy1 = interactiveStrategy @"Mike"
         player2 = Player "2" "Peter"
         strategy2 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette"), Teletype]
         strategy2 = interactiveStrategy @"Peter"
         player3 = Player "3" "Nicole"
         strategy3 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette"), Teletype]
         strategy3 = interactiveStrategy @"Nicole"
         player4 = Player "4" "Annette"
         strategy4 :: Strategy '[State (PlayerState "Mike"), State (PlayerState "Peter"), State (PlayerState "Nicole"), State (PlayerState "Annette"), Teletype]
         strategy4 = interactiveStrategy @"Annette"
     let players1 = addPlayer emptyPlayers player1 strategy1
         players2 = addPlayer players1 player2 strategy2
         players3 = addPlayer players2 player3 strategy3
         players4 = addPlayer players3 player4 strategy4
     let playerNames = Map.keys players4
     shuffledDeck <- shuffle deck
     let rwt = runEventSourcing (playGame players4 shuffledDeck) (emptyGameState playerNames)
     _ <- Teletype.runTeletype (State.runState emptyPlayerState (State.runState emptyPlayerState (State.runState emptyPlayerState (State.runState emptyPlayerState rwt))))
     return ()
