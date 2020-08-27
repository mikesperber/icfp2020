{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Serialization where

import           Data.Aeson
import           GHC.Generics (Generic)

import           Cards        (Card (..), Rank (..), Suit (..))
import           Game         (GameCommand (..), GameEvent (..), Player (..))

-- Type-class instances needed for JSON serialization

deriving instance Generic GameCommand
instance FromJSON GameCommand
instance ToJSON GameCommand

deriving instance Generic Player
instance FromJSON Player
instance ToJSON Player
instance FromJSONKey Player
instance ToJSONKey Player

deriving instance Generic Card
instance FromJSON Card
instance ToJSON Card

deriving instance Generic Rank
instance FromJSON Rank
instance ToJSON Rank

deriving instance Generic Suit
instance FromJSON Suit
instance ToJSON Suit

deriving instance Generic GameEvent
instance FromJSON GameEvent
instance ToJSON GameEvent
