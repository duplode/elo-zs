-- | 
-- Module: Types
--
-- Shared domain types.
module Types 
    ( PipId
    , Ratings
    , Result(..)
    , PipData(..)
    , Standing
    , RaceIx
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | Player data which is updated through the Elo ranking computation. 
data PipData = PipData
    { rating :: !Double     -- ^ Elo rating.
    , entries :: !Int       -- ^ Total number of events the player entered.
    , lastRace :: !RaceIx   -- ^ Index of the last event the player entered.
    }
    deriving (Eq, Ord, Show)

-- | Result attained by a player in an event.
--
-- This type is necessary to handle events with more than two players, such as
-- races.
data Result p a = Result 
    { pipsqueakTag :: !p  -- ^ Player identifier.
                          -- "pipsqueak" means racer in the Stunts community
                          -- jargon.
    , result :: !a        -- ^ Attained result.
    }
    deriving (Eq, Ord, Show)

-- | Concrete identifier for the racers/players being ranked.
type PipId = T.Text

-- | Key-value store of player data for retrieval and update.
type Ratings = Map.Map PipId PipData

-- | Concrete race result. 
type Standing = Result PipId Int

-- | Concrete index for locating and identifying races.
type RaceIx = Int