{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- |
-- Module: Types
--
-- Shared domain types.
module Types (
    -- * Elementary synonyms
      PipId
    , RaceIx
    -- * Core algorithm types
    , PipData(..)
    , Ratings
    , Result(..)
    , Standing
    , WDL(..)
    , FaceOff(..)
    , FaceOff'
    -- * Race index tagging
    , AtRace(..)
    , raceIx
    , fromAtRace
    , toAtRace
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Comonad

-- | Concrete identifier for the racers/players being ranked.
type PipId = T.Text

-- | Concrete index for locating and identifying races.
type RaceIx = Int

-- | Player data which is updated through the Elo ranking computation.
data PipData = PipData
    { rating :: !Double     -- ^ Elo rating.
    , entries :: !Int       -- ^ Total number of events the player entered.
    , lastRace :: !RaceIx   -- ^ Index of the last event the player entered.
    }
    deriving (Eq, Ord, Show)

-- | Key-value store of player data for retrieval and update.
type Ratings = Map.Map PipId PipData

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

-- | Concrete race result.
type Standing = Result PipId Int

-- | Possible outcomes of a match between two players.
data WDL = Loss | Draw | Win
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Record of a match between two players.
data FaceOff p = FaceOff
    { player :: !p     -- ^ A player.
    , opponent :: !p   -- ^ Their opponent.
    , outcome :: !WDL  -- ^ The outcome, from the 'player''s point of view.
    }
    deriving (Eq, Ord, Show)

-- | Concrete match record type.
type FaceOff' = FaceOff PipId

-- | A value tagged with a race index. Uses include race identification and
-- application of player activity windows.
--
-- By convention, indexes corresponding to a race are positive integers. The
-- value '0' is reserved for tagging values from before the first race.
data AtRace a = AtRace !RaceIx a
    deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

-- | Gets the race index associated with a value.
--
-- To extract the value itself, use 'extract'.
raceIx :: AtRace a -> RaceIx
raceIx (AtRace ri _) = ri

-- | Conversion from a pair
toAtRace :: (RaceIx, a) -> AtRace a
toAtRace = uncurry AtRace

-- | Conversion from a pair
fromAtRace :: AtRace a -> (RaceIx, a)
fromAtRace (AtRace ri a) = (ri, a)

instance Comonad AtRace where
    extract (AtRace _ a) = a
    extend f ar@(AtRace ri _) = AtRace ri (f ar)
