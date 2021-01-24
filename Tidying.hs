{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Tidying
--
-- Helpers and settings to clean up input and output data.
module Tidying
    ( -- * Data preparation
      DataPreparationOptions(..)
    , ghostbuster
    , tidyRanks
      -- * Result post-processing
    , PostProcessOptions(..)
    ) where

import Types

import Data.Default.Class
import qualified Data.List.NonEmpty as N

-- | Preferences for how raw data is to be cleaned up.
data DataPreparationOptions = DPOpts
    { removeGhosts :: Bool  -- ^ Should the removal of selected players from
                            -- the raw results be performed?
    }

instance Default DataPreparationOptions where
    def = DPOpts
        { removeGhosts = True
        }

-- | Removal of selected players from the raw results.
--
-- \"Ghost\" is Stunts community jargon for an additional entry by one person
-- in a single race, which is the most common reason for a result being
-- excluded from the race scoreboard.
ghostbuster :: [PipId] -> [Result PipId a] -> [Result PipId a]
ghostbuster ghosts = filter ((`notElem` ghosts) . pipsqueakTag)

-- | Tidies rank results of events by sorting the entries, ensuring there are
-- no gaps in the ranks, and adjusting draw ranks so they are equidistant
-- from the surrounding non-draw ranks.
tidyRanks :: N.NonEmpty (Result PipId Int) -> N.NonEmpty (Result PipId Rank')
tidyRanks =
    -- Adds the base position to the draw adjustments, and concatenates the
    -- groups.
    (=<<) (\(n, g) -> fmap @(Result _) (n+) <$> g)
    -- Accumulates the base positions by counting elements from the previous
    -- groups.
    . N.scanl1 (\(n, g) (_, g') -> (n + fromIntegral (length g), g'))
    -- Marks each group with its base position.
    . N.zip (N.fromList [1..])
    -- Replaces the reported result with a draw adjustment.
    . fmap (\g ->
        fmap @(Result _) (const (fromIntegral (length g - 1) / 2)) <$> g)
    . N.groupAllWith1 result

data PostProcessOptions = PPOpts
    { activityCut :: Maybe Int    -- ^ If defined, specifies that a player
                                  -- must have competed at least once within
                                  -- the specified number of recent events in
                                  -- order to be included in the final
                                  -- results.
    , selectedRace :: Maybe RaceIx   -- ^ Picks a specific event for a single
                                     -- event query. The usual default is the
                                     -- most recent event.
    , excludeProvisional :: Bool  -- ^ Should racers with provisional ratings
                                  -- be excluded from the final results?
    }

instance Default PostProcessOptions where
    def = PPOpts
        { activityCut = Nothing
        , selectedRace = Nothing
        , excludeProvisional = False
        }

