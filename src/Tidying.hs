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
    , isKeptByPP
    , isKeptRating
    , distillRatings
    , distillRatingsAssocList
    ) where

import Types

import Data.Default.Class
import qualified Data.List.NonEmpty as N
import qualified Data.Map.Strict as Map
import Control.Comonad

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
    { activityCut :: Maybe Int       -- ^ If defined, specifies that a player
                                     -- must have competed at least once
                                     -- within the specified number of recent
                                     -- events in order to be included in the
                                     -- final results.
    , provisionalCut :: Maybe Int    -- ^ If defined, specifies that a player
                                     -- must have competed in at least the
                                     -- specified number of events to be
                                     -- included in the final results. Note
                                     -- that this cutoff is strictly
                                     -- presentational and is not related to
                                     -- the provisional modulation parameters
                                     -- defined through 'EloOptions'.
    , selectedRace :: Maybe RaceIx   -- ^ Picks a specific event for a single
                                     -- event query. The usual default is the
                                     -- most recent event.
    }

instance Default PostProcessOptions where
    def = PPOpts
        { activityCut = Nothing
        , provisionalCut = Just 5
        , selectedRace = Nothing
        }

-- | Should this data be retained according to the post-processing
-- criteria?
isKeptByPP
    :: (d -> RaceIx)       -- ^ Obtain last race index for the data.
    -> (d -> Int)          -- ^ Obtain total entries for the data.
    -> PostProcessOptions
    -> RaceIx              -- ^ Current event index.
    -> d
    -> Bool
isKeptByPP fDLri fDEntr ppopts ri d =
    maybe True (\ac -> ri - fDLri d < ac) (activityCut ppopts)
        && maybe True (\pc -> fDEntr d >= pc) (provisionalCut ppopts)

-- | Should this rating be retained according to the post-processing
-- criteria?
isKeptRating
    :: PostProcessOptions
    -> RaceIx              -- ^ Current event index.
    -> PipData
    -> Bool
isKeptRating = isKeptByPP lastRace entries

-- | Apply the post-processing criteria to filter ratings (association list
-- version).
distillRatingsAssocList
    :: PostProcessOptions
    -> AtRace [(p, PipData)]
    -> AtRace [(p, PipData)]
distillRatingsAssocList ppopts = extend $
    \(AtRace ri rtgs) -> filter (isKeptRating ppopts ri . snd) rtgs

-- | Apply the post-processing criteria to filter ratings (Map version).
distillRatings
    :: PostProcessOptions
    -> AtRace Ratings
    -> AtRace Ratings
distillRatings ppopts = extend $
    \(AtRace ri rtgs) ->  Map.filter (isKeptRating ppopts ri) rtgs

