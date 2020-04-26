-- |
-- Module: Tidying
--
-- Helpers and settings to clean up input and output data.
module Tidying
    ( -- * Data preparation
      DataPreparationOptions(..)
    , ghostbuster
      -- * Result post-processing
    , PostProcessOptions(..)
    ) where

import Types

import Data.Default.Class

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
ghostbuster :: [PipId] -> [Standing] -> [Standing]
ghostbuster ghosts = filter ((`notElem` ghosts) . pipsqueakTag)

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

