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

import Data.Default

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
-- "Ghost" is Stunts community jargon for an additional entry by one person
-- in a single race, which is the most common reason for a result being
-- excluded from the race scoreboard.
ghostbuster :: [PipId] -> [Standing] -> [Standing]
ghostbuster ghosts = filter ((`notElem` ghosts) . pipsqueakTag)

data PostProcessOptions = PPOpts
    { activityCut :: Maybe Int
    , selectedRace :: Maybe Int
    , excludeProvisional :: Bool
    }

instance Default PostProcessOptions where
    def = PPOpts
        { activityCut = Nothing
        , selectedRace = Nothing
        , excludeProvisional = False
        }

