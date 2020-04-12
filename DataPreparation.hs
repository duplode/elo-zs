-- | 
-- Module: DataPreparation
--
-- Helpers to clean up raw result data.
module DataPreparation 
    ( DataPreparationOptions(..)
    , ghostbuster
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

