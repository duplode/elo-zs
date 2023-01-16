-- |
-- Module: R4K.Misc
--
-- Utility functions for handling the R4K sample data.
module R4K.Misc
    ( toR4KLabel
    ) where

import Types

-- | Converts a one-based race index to the corresponding R4K track
-- name.
toR4KLabel :: RaceIx -> String
-- TODO: Make this more DRY-y, give two digits to the offset indexes.
toR4KLabel ri
    | ri > 21 = "2021-" ++ show (ri - 21)
    | ri > 15 = "2020-" ++ show (ri - 15)
    | ri > 6 = "2019-" ++ show (ri - 6)
    | ri > 3 = "2018-" ++ show (ri - 3)
    | otherwise = "2017-" ++ show ri
