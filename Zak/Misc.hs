-- |
-- Module: Zak.Misc
--
-- Utility functions for handling the ZakStunts sample data.
module Zak.Misc
    ( toZakLabel
    ) where

import Types

-- | Converts a one-based race index to the corresponding ZakStunts track
-- name.
toZakLabel :: RaceIx -> String
toZakLabel ri
    | ri > 19 = "C" ++ show (ri - 3)
    | otherwise = case ri of
        15 -> "P1"
        16 -> "C15"
        17 -> "P2"
        18 -> "C16"
        19 -> "P3"
        _ -> "C" ++ show ri

