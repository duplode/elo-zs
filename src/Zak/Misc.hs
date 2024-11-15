-- |
-- Module: Zak.Misc
--
-- Utility functions for handling the ZakStunts sample data.
module Zak.Misc
    ( toZakLabel
    , toZakPlotIx
    ) where

import Types

-- | Description of a ZakStunts track for labelling purposes.
data ZakDescr = P1 | P2 | P3 | C RaceIx
    deriving Show

-- | Converts a one-based race index to the corresponding ZakStunts track
-- description.
toZakDescr :: RaceIx -> ZakDescr
toZakDescr ri
    | ri > 19 = C (ri - 3)
    | otherwise = case ri of
        15 -> P1
        16 -> C 15
        17 -> P2
        18 -> C 16
        19 -> P3
        _ -> C ri

-- | Converts a one-based race index to the corresponding ZakStunts track
-- name.
toZakLabel :: RaceIx -> String
toZakLabel ri = case (toZakDescr ri) of
    C n -> "C" ++ show n
    P1 -> "P1"
    P2 -> "P2"
    P3 -> "P3"

-- | Converts a one-based race index to a fractional "index". The point of
-- this function is providing x-values for plotting that match the actual
-- ZCT numbers of the tracks.
toZakPlotIx :: RaceIx -> Double
toZakPlotIx ri = case (toZakDescr ri) of
    C n -> fromIntegral n
    P1 -> 14.5
    P2 -> 15.5
    P3 -> 16.5

