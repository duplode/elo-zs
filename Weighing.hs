-- |
-- Module: Weighing
--
-- Various functions for weighing and computing scores. Though not
-- necessarily domain agnostic, they could conceivably be useful for multiple
-- purposes.
module Weighing
    ( -- * General purpose utility functions
      gaussian
    , witch
      -- * Weighing functions
    , fixedMidfieldWeight
    , altFixedMidfieldWeight
    , variableMidfieldWeight
      -- * Scoring functions
    , exponentialScoring
    ) where

import Types

-- | Scaled Gaussian.
gaussian
    :: Double  -- ^ x-scaling, as a specific distance from the center.
    -> Double  -- ^ Relative steepness (y-ratio at x-scaling from the peak).
    -> Double  -- ^ Peak location, center of the curve.
    -> Double  -- ^ x value.
    -> Double
gaussian d s c x = exp (-(x - c)^2 / w)
    where
    w = - d^2 / log s  -- sigma = - d / (2 * sqrt (ln s))

-- | Scaled witch of Agnesi.
witch
    :: Double  -- ^ x-scaling, as a specific distance from the center.
    -> Double  -- ^ Relative steepness (y-ratio at x-scaling from the peak).
    -> Double  -- ^ Peak location, center of the curve.
    -> Double  -- ^ x value.
    -> Double
witch d s c x = 1 / (b * (x - c)^2 + 1)
    where
    b = (1 - s) / (s * d^2)

-- | Midfield weighing through a gaussian centered at 6th place.
fixedMidfieldWeight
    :: Rank'  -- ^ Race standing.
    -> Double
fixedMidfieldWeight x = gaussian 5 (2/5) 6 x

-- | Midfield weighing through a witch of Agnesi centered at 6th place.
altFixedMidfieldWeight
    :: Rank'  -- ^ Race standing.
    -> Double
altFixedMidfieldWeight x = witch 6 (1/2) 6 x

-- | Midfield weighing through a gaussian centered at two fifths of the
-- scoreboard size. Not a particularly effective weighing curve.
variableMidfieldWeight
    :: Int    -- ^ Number of racers.
    -> Rank'  -- ^ Race standing.
    -> Double
variableMidfieldWeight n x = gaussian (peak - 1) (2/5) peak x
    where
    peak = (2 / 5) * fromIntegral n

-- | Scoring on an exponential curve: 24 points for the winner, 2 for the
-- 12th place. The ranks should already have had draw corrections applied to
-- them.
exponentialScoring :: Rank' -> Double
exponentialScoring rank
    = 24 * exp (-(log 12 / 11) * (rank - 1))
-- log 12 / 11 = 0.22590060452618185 ~ log (25 / 20)
-- log (25 / 18) = 0.32850406697203605
-- log (10 / 6) = 0.5108256237659907
-- log (9 / 6) = 0.4054651081081644

