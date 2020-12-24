-- |
-- Module: Analysis.PerfModel
--
-- An experimental, left-field approach to obtaining strengths:
--
-- 1. Assume time results for racers fit a distribution with PDF
--    p = 4 * k^3 * t^2 * exp (-2*k*t) , with t ranging from 0 to positive
--    infinity, as defined in Analysis.PerfModel.Orbital .
--
-- 2. Calculate victory probabilities in a match from each racer's k
--     parameter.
--
-- 3. For given values of k and a fixed opponent, assume victory
--    probabilities from the Elo ratings and from the distribution coincide
--    and accordingly calculate the rating that corresponds to k. (The fixed
--    opponent is taken to have 1800 rating and an empirically chosen value of
--    k).
--
-- 4. Use interpolation to invert the function from the previous step, thus
--     obtaing a way of finding the k that corresponds to a rating.
--
-- 5. Use https://stats.stackexchange.com/a/44142 to obtain the victory
--    probability of a 1500-rated racer given a field of racers, and use that
--    as a measure of strength.
--
-- In the PDF, t = 0 is taken to be the ideal laptime in a track. The PDF
-- rises from zero at t = 0, reaches a peak and then tapers off asymptotically
-- towards zero. Higher k values mean an earlier peak, and thus laptimes
-- typically closer to the ideal, and also a narrower distribution, and thus
-- more consistent laptimes. It is a very simple model, but one that makes at
-- least some sense in the context of its application and is quite tractable
-- (notably, both the CDF and the probabilities in step 2 can be worked out
-- analytically).
module Analysis.PerfModel
    ( perfModelStrength
    ) where

import Analysis.PerfModel.Orbital

import qualified Numeric.Interpolation.NodeList as NodeList
import qualified Numeric.Interpolation.Type as Interpolation.Type
import qualified Numeric.Interpolation.Piecewise as Piecewise
import qualified Numeric.Integration.TanhSinh as Integration

import Data.Profunctor
import qualified Control.Foldl as L

-- | See step 5 of the introduction.
raceWinPDF :: [Double] -> Double -> Double -> Double
raceWinPDF rs r t = orbitalPDF (kFromRating r) t
    * L.fold (lmap winVersus L.product) rs
    where
    winVersus r' = 1 - orbitalCDF (kFromRating r') t

-- | Integrating the race win PDF gives the likelihood of victory, whose
-- reciprocal we use as a strength metric.
perfModelStrength :: [Double] -> Double
perfModelStrength rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (raceWinPDF rs 1500)

-- $> perfModelStrength [2200,2100,1900,1870,1850]
