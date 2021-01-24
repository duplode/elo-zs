-- |
-- Module: Analysis.PerfModel.Reference.Precompute
--
-- Known correct implementation of Analysis.PerfModel, for testing purposes.
-- Performs acceptably, much faster than the naive implementation, though not
-- quite as fast as the (significantly more complicated) 'CombRose'
-- hylomorphism implementation.
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
module Analysis.PerfModel.Reference.Precompute
    ( perfModelStrength
    , perfModelTopStrength
    , example
    ) where

import Analysis.PerfModel.Orbital
import Engine (initialRating)
import qualified Analysis.PerfModel.Reference.Naive as R
import qualified Util.Combinations as Util

import qualified Numeric.Integration.TanhSinh as Integration

import Data.Profunctor
import qualified Control.Foldl as L
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
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
        Integration.nonNegative Integration.trap (raceWinPDF rs initialRating)


-- | Like 'raceWinPDF', but for an arbitrary position on the scoreboard.
positionPDF :: Int -> [Double] -> Double -> Double -> Double
positionPDF pos rs r t = orbitalPDF (kFromRating r) t
    * L.fold L.sum (L.fold L.product . toFactors <$> combos)
    where
    n = length rs
    combos = Util.combinations n (pos-1) [1..n]
    loseVersus r' = orbitalCDF (kFromRating r') t
    -- winVersus r' = 1 - loseVersus r'
    lvs = IntMap.fromList (zip [1..n] (loseVersus <$> rs))
    wvs = (1 -) <$> lvs
    toFactors (above, below) = map (lvs !) above ++ map (wvs !) below

-- | A variant of 'positionPDF' that takes precomputed probabilities.
positionPDFPre :: Int                            -- ^ Target position.
               -> (IntMap Double, IntMap Double) -- ^ Win and loss probabilities
                                                 -- (given a probe laptime) for
                                                 -- for each racer on the
                                                 -- scoreboard.
               -> Double                         -- ^ Probability density of the
                                                 -- probe laptime.
               -> Double
positionPDFPre pos (lvs, wvs) p =
    p * L.fold L.sum (L.fold L.product . toFactors <$> combos)
    where
    n = IntMap.size lvs
    combos = Util.combinations n (pos-1) [1..n]
    toFactors (above, below) = map (lvs !) above ++ map (wvs !) below


-- | Like 'positionPDF', but for a top-N finish.
topPDF :: Int -> [Double] -> Double -> Double -> Double
topPDF pos rs r t = L.fold (lmap (posPDF t) L.sum) validPositions
    where
    validPositions = zipWith const [1..pos] (r : rs)
    loseVersus r' = orbitalCDF (kFromRating r') t
    lvs = IntMap.fromList (zip [1..] (loseVersus <$> rs))
    wvs = (1 -) <$> lvs
    probeDensity = orbitalPDF (kFromRating r) t
    posPDF t i = positionPDFPre i (lvs, wvs) probeDensity
    -- posPDF t i = positionPDF i rs r t

-- | Like 'perfModelStrength', but for a top-N finish. Note that the
-- computational cost grows quickly with the length of the list of ratings.
perfModelTopStrength :: Int -> [Double] -> Double
perfModelTopStrength pos rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (topPDF pos rs initialRating)


example :: [Double]
example = [2200,2100,1900,1870,1850,1600]
-- example = [2200,2100,1900,1870,1850,1600,1590,1570,1510,1420,1370,1350,1225]

-- >$> perfModelStrength [2200,2100,1900,1870,1850]
--
-- >$> :set +s
--
-- >$> perfModelTopStrength 5 example == R.perfModelTopStrength 5 example

-- >$> perfModelTopStrength 5 example

