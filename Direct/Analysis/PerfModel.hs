{-# LANGUAGE LambdaCase #-}
-- |
-- Module: Direct.Analysis.PerfModel
--
-- Lixe 'Analysis.PerfModel', except we use the gamma parameters directly as
-- ratings.
module Direct.Analysis.PerfModel
    ( perfModelStrength
    , perfModelTopStrength
    , example
    ) where

import Engine (initialRating)
import Analysis.PerfModel.Orbital
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
raceWinPDF rs r t = orbitalPDF r t
    * L.fold (lmap winVersus L.product) rs
    where
    winVersus r' = 1 - orbitalCDF r' t

-- | Integrating the race win PDF gives the likelihood of victory, whose
-- reciprocal we use as a strength metric.
perfModelStrength :: [Double] -> Double
perfModelStrength rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (raceWinPDF rs initialRating)

-- TODO: positionPDF and positionPDFPre are still the same as in
-- Analysis.PerfModel.Reference.Precompute . Conside redefining them so that
-- topPDF can share code with them.

-- | Like 'raceWinPDF', but for an arbitrary position on the scoreboard.
positionPDF :: Int -> [Double] -> Double -> Double -> Double
positionPDF pos rs r t = orbitalPDF r t
    * L.fold L.sum (L.fold L.product . toFactors <$> combos)
    where
    n = length rs
    combos = Util.combinations n (pos-1) [1..n]
    loseVersus r' = orbitalCDF r' t
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
topPDF pos rs r t = (probeDensity *) . L.fold L.sum $
    L.fold L.sum . partials <$> validPositions
    where
    validPositions = zipWith const [1..pos] (r : rs)
    loseVersus r' = orbitalCDF r' t
    lvs = IntMap.fromList (zip [1..] (loseVersus <$> rs))
    wvs = (1 -) <$> lvs
    probeDensity = orbitalPDF r t
    partials = Util.processCombsInt alg (length rs) . subtract 1
    alg = \case
        Util.LeafF Nothing -> 0
        Util.LeafF (Just rest) ->
            L.fold L.product ((wvs !) <$> IntSet.toList rest)
        Util.FlowerF a rest -> (lvs ! a)
            * L.fold L.product ((wvs !) <$> IntSet.toList rest)
        Util.BranchF a bs -> (lvs ! a) * L.fold L.sum bs

-- | Like 'perfModelStrength', but for a top-N finish. Note that the
-- computational cost grows quickly with the length of the list of ratings.
perfModelTopStrength :: Int -> [Double] -> Double
perfModelTopStrength pos rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (topPDF pos rs initialRating)


example :: [Double]
example = kFromRating <$> [2200,2100,1900,1870,1850,1600]
--example = kFromRating
--  <$> [2200,2100,1900,1870,1850,1600,1590,1570,1510,1420,1370,1350,1225]

-- >$> :set +s
--
-- >$> perfModelTopStrength 5 example

