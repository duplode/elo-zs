{-# LANGUAGE LambdaCase #-}
-- |
-- Module: Analysis.PerfModel
--
-- An experimental approach to obtaining strengths, which takes Elo (or
-- Elo-like) ratings as derived from a performance model. With a performance
-- model in hand, we can use https://stats.stackexchange.com/a/44142 to
-- obtain the victory probability of a 1500-rated racer given a field of
-- racers, and use that as a measure of strength.
--
-- According to the model, player performances follow a gamma distribution,
-- with the outcome 0 corresponding to an ideal performance. The shape
-- parameter of the distribution is fixed, while the rate parameter varies
-- across players. A higher rate parameter amounts to results that are both
-- more consistent and closer to the ideal. (Pinning both mean and standard
-- deviation of player performance to a single parameter is the main
-- simplifying assumption of the model.)
--
-- Note that the performance model winning probabilities for pairs of players
-- agree exactly with the conventional Elo formula if the shape parameter of
-- the gamma distribution is taken to be 1.
module Analysis.PerfModel
    ( perfModelStrength
    , perfModelTopStrength
    , example
    ) where

import Orbital (orbitalDistr)
import Engine (initialRating)
import qualified Util.Combinations as Util

import qualified Numeric.Integration.TanhSinh as Integration
import Statistics.Distribution

import Data.Profunctor
import qualified Control.Foldl as L
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Control.Foldl as L

-- | PDF for a race victory against a field of racers.
raceWinPDF
    :: Int       -- ^ Gamma shape.
    -> [Double]  -- ^ Ratings of the field of opponents.
    -> Double    -- ^ Rating of the racer.
    -> Double    -- ^ Performance.
    -> Double
raceWinPDF gsh rs r t = density (orbitalDistr gsh r) t
    * L.fold (lmap winVersus L.product) rs
    where
    winVersus r' = 1 - cumulative (orbitalDistr gsh r') t

-- | Integrating the race win PDF gives the likelihood of victory, whose
-- reciprocal we use as a strength metric.
perfModelStrength
    :: Int       -- ^ Gamma shape.
    -> [Double]  -- ^ Ratings of the field of opponents.
    -> Double
perfModelStrength gsh rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (raceWinPDF gsh rs initialRating)

-- TODO: positionPDF and positionPDFPre are still the same as in
-- Analysis.PerfModel.Reference.Precompute . Conside redefining them so that
-- topPDF can share code with them.

-- | Like 'raceWinPDF', but for an arbitrary position on the scoreboard.
positionPDF
    :: Int       -- ^ Gamma shape.
    -> Int       -- ^ Target position.
    -> [Double]  -- ^ Ratings of the field of opponents.
    -> Double    -- ^ Rating of the racer.
    -> Double    -- ^ Performance.
    -> Double
positionPDF gsh pos rs r t = density (orbitalDistr gsh r) t
    * L.fold L.sum (L.fold L.product . toFactors <$> combos)
    where
    n = length rs
    combos = Util.combinations n (pos-1) [1..n]
    loseVersus r' = cumulative (orbitalDistr gsh r') t
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
topPDF
    :: Int       -- ^ Gamma shape.
    -> Int       -- ^ Target position.
    -> [Double]  -- ^ Ratings of the field of opponents.
    -> Double    -- ^ Rating of the racer.
    -> Double    -- ^ Performance.
    -> Double
topPDF gsh pos rs r t = (probeDensity *) . L.fold L.sum $
    L.fold L.sum . partials <$> validPositions
    where
    validPositions = zipWith const [1..pos] (r : rs)
    loseVersus r' = cumulative (orbitalDistr gsh r') t
    lvs = IntMap.fromList (zip [1..] (loseVersus <$> rs))
    wvs = (1 -) <$> lvs
    probeDensity = density (orbitalDistr gsh r) t
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
perfModelTopStrength
    :: Int       -- ^ Gamma shape.
    -> Int       -- ^ Target position.
    -> [Double]  -- ^ Ratings of the field of racers.
    -> Double
perfModelTopStrength gsh pos rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (topPDF gsh pos rs initialRating)


example :: [Double]
example = [2200,2100,1900,1870,1850,1600]
--example = [2200,2100,1900,1870,1850,1600,1590,1570,1510,1420,1370,1350,1225]

-- >$> perfModelStrength [2200,2100,1900,1870,1850]
--
-- >$> :set +s
--

-- >$> perfModelTopStrength 5 example

