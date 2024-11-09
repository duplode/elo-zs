--{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Analysis.Extra
--
-- Experimental or superseded analytics.
module Analysis.Extra where

import Types
import Tidying
import Engine
import Weighing
import Analysis.Common (foldRatingsPerRace, distillRatings)

import qualified Data.Map.Strict as Map
import qualified Control.Foldl as L
import qualified Control.Scanl as LS
import qualified Data.List.NonEmpty as N
import Data.Profunctor
import Control.Arrow
import Control.Comonad
import Data.Default.Class

-- | Prototype for scoring weighed by strength.
simpleWeighedScores
    :: EloOptions
    -> LS.Scan
        (N.NonEmpty (Result PipId Rank'))
        (AtRace (N.NonEmpty (Result PipId Rank')))
simpleWeighedScores eopts
    = (\str rs -> AtRace (raceIx str)
        $ fmap @(Result _) (extract str *) <$> rs)
            <$> strength <*> basePoints
    where
    basePoints = arr computeScores
    computeScores = fmap (fmap @(Result _) exponentialScoring)
    strength = fmap @AtRace strengthConversion
        . accumulatedRatings
        . distillRatings def {provisionalCut=Nothing}
        <$> allRatings eopts
    strengthConversion = logisticStrengthConversion
    -- Just to avoid importing Analysis here
    accumulatedRatings = foldRatingsPerRace (lmap rating L.sum)

-- | Race strength on a half-logistic slope, with an accumulated rating of
-- 15000 amounting to a 0.5 factor.
logisticStrengthConversion :: Double -> Double
logisticStrengthConversion s = tanh ((log 3 / 15000) * s / 2)
-- logisticStrengthConversion s = 2 / (1 + exp (-(log 3 / 15000) * s)) - 1

-- | Race strength on a linear slope, with an accumulated rating of 45000
-- amounting to a 1.0 factor.
linearStrengthConversion s = s / 45000

-- | A race strength metric obtained by giving extra weight to the strength
-- of racers in the scoreboard midfield. Uses 'fixedMidfieldWeight' as
-- weighing function.
weighedStrength
    :: EloOptions
    -> LS.Scan (N.NonEmpty (Result PipId Rank')) (AtRace Rank')
weighedStrength eopts = id  -- fmap @AtRace strengthConversion
    . accumulatedRatings
    . distillRatings def {provisionalCut=Nothing}
    <$> (fmap @AtRace . preWeighing <$> returnA <*> allRatings eopts)
    where
    -- Variable weighing is very similar to not having midfield weighing at
    -- all.
    -- preStrengthWeight = variableMidfieldWeight
    preStrengthWeight = const fixedMidfieldWeight
    preWeighing ress
        = foldr (\(Result p n) f ->
            f . Map.adjust
                (\pipData ->
                    pipData
                        { rating = rating pipData
                            * preStrengthWeight (length ress) n})
            p) id ress
    -- Just to avoid importing Analysis here
    accumulatedRatings = foldRatingsPerRace (lmap rating L.sum)

-- | Assigns points to race results, as in a regular season scoreboard,
-- but using 'weighedStrength' as weight for the score of each race. Uses
-- 'exponentialScoring' as the base score system.
weighedScores
    :: EloOptions
    -> LS.Scan
        (N.NonEmpty (Result PipId Rank'))
        (AtRace (N.NonEmpty (Result PipId Double)))
weighedScores eopts
    = (\str rs -> AtRace (raceIx str)
        $ fmap @(Result _) (extract str *) <$> rs)
            <$> weighedStrength eopts <*> basePoints
    where
    basePoints = arr computeScores
    computeScores = fmap (fmap @(Result _) exponentialScoring)


-- | Race strength as the logarithm of the reciprocal of the probability of
-- an hypothetical extra racer (the "probe") defeating every other racer in
-- independent head-to-head matches. Behaves reasonably well as a strength
-- metric, even though the independent matches scenario doesn't really
-- reflect what goes on in a race. The reciprocal of the probabilities is
-- obtained through 'reinvertedRatings'.
reinvertedStrength
    :: EloOptions
    -> LS.Scan (N.NonEmpty (Result PipId Rank')) (AtRace Double)
reinvertedStrength eopts = id  -- fmap @AtRace strengthConversion
    . fmap (logBase 10)
    . reinvertedRatings
    . distillRatings def {provisionalCut=Nothing}
    <$> allRatings eopts

-- | Reciprocal of the probability of a 1500-rated probe-racer (see also
-- 'reinvertedStrength') winning independent head-to-head matches against
-- every racer in a race.
reinvertedRatings :: AtRace Ratings -> AtRace Double
reinvertedRatings
    = foldRatingsPerRace (lmap (invExpected . rating) L.product)
    where
    invExpected r = 1 + 10**((r - r0)/400)
    r0 = 1500
