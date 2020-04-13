{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Zak
--
-- Demo functions that generate results from the ZakStunts sample data and
-- display them.
module Zak where

import Analysis
import Engine
import Types
import Tidying
import Tabular
import Zak.Misc
import Zak.Results

import qualified Data.Map.Strict as Map
import Data.Ord
import Data.List
import qualified Data.Text as T
import Control.Arrow
import Data.Default
import qualified Text.Tabular as Tab
import Data.Function ((&))
import Control.Comonad

-- >$> sortBy (comparing (Down . snd . snd)) $ Map.toList $ highestPerPip (allRatings (testData def))
demoHighest :: Tab.Table String String String
demoHighest = highestPerPip
        (distillRatings def {excludeProvisional=True}
            <$> allRatings (testData def))
    & Map.toList & sortBy (comparing (Down . extract . snd))
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Race", "Rating"]
        (\(p, AtRace ri rtg) -> [T.unpack p, toZakLabel ri, show rtg])

-- >$> sortBy (comparing (Down . snd)) $ accumulatedRatings (allRatings (testData def))
demoAccumulated :: Tab.Table String String String
demoAccumulated = accumulatedRatings
        (distillRatings def {excludeProvisional=False}
            <$> allRatings (testData def))
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Accumulated Rating"]
        ((:[]) . show . extract)
-- $> demoPretty demoAccumulated

-- >$> meanRatingPerRace (allRatings (testData def))
demoMean :: Tab.Table String String String
demoMean = meanRatingPerRace
        (distillRatings def {excludeProvisional=True}
            <$> allRatings (testData def))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

-- >$> windowLeaders (allRatings (testData def))
demoWindowLeaders :: Tab.Table String String String
demoWindowLeaders = windowLeaders
        (distillRatings def {activityCut=Just 12, excludeProvisional=True}
            <$> allRatings (testData def))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Racer", "Rating"]
        (maybe ["N/A", "N/A"] (\(p, x) -> [T.unpack p, show x]) . extract)

-- >$> meanRatingPerSnapshot (allRatings (testData def))
demoMeanSnap :: Tab.Table String String String
demoMeanSnap = meanRatingPerSnapshot
        (distillRatings def {activityCut=Just 12, excludeProvisional=True}
            <$> allRatings (testData def))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

testPersonalHistory :: PipId -> [AtRace Double]
testPersonalHistory p = personalHistory p (allRatings (testData def))

-- This might cause trouble with many rows. Rendering the race labels as a
-- normal column would likely help.
demoPersonalHistory :: PipId -> Tab.Table String String String
demoPersonalHistory p = testPersonalHistory p
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        [T.unpack p]
        ((:[]) . show . extract)

-- | Sorted racer-rating pairs for a specific race (defined through
-- 'PostProcessOptions.selectedRace', defaults to the latest race).
runTest :: DataPreparationOptions -> PostProcessOptions -> [(PipId, Double)]
runTest dpopts ppopts = allRatings (testData dpopts)
    & maybe last (flip (!!)) (selectedRace ppopts)
    & fmap @AtRace Map.toList
    & distillRatingsAssocList ppopts
    & extract & fmap (second rating)
    & sortBy (comparing (Down . snd))

-- | Sorted racer-rating pairs for the latest race, generated using the
-- default options.
runTestDefault :: [(PipId, Double)]
runTestDefault = runTest def def

demoRanking :: PostProcessOptions -> Tab.Table String String String
demoRanking ppopts = runTest def ppopts
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Rating"]
        (\(p, rtg) -> [T.unpack p, show rtg])

