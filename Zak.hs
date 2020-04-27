{-# LANGUAGE TypeApplications, LambdaCase #-}
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
import Util.Lone

import qualified Data.Map.Strict as Map
import Data.Ord
import Data.List
import qualified Data.Text as T
import Control.Arrow
import Data.Default.Class
import qualified Text.Tabular as Tab
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Comonad
import qualified Control.Scanl as LS
import qualified Control.Foldl as L
import Data.Semigroupoid
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad
import Data.Maybe

-- >$> sortBy (comparing (Down . extract . snd)) $ Map.toList $ highestPerPip (allRatings (testData def))
demoHighest :: Tab.Table String String String
demoHighest = testData def
    & L.fold
        (highestPerPip `o`
            (distillRatings def {excludeProvisional=True} <$> finalRatings))
    & Map.toList & sortBy (comparing (Down . extract . snd))
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Race", "Rating"]
        (\(p, AtRace ri rtg) -> [T.unpack p, toZakLabel ri, show rtg])

-- >$> sortBy (comparing (Down . snd)) $ accumulatedRatings (allRatings (testData def))
demoAccumulated :: Tab.Table String String String
demoAccumulated = testData def
    & LS.scan
        (accumulatedRatings
            . distillRatings def {excludeProvisional=False}
                <$> allRatings)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Accumulated Rating"]
        ((:[]) . show . extract)

-- >$> meanRatingPerRace (allRatings (testData def))
demoMean :: Tab.Table String String String
demoMean = testData def
    & LS.scan
        (meanRatingPerRace
            . distillRatings def {excludeProvisional=False}
                <$> allRatings)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

-- >$> windowLeaders (allRatings (testData def))
demoWindowLeaders :: Tab.Table String String String
demoWindowLeaders = testData def
    & LS.scan
        (windowLeaders
            . distillRatings def {excludeProvisional=True}
                <$> allRatings)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Racer", "Rating"]
        (maybe ["N/A", "N/A"] (\(p, x) -> [T.unpack p, show x]) . extract)

-- >$> meanRatingPerSnapshot (allRatings (testData def))
demoMeanSnap :: Tab.Table String String String
demoMeanSnap = testData def
    & LS.scan
        (meanRatingAtSnapshot
            . distillRatings def {activityCut=Just 12, excludeProvisional=True}
                <$> allRatings)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

-- This might cause trouble with many rows. Rendering the race labels as a
-- normal column would likely help.
demoPersonalHistory :: PipId -> Tab.Table String String String
demoPersonalHistory p = testData def
    & LS.scan
        (distillRatings def {excludeProvisional = True}
            <$> allRatings)
    & personalHistory p
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        [T.unpack p]
        ((:[]) . show . extract)

demoHeadToHead :: NonEmpty PipId -> Tab.Table String String String
demoHeadToHead ps = testData def
    & LS.scan
        (combineAtRaces
            . traverse personalRating ps
            . distillRatings def {excludeProvisional = True}
                <$> allRatings)
    & catMaybes
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        (T.unpack <$> N.toList ps)
        (map show . extract)
    where
    combineAtRaces :: NonEmpty (AtRace (Maybe a)) -> Maybe (AtRace [a])
    combineAtRaces ys
        = fmap (AtRace ri) . join . fmap sequenceA
            $ foldr op (Just []) ys
        where
        ri = raceIx (N.head ys)
        op x = \case
            Just ys
                | raceIx x == ri -> Just (extract x : ys)
                | otherwise -> Nothing
            Nothing -> Nothing

-- | Sorted racer-rating pairs for a specific race (defined through
-- 'PostProcessOptions.selectedRace', defaults to the latest race).
runTest :: DataPreparationOptions -> PostProcessOptions -> [(PipId, Double)]
runTest dpopts ppopts = LS.scan allRatings (testData dpopts)
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


demoWeighedScores :: RaceIx -> Tab.Table String String String
demoWeighedScores selRaceIx = testData def
    & LS.scan weighedScores
    & (!! selRaceIx)
    & N.toList . extract
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(Result p sc) -> [T.unpack p, show sc])

demoWeighedSeason :: RaceIx -> Int -> Tab.Table String String String
demoWeighedSeason start delta  = testData def
    & LS.scan (Map.fromList . fmap (\(Result p sc) -> (p, sc))
            . N.toList . extract
        <$> weighedScores)
    & sortBy (comparing (Down . snd)) . Map.toList
        . fmap (sum . take (delta - 3) . sortBy (comparing Down))
        . foldr (Map.unionWith (++)) Map.empty
        . fmap (fmap (:[]))
        . take delta . drop start
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(p, sc) -> [T.unpack p, show sc])

demoScoreHistory :: PipId -> Tab.Table String String String
demoScoreHistory p = testData def
    & LS.scan (fmap (map result
                . filter (\r -> pipsqueakTag r == p) . N.toList)
        <$> weighedScores)
    & catMaybes . fmap (listToMaybe . codistributeL)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        [T.unpack p]
        ((:[]) . show . extract)
