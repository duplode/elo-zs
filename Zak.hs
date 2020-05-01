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

demoAccumulated :: Tab.Table String String String
demoAccumulated = testData def
    & LS.scan
        (accumulatedRatings
            . distillRatings def {excludeProvisional=False}
                <$> allRatings)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Accumulated Rating"]
        (\(AtRace ri x) -> [show ri, show x])

demoPipCount :: Tab.Table String String String
demoPipCount = testData def
    & LS.scan
        (codistributeL
            . (extract . pipCount &&& accumulatedRatings)
            . distillRatings def {excludeProvisional=False}
                <$> allRatings)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Number of Racers", "Accumulated Rating"]
        (\(AtRace ri (n, x)) -> [show ri, show n, show x])

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

demoMeanSnap :: Tab.Table String String String
demoMeanSnap = testData def
    & LS.scan
        (meanRatingAtSnapshot
            -- . distillRatings def{activityCut=Just 12, excludeProvisional=True}
            . distillRatings def {activityCut=Nothing, excludeProvisional=True}
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

-- | Current rating and past peak rating for racers active within the last
-- 12 races.
demoCurrent :: Tab.Table String String String
demoCurrent = testData def
    -- Using a prescan for highestPerPeak is intentional, as it gives the
    -- peak before the last update, which is an interesting information.
    -- To check: is the composed scan here sufficiently strict?
    & LS.scan (returnA &&& LS.prescan highestPerPip <<< ratingsFold)
    & (fmap rating . extract *** fmap extract) . last
    & uncurry (Map.intersectionWith (,))
    & Map.toList
    & sortBy (comparing (Down . snd))
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Rating", "Prev. Peak"]
        (\(p, (rtg, pk)) -> [T.unpack p, show rtg, show pk])
    where
    ratingsFold
        = distillRatings def {activityCut=Just 12, excludeProvisional=True}
        <$> allRatings

demoWeighedScores :: RaceIx -> Tab.Table String String String
demoWeighedScores selRaceIx = testData def
    & LS.scan weighedScores
    & (!! selRaceIx)
    & N.toList . extract
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(Result p sc) -> [T.unpack p, show sc])

-- | Sum of weighed scores over a span of races, discarding the three worst
-- results.
demoWeighedSeason
    :: RaceIx -- ^ Initial race.
    -> Int -- ^ Number of races.
    -> Tab.Table String String String
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

demoWeighedStrength :: Tab.Table String String String
demoWeighedStrength = testData def
    & LS.scan weighedStrength
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

demoReinvertedStrength :: Tab.Table String String String
demoReinvertedStrength = testData def
    & LS.scan reinvertedStrength
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

-- $> :set -XOverloadedStrings
-- $>
-- >$>  demoHeadToHead ("dreadnaut" :| ["Overdrijf","Seeker1982"]) & Text.Tabular.Csv.render id id id & writeFile "test.csv"
-- >$> demoAccumulated & Text.Tabular.Csv.render id id id & writeFile "test.csv"
-- >$> demoCurrent & Text.Tabular.Csv.render id id id & writeFile "test.csv"
-- >$> demoWeighedSeason 214 12 & Text.Tabular.Csv.render id id id & writeFile "test.csv"
-- >$> demoPipCount & Text.Tabular.Csv.render id id id & writeFile "test.csv"
-- >$> demoReinvertedStrength & Text.Tabular.Csv.render id id id & writeFile "test.csv"
-- >$> demoRanking def { activityCut = Just 4, selectedRace = Just 104, excludeProvisional = True } & demoPretty
-- $> demoMeanSnap & Text.Tabular.Csv.render id id id & writeFile "test.csv"

