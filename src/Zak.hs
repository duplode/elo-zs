{-# LANGUAGE TypeApplications, LambdaCase #-}
-- |
-- Module: Zak
--
-- Demo functions that generate results from the ZakStunts sample data and
-- display them.
module Zak where

import Analysis
import Analysis.Common (distillRatings, distillRatingsAssocList, isKeptRating)
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
import System.Random.MWC
import Control.Monad.State.Strict

runHighest :: EloOptions -> [(PipId, AtRace Double)]
runHighest eopts = testData def
    & L.fold
        (highestPerPip `o`
            (distillRatings def <$> finalRatings eopts))
    & Map.toList & sortBy (comparing (Down . extract . snd))

demoHighest :: EloOptions -> Tab.Table String String String
demoHighest eopts = runHighest eopts
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Race", "Rating"]
        (\(p, AtRace ri rtg) -> [T.unpack p, toZakLabel ri, show rtg])

-- | Highest-ever ranking, with default options and rounded down ratings.
--
-- The ratings are rounded down because specific rating values might be taken
-- as milestones. That being so, it wouldn't be appropriate if 2000 points on
-- a published table turned out to actually be 1999.57413.
demoHighestForPub :: Tab.Table String String String
demoHighestForPub = runHighest def
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Pers. Best", "At"]
        (\(p, AtRace ri rtg) ->
            [T.unpack p, show @Integer (floor rtg), toZakLabel ri])

demoAccumulated :: EloOptions -> Tab.Table String String String
demoAccumulated eopts = testData def
    & LS.scan
        (accumulatedRatings
            . distillRatings def {provisionalCut=Nothing}
                <$> allRatings eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Accumulated Rating"]
        (\(AtRace ri x) -> [show ri, show x])

demoPipCount :: EloOptions -> Tab.Table String String String
demoPipCount eopts = testData def
    & LS.scan
        (codistributeL
            . (extract . pipCount &&& fmap log . reinvertedRatings)
            . distillRatings def {provisionalCut=Nothing}
                <$> allRatings eopts)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Number of Racers", "Strength"]
        (\(AtRace ri (n, x)) -> [show ri, show n, show x])

demoMean :: EloOptions -> Tab.Table String String String
demoMean eopts = testData def
    & LS.scan
        (meanRatingPerRace
            . distillRatings def {provisionalCut=Nothing}
                <$> allRatings eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

demoWindowLeaders :: EloOptions -> Tab.Table String String String
demoWindowLeaders eopts = testData def
    & LS.scan
        (windowLeaders
            . distillRatings def
                <$> allRatings eopts)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Racer", "Rating"]
        (maybe ["N/A", "N/A"] (\(p, x) -> [T.unpack p, show x]) . extract)

demoMeanSnap :: EloOptions -> Tab.Table String String String
demoMeanSnap eopts = testData def
    & LS.scan
        (meanRatingAtSnapshot
            -- . distillRatings eopts def{activityCut=Just 12, excludeProvisional=True}
            . distillRatings def {activityCut=Nothing, provisionalCut=Nothing}
                <$> allRatings eopts)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

-- This might cause trouble with many rows. Rendering the race labels as a
-- normal column would likely help.
demoPersonalHistory
    :: EloOptions
    -> PostProcessOptions
    -> PipId
    -> Tab.Table String String String
demoPersonalHistory eopts ppopts p = testData def
    & LS.scan
        (distillRatings ppopts
            <$> allRatings eopts)
    & personalHistory p
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        [T.unpack p]
        ((:[]) . show . extract)

demoHeadToHead :: EloOptions -> NonEmpty PipId -> Tab.Table String String String
demoHeadToHead eopts ps = testData def
    & LS.scan
        (combineAtRaces
            . traverse personalRating ps
            . distillRatings def
                <$> allRatings eopts)
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
runTest :: EloOptions -> DataPreparationOptions -> PostProcessOptions -> [(PipId, Double)]
runTest eopts dpopts ppopts = LS.scan (allRatings eopts) (testData dpopts)
    & maybe last (flip (!!)) (selectedRace ppopts)
    & fmap @AtRace Map.toList
    & distillRatingsAssocList ppopts
    & extract & fmap (second rating)
    & sortBy (comparing (Down . snd))

-- | Sorted racer-rating pairs for the latest race, generated using the
-- default options.
runTestDefault :: [(PipId, Double)]
runTestDefault = runTest def def def

demoRanking :: EloOptions -> PostProcessOptions -> Tab.Table String String String
demoRanking eopts ppopts = runTest eopts def ppopts
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Rating"]
        (\(p, rtg) -> [T.unpack p, show rtg])

-- | Sorted racer-rating pairs, suitable for publication.
runTestForPub
    :: Maybe RaceIx -- ^ Selected race.
    -> Int -- ^ Activity cut.
    -> [(PipId, Integer)]
runTestForPub selRace aCut = runTest def def
        def { selectedRace = selRace, activityCut = Just aCut }
    & fmap (fmap floor)

demoTestForPub :: Maybe RaceIx -> Int -> Tab.Table String String String
demoTestForPub selRace aCut = runTestForPub selRace aCut
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Rating"]
        (\(p, rtg) -> [T.unpack p, show rtg])

-- | Sorted racer-rating pairs for the latest race, suitable for ranking
-- publication.
runCurrentForPub
    :: Int -- ^ Activity cut.
    -> [(PipId, Integer)]
runCurrentForPub = runTestForPub Nothing

demoRankingForPub :: Int -> Tab.Table String String String
demoRankingForPub = demoTestForPub Nothing

-- | Current rating and past peak rating for racers active within the last
-- 12 races.
demoCurrent :: EloOptions -> Tab.Table String String String
demoCurrent eopts = testData def
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
        = distillRatings def {activityCut=Just 12}
        <$> allRatings eopts

-- | Alphabetical list of all racers.
demoAlphabeticalForPub :: Tab.Table String String String
demoAlphabeticalForPub = testData def
    & L.fold (finalRatings def)
    & Map.toList . extract
    & sortBy (comparing (T.toUpper . fst))
    & arrangeTable
        (const "" <$>)
        ["Racer", "Rating", "Incl. Races", "Latest"]
        (\(p, d) ->
            [ T.unpack p
            , show (floor (rating d))
            , show (entries d)
            , toZakLabel (lastRace d)
            ])

-- | Alphabetical list of near-misses due to activity cut.
demoNearMissesForPub
    :: Int  -- ^ Tolerance.
    -> Int  -- ^ Activity cut.
    -> Tab.Table String String String
demoNearMissesForPub tol aCut = testData def
    & L.fold (finalRatings def)
    -- TODO: Consider moving the filtering logic to Analysis.Commons
    & (\ards ->
        let ri = raceIx ards
            ds = extract ards
        in Map.toList $ Map.filter
            (\d ->
                isKeptRating def{ activityCut = Just tol } ri d
                    && not (isKeptRating def{ activityCut = Just aCut } ri d))
            ds)
    & sortBy (comparing (T.toUpper . fst))
    & arrangeTable
        (const "" <$>)
        ["Racer", "Rating", "Latest"]
        (\(p, d) ->
            [ T.unpack p
            , show (floor (rating d))
            , toZakLabel (lastRace d)
            ])

demoWeighedScores :: EloOptions -> RaceIx -> Tab.Table String String String
demoWeighedScores eopts selRaceIx = testData def
    & LS.scan (weighedScores eopts)
    & (!! selRaceIx)
    & N.toList . extract
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(Result p sc) -> [T.unpack p, show sc])

-- | Sum of weighed scores over a span of races, discarding the three worst
-- results.
demoWeighedSeason
    :: EloOptions
    -> RaceIx -- ^ Initial race.
    -> Int -- ^ Number of races.
    -> Tab.Table String String String
demoWeighedSeason eopts start delta  = testData def
    & LS.scan (Map.fromList . fmap (\(Result p sc) -> (p, sc))
            . N.toList . extract
        <$> weighedScores eopts)
    & sortBy (comparing (Down . snd)) . Map.toList
        . fmap (sum . take (delta - 3) . sortBy (comparing Down))
        . foldr (Map.unionWith (++)) Map.empty
        . fmap (fmap (:[]))
        . take delta . drop start
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(p, sc) -> [T.unpack p, show sc])

demoScoreHistory :: EloOptions -> PipId -> Tab.Table String String String
demoScoreHistory eopts p = testData def
    & LS.scan (fmap (map result
                . filter (\r -> pipsqueakTag r == p) . N.toList)
        <$> weighedScores eopts)
    & catMaybes . fmap (listToMaybe . codistributeL)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        [T.unpack p]
        ((:[]) . show . extract)

demoWeighedStrength :: EloOptions -> Tab.Table String String String
demoWeighedStrength eopts = testData def
    & LS.scan (weighedStrength eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

demoReinvertedStrength :: EloOptions -> Tab.Table String String String
demoReinvertedStrength eopts = testData def
    & LS.scan (reinvertedStrength eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

demoPerfStrength :: EloOptions -> Tab.Table String String String
demoPerfStrength eopts = testData def
    & LS.scan (perfStrength eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

demoPerfTopStrength :: EloOptions -> Tab.Table String String String
demoPerfTopStrength eopts = testData def
    & LS.scan (perfTopStrength eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

demoPerfTopStrength' :: EloOptions -> IO (Tab.Table String String String)
demoPerfTopStrength' eopts = testData def
    & LS.scanM (perfTopStrength' eopts 5)
    >>= \res -> res & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])
    & return

demoSimStrength :: EloOptions -> SimM (Tab.Table String String String)
demoSimStrength eopts = testData def
    & LS.scanM (simStrength eopts)
    >>= \res -> res & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])
    & return


demoNdcg :: EloOptions -> Tab.Table String String String
demoNdcg eopts = testData def
    & LS.scan (ndcg eopts)
    & drop 1  -- TODO: Figure out how to get useful results for the first race.
    & arrangeTable
         (fmap (toZakLabel . (+ 1) . raceIx))
         ["NDCG"]
         ((:[]) . show . extract)

demoNdcgComparison :: [(String, EloOptions)] -> Tab.Table String String String
demoNdcgComparison experiments =
    map (\eopts -> LS.scan (ndcg eopts) (testData def)) eoptss
    & transpose
    & drop 1  -- TODO: Figure out how to get useful results for the first race.
    & arrangeTable
         (map (toZakLabel . (+ 1) . maybe 0 raceIx . listToMaybe))
         titles
         (map (show . extract))
    where
    (titles, eoptss) = N.unzip experiments

demoNdcgSim :: EloOptions -> SimM (Tab.Table String String String)
demoNdcgSim eopts = testData def
    & LS.scanM (ndcgSim eopts)
    >>= \res -> res
    & drop 1  -- TODO: Figure out how to get useful results for the first race.
    & arrangeTable
         (fmap (toZakLabel . (+ 1) . raceIx))
         ["NDCG"]
         ((:[]) . show . extract)
    & return

demoNdcgSimComparison :: [(String, EloOptions)] -> SimM (Tab.Table String String String)
demoNdcgSimComparison experiments =
    traverse runExperiment eoptss
    >>= \res -> res & transpose
    & drop 1  -- TODO: Figure out how to get useful results for the first race.
    & arrangeTable
         (map (toZakLabel . (+ 1) . maybe 0 raceIx . listToMaybe))
         titles
         (map (show . extract))
    & return
    where
    (titles, eoptss) = N.unzip experiments
    runExperiment eopts = do
        seed <- get
        res <- LS.scanM (ndcgSim eopts) (testData def)
        liftIO $ restore seed
        return res
