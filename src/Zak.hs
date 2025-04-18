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
import Tidying (DataPreparationOptions(..), PostProcessOptions(..)
    , isKeptRating, isKeptByPP, distillRatings, distillRatingsAssocList)
import Tabular
import Zak.Misc
import Zak.Results
import Util.Lone

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ord
import Data.List
import Data.List.Extra (takeEnd)
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

-- | Writes the tables used for the Folyami updates.
writeForPub :: IO ()
writeForPub = do
    demoCurrentForPub 4 & demoToHtml "test4.html"
    demoHighestForPub & demoToHtml "testh.html"
    demoNearMissesForPub 12 4 & demoToHtml "testn.html"
    demoForChart & demoToCsv "teste.csv"
    demoFullHistory & demoToCsv "history.csv"

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
        (pipCount
            . distillRatings def {provisionalCut=Nothing}
                <$> allRatings eopts)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Number of Racers"]
        (\(AtRace ri n) -> [show ri, show n])

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

-- The PostProcessOptions argument should be used to set the desired
-- windows for the mean.
demoMeanSnap
    :: EloOptions
    -> PostProcessOptions
    -> Tab.Table String String String
demoMeanSnap eopts ppopts = testData def
    & LS.scan
        (meanRatingAtSnapshot
            . distillRatings ppopts
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

-- | Bespoke type for a publication-ready entry for the current ranking.
-- Note that ratings are rounded down (ergo the integral type).
data CurrentRankingEntry = CurrentRankingEntry
    { crePipId :: !PipId
    , creRating :: !Integer
    , creDelta :: !(Maybe Integer)
    }
    deriving Show

-- | Sorted current ranking entries, suitable for publication.
runForPub
    :: Maybe RaceIx -- ^ Selected race.
    -> Int -- ^ Activity cut.
    -> [CurrentRankingEntry]
runForPub selRace aCut = testData dpopts
    & LS.scan (returnA &&& LS.postscan (displayDeltas ppopts)
        <<< allRatings eopts)
    -- TODO: This is a little too messy.
    -- Note that the intersection means that, for this specific
    -- purpose, the filtering in the display delta scan is not
    -- needed.
    & map (\(AtRace ri rtgs, AtRace _ dds) ->
        AtRace ri (Map.intersectionWith (,) rtgs dds))
    & maybe last (flip (!!)) (selectedRace ppopts)
    & fmap @AtRace Map.toList
    & distill & extract
    -- Sort by rating before rounding down the ratings.
    & sortBy (comparing (Down . fst . snd))
    & fmap mkEntry
    where
    -- We might want to make these configurable eventually.
    dpopts = def
    eopts = def
    ppopts = def { selectedRace = selRace, activityCut = Just aCut }
    -- TODO: Consider abstracting the distillers
    distill = extend $
        \(AtRace ri xs) -> filter (isKeptRating ppopts ri . fst . snd) xs
    mkEntry (pipId, (pipData, delta)) = CurrentRankingEntry
        { crePipId = pipId
        , creRating = floor (rating pipData)
        , creDelta = delta
        }

demoForPub :: Maybe RaceIx -> Int -> Tab.Table String String String
demoForPub selRace aCut = runForPub selRace aCut
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Rating", "Change"]
        (\cre ->
            [ T.unpack (crePipId cre)
            , show (creRating cre)
            , formatDelta (creDelta cre)
            ])
    where
    formatDelta = maybe "n/a" $ \delta ->
        (if delta > 0 then "+" else "") ++ show delta

demoCurrentForPub :: Int -> Tab.Table String String String
demoCurrentForPub = demoForPub Nothing

-- | Table for historical chart generation.
runForChart :: Maybe Int -> Maybe RaceIx -> [AtRace (Map.Map PipId Integer)]
runForChart wnd selRace = testData dpopts
    & LS.scan (fmap @AtRace (fmap mkVal) . distill <$> allRatings eopts)
    & maybe id (take . (+1)) selRace
    & maybe id takeEnd wnd
    where
    -- TODO: Make these configurable
    aCut = 4
    dpopts = def
    eopts = def

    ppopts = def { selectedRace = selRace, activityCut = Just aCut }
    distill = extend $
        \(AtRace ri xs) -> Map.filter (isKeptRating ppopts ri) xs
    mkVal = floor . rating
    -- addPip ps = Set.union ps . Map.keysSet . extract
    -- pickPips = foldl' addPip Set.empty

demoForChart'
    :: (RaceIx -> String)
    -> Bool
    -> Maybe Int
    -> Maybe RaceIx
    -> Tab.Table String String String
demoForChart' trkFmt sortByRtg wnd selRace = dat
    & arrangeTable
        (fmap (trkFmt . raceIx))
        (T.unpack <$> pipsList)
        (\arxs -> pipsList
            <&> (\p -> formatRetrieved $ Map.lookup p (extract arxs)))
    where
    dat = runForChart wnd selRace
    latestRatings = foldl'
        (\lts arxs -> Map.unionWith
            (\(AtRace rix _) (AtRace _ x') -> AtRace rix x')
            lts
            (codistributeL arxs))
        Map.empty
        dat
    comparison = if sortByRtg
        then comparing (Down . extract . snd)
        else comparing (raceIx . snd &&& fst)
    pipsList = fmap fst . sortBy comparison
        . Map.toList $ latestRatings
    formatRetrieved = maybe "" show

demoForChart :: Tab.Table String String String
demoForChart = demoForChart' toZakLabel True (Just 13) Nothing

demoFullHistory :: Tab.Table String String String
demoFullHistory = demoForChart' (show . toZakPlotIx) True Nothing Nothing

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
