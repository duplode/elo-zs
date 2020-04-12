{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Elo where

import Zak.Results
import Types
import Elo.Core
import DataPreparation

import qualified Data.Map.Strict as Map
import Data.Ord
import Data.List
import qualified Data.Text as T
import Control.Arrow
import Data.Default
import qualified Control.Foldl as L
import Data.Profunctor
import Data.Maybe
import qualified Text.Tabular as Tab
import qualified Text.Tabular.AsciiArt as Tab
import Data.Function ((&))
import Control.Comonad

data  PostProcessOptions = PPOpts
    { activityCut :: Maybe Int
    , selectedRace :: Maybe Int
    , excludeProvisional :: Bool
    }

instance Default PostProcessOptions where
    def = PPOpts
        { activityCut = Nothing
        , selectedRace = Nothing
        , excludeProvisional = False
        }

-- | Arrange a ranking or listing as a 'Table'.
arrangeTable 
    :: ([a] -> [String])  -- ^ How to make row headers from the source list.
    -> [String]           -- ^ Column headers.
    -> (a -> [String])    -- ^ How to display values.
    -> [a]                -- ^ Source list.
    -> Tab.Table String String String
arrangeTable mkRhs chs mkVals as = Tab.Table
    (Tab.Group Tab.SingleLine (Tab.Header <$> mkRhs as))
    (Tab.Group Tab.SingleLine (Tab.Header <$> chs))
    (mkVals <$> as)

highestPerPip :: [AtRace Ratings] -> Map.Map PipId (AtRace Double)
highestPerPip =
    foldl' (\highs (AtRace ri rtgs) ->
        let rtgs' = AtRace ri . rating <$> rtgs  -- Lone, or left adjoint.
        in Map.unionWith higher highs rtgs') Map.empty
    where
    higher ari@(AtRace _ x) arj@(AtRace _ y) = if y > x then ari else arj

--demoHighest = sortBy (comparing (Down . snd . snd)) $ Map.toList $ highestPerPip (allRatings (testData def))
demoHighest :: Tab.Table String String String
demoHighest = highestPerPip
        (distillRatings def {excludeProvisional=True}
            <$> allRatings (testData def))
    & Map.toList & sortBy (comparing (Down . extract . snd))
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Race", "Rating"]
        (\(p, AtRace ri rtg) -> [T.unpack p, toZakLabel ri, show rtg])

-- | Folds collections of ratings-at-races while only using the ratings of
-- racers who took part in the current race.
foldRatingsPerRace :: L.Fold PipData b -> [AtRace Ratings] -> [AtRace b]
foldRatingsPerRace alg = fmap $ extend (L.fold alg . currentlyActivePipsOnly)
    where
    currentlyActivePipsOnly (AtRace ri rtgs) =
        Map.filter (\rtg -> lastRace rtg == ri) rtgs

-- | Acumulated ratings per race, including only racers who took part in each
-- race. A race strength metric.
accumulatedRatings :: [AtRace Ratings] -> [AtRace Double]
accumulatedRatings = foldRatingsPerRace (lmap rating L.sum)

-- | Mean ratings per race, including only racers who took part in each race
-- A race strength metric.
meanRatingPerRace :: [AtRace Ratings] -> [AtRace Double]
meanRatingPerRace = foldRatingsPerRace (lmap rating L.mean)

-- demoAccumulated = sortBy (comparing (Down . snd)) $ accumulatedRatings (allRatings (testData def))
demoAccumulated :: Tab.Table String String String
demoAccumulated = accumulatedRatings
        (distillRatings def {excludeProvisional=True}
            <$> allRatings (testData def))
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Accumulated Rating"]
        ((:[]) . show . extract)

--demoMean = meanRatingPerRace (allRatings (testData def))
demoMean :: Tab.Table String String String
demoMean = meanRatingPerRace
        (distillRatings def {excludeProvisional=True}
            <$> allRatings (testData def))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

-- | Highest rating among racers active within the last three races. A
-- current form metric.
windowLeaders :: [AtRace Ratings] -> [AtRace (Maybe (PipId, Double))]
windowLeaders = fmap
    $ extend (L.fold (L.maximumBy (comparing extract))
        . Map.toList . fmap rating . recentlyActivePipsOnly)
    where
    recentlyActivePipsOnly (AtRace ri rtgs) =
        Map.filter (\rtg -> ri - lastRace rtg <= 3) rtgs


--demoWindowLeaders = windowLeaders (allRatings (testData def))
demoWindowLeaders :: Tab.Table String String String
demoWindowLeaders = windowLeaders
        (distillRatings def {activityCut=Just 12, excludeProvisional=True}
            <$> allRatings (testData def))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Racer", "Rating"]
        (maybe ["N/A", "N/A"] (\(p, x) -> [T.unpack p, show x]) . extract)

-- | Folds collections of ratings-at-races, with no filtering.
foldRatingsPerSnapshot :: L.Fold PipData b -> [AtRace Ratings] -> [AtRace b]
foldRatingsPerSnapshot alg = fmap (fmap @AtRace (L.fold alg))

-- | Mean ratings per race, including all racers ever. A meta-metric.
meanRatingPerSnapshot :: [AtRace Ratings] -> [AtRace Double]
meanRatingPerSnapshot = foldRatingsPerSnapshot (lmap rating L.mean)

-- demoMeanSnap = meanRatingPerSnapshot (allRatings (testData def))
demoMeanSnap :: Tab.Table String String String
demoMeanSnap = meanRatingPerSnapshot
        (distillRatings def {activityCut=Just 12, excludeProvisional=True}
            <$> allRatings (testData def))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Mean Rating"]
        ((:[]) . show . extract)

-- | Converts a one-based race index to the corresponding ZakStunts track
-- name.
toZakLabel :: RaceIx -> String
toZakLabel ri
    | ri > 19 = "C" ++ show (ri - 3)
    | otherwise = case ri of
        15 -> "P1"
        16 -> "C15"
        17 -> "P2"
        18 -> "C16"
        19 -> "P3"
        _ -> "C" ++ show ri

-- | Rating evolution for a racer.
personalHistory :: PipId -> [AtRace Ratings] -> [AtRace Double]
personalHistory p = mapMaybe (traverse @AtRace (fmap rating . Map.lookup p))

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

-- | Prints a 'Table' to the console.
demoPretty :: Tab.Table String String String -> IO ()
demoPretty t = putStrLn (Tab.render id id id t)

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

-- | Should this rating be retained according to the post-processing
-- criteria?
isKeptRating
    :: PostProcessOptions
    -> RaceIx              -- ^ Current event index.
    -> PipData
    -> Bool
isKeptRating ppopts ri rtg =
    maybe True (\ac -> ri - lastRace rtg <= ac) (activityCut ppopts)
        && not (excludeProvisional ppopts && isProvisional rtg)

-- | Apply the post-processing criteria to filter ratings (association list
-- version).
distillRatingsAssocList
    :: PostProcessOptions -> AtRace [(p, PipData)] -> AtRace [(p, PipData)]
distillRatingsAssocList ppopts = extend $
    \(AtRace ri rtgs) -> filter (isKeptRating ppopts ri . snd) rtgs

-- | Apply the post-processing criteria to filter ratings (Map version).
distillRatings :: PostProcessOptions -> AtRace Ratings -> AtRace Ratings
distillRatings ppopts = extend $
    \(AtRace ri rtgs) ->  Map.filter (isKeptRating ppopts ri) rtgs


-- $> :set -XOverloadedStrings
--
-- $> demoPretty $ demoRanking def { activityCut = Just 12 }
--
-- >$> demoPretty $ demoPersonalHistory "Alan Rotoi"
