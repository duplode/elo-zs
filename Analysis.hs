{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Analysis where

import Types
import Engine
import Tidying

import qualified Data.Map.Strict as Map
import Data.Ord
import Data.List
import qualified Control.Foldl as L
import Data.Profunctor
import Data.Maybe
import Control.Comonad

highestPerPip :: [AtRace Ratings] -> Map.Map PipId (AtRace Double)
highestPerPip =
    foldl' (\highs (AtRace ri rtgs) ->
        let rtgs' = AtRace ri . rating <$> rtgs  -- Lone, or left adjoint.
        in Map.unionWith higher highs rtgs') Map.empty
    where
    higher ari@(AtRace _ x) arj@(AtRace _ y) = if y > x then ari else arj

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

-- | Highest rating among racers active within the last three races. A
-- current form metric.
windowLeaders :: [AtRace Ratings] -> [AtRace (Maybe (PipId, Double))]
windowLeaders = fmap
    $ extend (L.fold (L.maximumBy (comparing extract))
        . Map.toList . fmap rating . recentlyActivePipsOnly)
    where
    recentlyActivePipsOnly (AtRace ri rtgs) =
        Map.filter (\rtg -> ri - lastRace rtg <= 3) rtgs

-- | Folds collections of ratings-at-races, with no filtering.
foldRatingsPerSnapshot :: L.Fold PipData b -> [AtRace Ratings] -> [AtRace b]
foldRatingsPerSnapshot alg = fmap (fmap @AtRace (L.fold alg))

-- | Mean ratings per race, including all racers ever. A meta-metric.
meanRatingPerSnapshot :: [AtRace Ratings] -> [AtRace Double]
meanRatingPerSnapshot = foldRatingsPerSnapshot (lmap rating L.mean)

-- | Rating evolution for a racer.
personalHistory :: PipId -> [AtRace Ratings] -> [AtRace Double]
personalHistory p = mapMaybe (traverse @AtRace (fmap rating . Map.lookup p))

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


-- >$> :set -XOverloadedStrings
--
-- >$> demoPretty $ demoRanking def { activityCut = Just 12 }
--
-- >$> demoPretty $ demoPersonalHistory "Alan Rotoi"
