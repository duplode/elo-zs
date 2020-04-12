{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
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

highestPerPip :: [(Int, Ratings)] -> Map.Map PipId (Int, Double)
highestPerPip =
    foldl' (\hgs (ri, rtgs) ->
        let rtgs' = (ri,) . rating <$> rtgs
        in Map.unionWith (\(ri, x) (rj, y)
            -> if y > x then (rj, y) else (ri, x)) hgs rtgs') Map.empty

--demoHighest = sortBy (comparing (Down . snd . snd)) $ Map.toList $ highestPerPip (allRatings (testData def))
demoHighest :: Tab.Table String String String
demoHighest = highestPerPip 
        (distillRatings' def {excludeProvisional=True}
            <$> allRatings (testData def))
    & Map.toList & sortBy (comparing (Down . snd . snd)) 
    & \rs -> Tab.Table
        (Tab.Group Tab.SingleLine (Tab.Header . show
            <$> zipWith const [1..] rs))
        (Tab.Group Tab.SingleLine 
            [Tab.Header "Racer", Tab.Header "Race", Tab.Header "Rating"])
        ((\(p, (ri, rtg)) -> [T.unpack p, toZakLabel ri, show rtg]) <$> rs)

foldRatingsPerRace :: L.Fold PipData b -> [(Int, Ratings)] -> [(Int, b)]
foldRatingsPerRace alg = fmap 
    $ second (L.fold alg)
    . (\(ri, rtgs) -> (ri, Map.filter (\rtg -> lastRace rtg == ri) rtgs))

accumulatedRatings :: [(Int, Ratings)] -> [(Int, Double)]
accumulatedRatings = foldRatingsPerRace (lmap rating L.sum) 

meanRatingPerRace :: [(Int, Ratings)] -> [(Int, Double)]
meanRatingPerRace = foldRatingsPerRace (lmap rating L.mean) 

-- demoAccumulated = sortBy (comparing (Down . snd)) $ accumulatedRatings (allRatings (testData def))
demoAccumulated :: Tab.Table String String String
demoAccumulated = accumulatedRatings 
        (distillRatings' def {excludeProvisional=True} 
            <$> allRatings (testData def))
    & sortBy (comparing (Down . snd)) 
    & \rs -> Tab.Table
        (Tab.Group Tab.SingleLine (Tab.Header . toZakLabel . fst <$> rs))
        (Tab.Group Tab.SingleLine [Tab.Header "Accumulated Rating"])
        ((:[]) . show . snd <$> rs)

--demoMean = meanRatingPerRace (allRatings (testData def))
demoMean :: Tab.Table String String String
demoMean = meanRatingPerRace
        (distillRatings' def {excludeProvisional=True} 
            <$> allRatings (testData def))
    & \rs -> Tab.Table
        (Tab.Group Tab.SingleLine (Tab.Header . toZakLabel . fst <$> rs))
        (Tab.Group Tab.SingleLine [Tab.Header "Mean Rating"])
        ((:[]) . show . snd <$> rs)

windowLeaders :: [(Int, Ratings)] -> [(Int, Maybe (PipId, Double))]
windowLeaders = fmap 
    $ second (fmap (fst &&& rating . snd)
        . L.fold (L.maximumBy (comparing (rating . snd))) . Map.toList)
    . (\(ri, rtgs) -> (ri, Map.filter (\rtg -> ri - lastRace rtg < 4) rtgs))

--demoWindowLeaders = windowLeaders (allRatings (testData def))
demoWindowLeaders :: Tab.Table String String String
demoWindowLeaders = windowLeaders
        (distillRatings' (def {activityCut=Just 12, excludeProvisional=True}) 
            <$> allRatings (testData def))
    & \rs -> Tab.Table
        (Tab.Group Tab.SingleLine (Tab.Header . toZakLabel . fst <$> rs))
        (Tab.Group Tab.SingleLine [Tab.Header "Racer", Tab.Header "Rating"])
        (maybe [] (\(p, x) -> [T.unpack p, show x]) . snd <$> rs)

-- This one doesn't filter.
foldRatingsPerSnapshot :: L.Fold PipData b -> [(Int, Ratings)] -> [(Int, b)]
foldRatingsPerSnapshot alg = fmap (second (L.fold alg))

meanRatingPerSnapshot :: [(Int, Ratings)] -> [(Int, Double)]
meanRatingPerSnapshot = foldRatingsPerSnapshot (lmap rating L.mean)

-- demoMeanSnap = meanRatingPerSnapshot (allRatings (testData def))
demoMeanSnap :: Tab.Table String String String
demoMeanSnap = meanRatingPerSnapshot
        (distillRatings' (def {activityCut=Just 12, excludeProvisional=True}) 
            <$> allRatings (testData def))
    & \rs -> Tab.Table
        (Tab.Group Tab.SingleLine (Tab.Header . toZakLabel . fst <$> rs))
        (Tab.Group Tab.SingleLine [Tab.Header "Mean Rating"])
        ((:[]) . show . snd <$> rs)

toZakLabel :: Int -> String
toZakLabel ri
    | ri > 19 = "C" ++ show (ri - 3)
    | otherwise = case ri of
        15 -> "P1"
        16 -> "C15"
        17 -> "P2"
        18 -> "C16"
        19 -> "P3"
        _ -> "C" ++ show ri

personalHistory :: PipId -> [(Int, Ratings)] -> [(Int, Double)]
personalHistory p = catMaybes
    . fmap (traverse (fmap rating . Map.lookup p))

demoPersonalHistory :: PipId -> [(Int, Double)]
demoPersonalHistory p = personalHistory p (allRatings (testData def))

-- This might cause trouble with many rows. Rendering the race labels as a
-- normal column would likely help.
prettyPersonalHistory :: PipId -> Tab.Table String String String
prettyPersonalHistory p = demoPersonalHistory p 
    & \rs -> Tab.Table 
        (Tab.Group Tab.SingleLine (Tab.Header . toZakLabel . fst <$> rs))
        (Tab.Group Tab.SingleLine [Tab.Header (T.unpack p)])
        ((:[]) . show . snd <$> rs)

demoPretty :: Tab.Table String String String -> IO ()
demoPretty t = putStrLn (Tab.render id id id t)

prettyRanking :: PostProcessOptions -> Tab.Table String String String
prettyRanking ppopts = runTest True ppopts
    & \rs -> Tab.Table
        (Tab.Group Tab.SingleLine (Tab.Header . show
            <$> zipWith const [1..] rs))
        (Tab.Group Tab.SingleLine [Tab.Header "Racer", Tab.Header "Rating"])
        ((\(p, rtg) -> [T.unpack p, show rtg]) <$> rs)

distillRatings
  :: PostProcessOptions
     -> (Int, [(a, PipData)]) -> (Int, [(a, PipData)])
distillRatings ppopts (ri, rtgs) =
    (ri, filter 
        (\(_, rtg) -> maybe True
                (\ac -> ri - lastRace rtg <= ac) 
                (activityCut ppopts)
            && not (excludeProvisional ppopts && isProvisional rtg)) 
        rtgs)

distillRatings'
  :: PostProcessOptions
       -> (Int, Ratings) -> (Int, Ratings)
distillRatings' ppopts (ri, rtgs) =
    (ri, Map.filter 
        (\rtg -> maybe True
                (\ac -> ri - lastRace rtg <= ac) 
                (activityCut ppopts)
            && not (excludeProvisional ppopts && isProvisional rtg)) 
        rtgs)

runTest :: Bool -> PostProcessOptions -> [(PipId, Double)]
runTest noGhosts ppopts = id
    . sortBy (comparing (Down . snd)) 
    . fmap (second rating) . snd
    . distillRatings ppopts
    . second Map.toList . maybe last (flip (!!)) (selectedRace ppopts) 
    $ allRatings (testData (DPOpts noGhosts))

runTestDefault :: [(PipId, Double)]
runTestDefault = runTest True def


