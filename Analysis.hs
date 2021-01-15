{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Analysis where

import Types
import Engine
import Tidying
import Util.Lone
import Analysis.PerfModel
import Analysis.Simulation

import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Control.Foldl as L
import qualified Control.Scanl as LS
import Data.Profunctor
import Control.Comonad
import Control.Arrow
import Data.Maybe
import Data.Default.Class
import qualified Data.List.NonEmpty as N
import Data.List
import Control.Monad
import Control.Monad.Trans

-- | Highest rating achieved by each racer, annotated with the race at which
-- it was achieved.
highestPerPip :: L.Fold (AtRace Ratings) (Map.Map PipId (AtRace Double))
highestPerPip = L.Fold unionHighest Map.empty id
    where
    unionHighest highs = Map.unionWith higher highs . surroundL (fmap rating)
    higher ari@(AtRace _ x) arj@(AtRace _ y) = if y > x then arj else ari

-- | Folds a source by the means of 'Control.Foldl.Fold', while preserving the
-- 'Lone' comonadic context the source lies in, as well as using it for
-- filtering the fold input.
--
-- This function provides a general strategy that covers the problem of
-- summarising ratings-at-a-race data.
foldThroughLone
    :: (Foldable f, Functor f, Lone g u)
    => (s -> f a)     -- ^ Prepare the source for folding.
    -> (g a -> Bool)  -- ^ Filtering predicate for the input which uses the
                      -- 'Lone' comonadic context around it.
    -> L.Fold a b     -- ^ Folding algebra. /foldl/ provides ways to perform
                      -- various auxiliary steps, including input mapping
                      -- ('lmap'/'premap') and filtering ('prefilter').
    -> g s            -- ^ Source. Typically a collection of
                      -- ratings-at-races.
    -> g b            -- ^ Result.
foldThroughLone expose pred alg =
    extend (L.fold ((L.prefilter pred . lmap extract) alg) . codistributeL)
        . fmap expose
-- This implementation calls for a little explanation. The crucial trick is
-- @extend codistributeL@:
--
-- > extend codistributeL :: (Lone g, Functor f) => g (f a) -> g (f (g a))
--
-- It duplicates the 'Lone' comonadic environment two layers down, so that
-- it can be used by the filter predicate. Afterwards, @extract@ throws it
-- away. Note that @prefilter pred@ and @lmap extract@ seem to be composed
-- backwards. That happens because they are acting on inputs, and therefore
-- contravariantly. Moving those transformations inside the algebra matters
-- because it frees us from imposing a @Filterable@ constraint on @f@.
--
-- The concrete @f@ of most immediate interest here is 'Map', which is not
-- @Applicative@, and so using the more familiar @sequenceA@ instead of
-- @codistributeL@ wouldn't do. A more left field alternative would be
-- @sequence1@ from /semigroupoids/, and even then the @Apply@ constraint
-- would be needlessly restrictive.

-- | Folds collections of ratings-at-races while only using the ratings of
-- racers who took part in the current race.
foldRatingsPerRace :: L.Fold PipData b -> AtRace Ratings -> AtRace b
foldRatingsPerRace = foldThroughLone id isCurrentlyActive
    where
    isCurrentlyActive (AtRace ri rtg) = lastRace rtg == ri

-- | Acumulated ratings per race, including only racers who took part in each
-- race. A race strength metric.
accumulatedRatings :: AtRace Ratings -> AtRace Double
accumulatedRatings = foldRatingsPerRace (lmap rating L.sum)

-- | Mean ratings per race, including only racers who took part in each race
-- A race strength metric.
meanRatingPerRace :: AtRace Ratings -> AtRace Double
meanRatingPerRace = foldRatingsPerRace (lmap rating L.mean)

-- | Number of racers per race. A race strength metric.
pipCount :: AtRace Ratings -> AtRace Int
pipCount = foldRatingsPerRace L.length

-- | Highest rating among racers active within the last four races. A
-- current form metric.
windowLeaders :: AtRace Ratings -> AtRace (Maybe (PipId, Double))
windowLeaders = foldThroughLone Map.toList isRecentlyActive algLead
    where
    isRecentlyActive (AtRace ri (_, rtg)) = ri - lastRace rtg < 4
    algLead = lmap (second rating) (L.maximumBy (comparing snd))
-- windowLeaders is the primary reason why 'foldThroughLone' has its
-- @s -> f a@ argument. In particular, note that @Map.toList@ is not a
-- natural transformation between @Functor@s, and that we ultimately want
-- the dictionary keys to show up in the output of the fold.

-- | Folds ratings-at-a-race, with no filtering.
foldRatingsAtSnapshot :: L.Fold PipData b -> AtRace Ratings -> AtRace b
foldRatingsAtSnapshot alg = fmap @AtRace (L.fold alg)
-- This is straightforward enough to be done without 'foldThroughLone'.

-- | Mean rating at a race, including all racers ever. A meta-metric.
meanRatingAtSnapshot :: AtRace Ratings -> AtRace Double
meanRatingAtSnapshot = foldRatingsAtSnapshot (lmap rating L.mean)

-- | Rating evolution for a racer.
personalHistory :: PipId -> [AtRace Ratings] -> [AtRace Double]
personalHistory p = mapMaybe (surroundL @AtRace (fmap rating . Map.lookup p))

-- | Prototype for running multiple histories side by side.
personalRating :: PipId -> AtRace Ratings -> AtRace (Maybe Double)
personalRating p = fmap @AtRace (fmap rating . Map.lookup p)

-- | Should this rating be retained according to the post-processing
-- criteria?
isKeptRating
    :: EloOptions
    -> PostProcessOptions
    -> RaceIx              -- ^ Current event index.
    -> PipData
    -> Bool
isKeptRating eopts ppopts ri rtg =
    maybe True (\ac -> ri - lastRace rtg < ac) (activityCut ppopts)
        && not (excludeProvisional ppopts && isProvisional eopts rtg)

-- | Apply the post-processing criteria to filter ratings (association list
-- version).
distillRatingsAssocList
    :: EloOptions
    -> PostProcessOptions
    -> AtRace [(p, PipData)]
    -> AtRace [(p, PipData)]
distillRatingsAssocList eopts ppopts = extend $
    \(AtRace ri rtgs) -> filter (isKeptRating eopts ppopts ri . snd) rtgs

-- | Apply the post-processing criteria to filter ratings (Map version).
distillRatings
    :: EloOptions
    -> PostProcessOptions
    -> AtRace Ratings
    -> AtRace Ratings
distillRatings eopts ppopts = extend $
    \(AtRace ri rtgs) ->  Map.filter (isKeptRating eopts ppopts ri) rtgs

-- Below follow various approaches to race strength estimation.

-- | Prototype for scoring weighed by strength.
simpleWeighedScores
    :: EloOptions
    -> LS.Scan
        (N.NonEmpty (Result PipId Int))
        (AtRace (N.NonEmpty (Result PipId Double)))
simpleWeighedScores eopts
    = (\str rs -> AtRace (raceIx str)
        $ fmap @(Result _) (extract str *) <$> rs)
            <$> strength <*> basePoints
    where
    basePoints = arr computeScores
    computeScores = join
        . fmap (\gp -> fmap @(Result _) (computeScore (length gp)) <$> gp)
        . N.groupAllWith1 result
    computeScore = exponentialScoring
    strength = fmap @AtRace strengthConversion
        . accumulatedRatings
        . distillRatings eopts def {excludeProvisional=False}
        <$> allRatings eopts
    strengthConversion = logisticStrengthConversion

-- | Race strength on a half-logistic slope, with an accumulated rating of
-- 15000 amounting to a 0.5 factor.
logisticStrengthConversion :: Double -> Double
logisticStrengthConversion s = tanh ((log 3 / 15000) * s / 2)
-- logisticStrengthConversion s = 2 / (1 + exp (-(log 3 / 15000) * s)) - 1

-- | Race strength on a linear slope, with an accumulated rating of 45000
-- amounting to a 1.0 factor.
linearStrengthConversion s = s / 45000

-- | Scoring on an exponential curve: 24 points for the winner, 2 for the
-- 12th place, averaging points over drawn racers.
exponentialScoring :: Int -> Int -> Double
exponentialScoring nDrawn rank
    = (24 *) . exp
        $ -(log 12 / 11)
            * (fromIntegral rank + fromIntegral (nDrawn - 1)/2 - 1)
-- log 12 / 11 = 0.22590060452618185 ~ log (25 / 20)
-- log (25 / 18) = 0.32850406697203605
-- log (10 / 6) = 0.5108256237659907
-- log (9 / 6) = 0.4054651081081644

-- | A race strength metric obtained by giving extra weight to the strength
-- of racers in the scoreboard midfield. Uses 'fixedMidfieldWeight' as
-- weighing function.
weighedStrength
    :: EloOptions
    -> LS.Scan (N.NonEmpty (Result PipId Int)) (AtRace Double)
weighedStrength eopts = id  -- fmap @AtRace strengthConversion
    . accumulatedRatings
    . distillRatings eopts def {excludeProvisional=False}
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

-- | Assigns points to race results, as in a regular season scoreboard,
-- but using 'weighedStrength' as weight for the score of each race. Uses
-- 'exponentialScoring' as the base score system.
weighedScores
    :: EloOptions
    -> LS.Scan
        (N.NonEmpty (Result PipId Int))
        (AtRace (N.NonEmpty (Result PipId Double)))
weighedScores eopts
    = (\str rs -> AtRace (raceIx str)
        $ fmap @(Result _) (extract str *) <$> rs)
            <$> weighedStrength eopts <*> basePoints
    where
    basePoints = arr computeScores
    computeScores = join
        . fmap (\gp -> fmap @(Result _) (computeScore (length gp)) <$> gp)
        . N.groupAllWith1 result
    computeScore = exponentialScoring


-- | Scaled Gaussian.
gaussian
    :: Double  -- ^ x-scaling, as a specific distance from the center.
    -> Double  -- ^ Relative steepness (y-ratio at x-scaling from the peak).
    -> Double  -- ^ Peak location, center of the curve.
    -> Double  -- ^ x value.
    -> Double
gaussian d s c x = exp (-(x - c)^2 / w)
    where
    w = - d^2 / log s  -- sigma = - d / (2 * sqrt (ln s))

-- | Scaled witch of Agnesi.
witch
    :: Double  -- ^ x-scaling, as a specific distance from the center.
    -> Double  -- ^ Relative steepness (y-ratio at x-scaling from the peak).
    -> Double  -- ^ Peak location, center of the curve.
    -> Double  -- ^ x value.
    -> Double
witch d s c x = 1 / (b * (x - c)^2 + 1)
    where
    b = (1 - s) / (s * d^2)

-- | Midfield weighing through a gaussian centered at 6th place.
fixedMidfieldWeight
    :: Int  -- ^ Race standing.
    -> Double
fixedMidfieldWeight x = gaussian 5 (2/5) 6 (fromIntegral x)

-- | Midfield weighing through a witch of Agnesi centered at 6th place.
altFixedMidfieldWeight
    :: Int  -- ^ Race standing.
    -> Double
altFixedMidfieldWeight x = witch 6 (1/2) 6 (fromIntegral x)

-- | Midfield weighing through a gaussian centered at two fifths of the
-- scoreboard size. Not a particularly effective weighing curve.
variableMidfieldWeight
    :: Int  -- ^ Number of racers.
    -> Int  -- ^ Race standing.
    -> Double
variableMidfieldWeight n x = gaussian (peak - 1) (2/5) peak (fromIntegral x)
    where
    peak = (2 / 5) * fromIntegral n

-- | Race strength as the logarithm of the reciprocal of the probability of
-- an hypothetical extra racer (the "probe") defeating every other racer in
-- independent head-to-head matches. Behaves reasonably well as a strength
-- metric, even though the independent matches scenario doesn't really
-- reflect what goes on in a race. The reciprocal of the probabilities is
-- obtained through 'reinvertedRatings'.
reinvertedStrength
    :: EloOptions
    -> LS.Scan (N.NonEmpty (Result PipId Int)) (AtRace Double)
reinvertedStrength eopts = id  -- fmap @AtRace strengthConversion
    . fmap (logBase 10)
    . reinvertedRatings
    . distillRatings eopts def {excludeProvisional=False}
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

-- | Race strength as victory unlikelihood of a probe-racer, in terms of the
-- racer performance model and in particular
-- 'Analysis.PerfModel.perfModelStrength'. Computationally cheap, but not a
-- full reflection of the strength of a race, with a very small handful of
-- top racers being able to largely determine the result.
perfStrength
    :: EloOptions
    -> LS.Scan (N.NonEmpty (Result PipId Int)) (AtRace Double)
perfStrength eopts = id
    . fmap @AtRace (perfModelStrength . map rating . Map.elems)
    . extend (\(AtRace ri rtgs)
        -> Map.filter (isCurrentlyActive . AtRace ri) rtgs)
    . distillRatings eopts def {excludeProvisional=False}
    <$> allRatings eopts
    where
    isCurrentlyActive (AtRace ri rtg) = lastRace rtg == ri

perfTopStrength
    :: EloOptions
    -> LS.Scan (N.NonEmpty (Result PipId Int)) (AtRace Double)
perfTopStrength eopts = id
    . fmap @AtRace (perfModelTopStrength 5 . map rating . Map.elems)
    . extend (\(AtRace ri rtgs)
        -> Map.filter (isCurrentlyActive . AtRace ri) rtgs)
    . distillRatings eopts def {excludeProvisional=False}
    <$> allRatings eopts
    where
    isCurrentlyActive (AtRace ri rtg) = lastRace rtg == ri

perfTopStrength'
    :: EloOptions
    -> Int  -- ^ Which top-N probability to use.
    -> LS.ScanM IO (N.NonEmpty (Result PipId Int)) (AtRace Double)
perfTopStrength' eopts pos = LS.arrM integrateRaces <<< LS.generalize basicScan
    where
    basicScan = extend (\(AtRace ri rtgs)
            -> Map.filter (isCurrentlyActive . AtRace ri) rtgs)
        . distillRatings eopts def {excludeProvisional=False}
        <$> allRatings eopts
    integrateRaces ar = do
        -- The top-N cutoff should be made properly configurable.
        let ar' = fmap @AtRace (perfModelTopStrength pos . map rating . Map.elems) ar
        liftIO $ putStrLn ("Done race #" ++ show (raceIx ar') ++ " : " ++ show (extract ar'))
        return ar'
    isCurrentlyActive (AtRace ri rtg) = lastRace rtg == ri

-- | Race strengths obtained as top-N finish unlikelihood, in terms of the
-- racer performance model. Estimated by simulating race results a large
-- number of times (see the 'Analysis.Simulation' module). Computationally
-- expensive, but gives a much better picture than using victory
-- probabilities.
simStrength
    :: EloOptions
    -> SimOptions
    -> LS.ScanM SimM (N.NonEmpty (Result PipId Int)) (AtRace Double)
simStrength eopts simOpts =
    LS.arrM runSimsForRace <<< LS.generalize basicScan
    where
    isCurrentlyActive (AtRace ri rtg) = lastRace rtg == ri
    basicScan = extend (\(AtRace ri rtgs)
            -> Map.filter (isCurrentlyActive . AtRace ri) rtgs)
        . distillRatings eopts def {excludeProvisional=False}
        <$> allRatings eopts
    runSimsForRace = codistributeL . fmap @AtRace (simModelStrength simOpts)
        <=< (\ar -> liftIO $ putStrLn ("Runs for race #" ++ show (raceIx ar))
            >> return ar)

-- | Normalized discounted cumulative gain, implemented as a rating system
-- evaluation metric along the lines of Dehpanah et al.,
-- [/The Evaluation of Rating Systems in Online Free-for-All Games/](https://arxiv.org/abs/2008.06787v1)
-- (2020). See also https://en.wikipedia.org/wiki/Discounted_cumulative_gain .
ndcg :: EloOptions -> LS.Scan (N.NonEmpty (Result PipId Int)) Double
ndcg eopts = previousRatings eopts &&& returnA
    >>> arr (uncurry pickActives)
    >>> arr (expectedRanks *** actualRanks)
    >>> arr (uncurry computeNdcg)
    where
    -- The order is such that both rank lists are ordered by pipsqueak tag.
    expectedRanks = id
        . map snd
        . sortBy (comparing fst)
        . zipWith (\n (p, _) -> (p, n)) [1..]
        . sortBy (comparing (Down . rating . snd))
        . Map.assocs
    -- Using the race results to only keep previous ratings for those who took
    -- part in the current race.
    pickActives rtgs entries =
        let selector = Map.fromList
                . map (\(Result p x) -> (p, x))
                . N.toList
                $ entries
        in (Map.intersectionWith const (extract rtgs) selector, entries)
    actualRanks = id
        . map result
        . sortBy (comparing pipsqueakTag)
        . tidyRanks
    computeNdcg exs acs = L.fold ndcgFold (zipWith ndcgParcel exs acs)
    ndcgParcel ex ac = SP (log 2 / log (ac + 1)) (1 / (1 + abs (ac - ex)))
    ndcgFold = L.Fold
        (\(SP norm acc) (SP normParcel parcel)
            -> SP (norm + normParcel) (acc + normParcel * parcel))
        (SP 0 0)
        (\(SP idcg acc) -> acc / idcg)

data SP a b = SP !a !b

tidyRanks :: N.NonEmpty (Result PipId Int) -> [Result PipId Double]
tidyRanks =
    -- sortBy (comparing pipsqueakTag)
    concatMap (\(n, g) -> fmap @(Result _) (n+) <$> g)
    -- Accumulates the base positions by counting elements from the previous
    -- groups.
    . scanl1 (\(n, g) (_, g') -> (n + fromIntegral (length g), g'))
    . zip [1..]
    -- Replaces the reported result with a draw adjustment.
    . map (\g ->
        fmap @(Result _) (const (fromIntegral (length g - 1) / 2)) <$> N.toList g)
    . N.groupAllWith result
    . N.toList
