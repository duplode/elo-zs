{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
-- |
-- Module: Direct.Engine
--
-- An Elo-like engune in which the gamma performance model parameter is used
-- in lieu of the ratings.
--
-- The fine tuning of algorithm parameters was partly guided by the
-- discussion on Glickman, Mark E.,
-- [/A Comprehensive Guide to Chess Ratings/](http://www.glicko.net/research/acjpaper.pdf)
-- (1995).
module Direct.Engine
    ( finalRatings
    , allRatings
    , previousRatings
    , isProvisional
    , initialRating
    ) where

import Types
import Analysis.PerfModel.Orbital

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bool
import Data.Ord
import Control.Comonad
import qualified Control.Foldl as L
import qualified Control.Scanl as LS
import Data.List

-- | Initial rating for new players. Defined as a constant here for
-- expediteness, taking into account that making it configurable isn't as
-- essential as it is making other engine parameters configurable.
initialRating :: Double
initialRating = kFromRating 1500

-- | Converts a 'WDL' outcome to a numeric value.
wdlScore :: WDL -> Double
wdlScore = \case
    Loss -> 0
    Draw -> 0.5
    Win -> 1

-- | Converts two player results into a match between the players.
--
-- When it comes to determine the match outcome, lower is better, as befits
-- standings on a race scoreboard.
faceOff :: Ord a => Result p a -> Result p a -> FaceOff p
faceOff x y = FaceOff
    { player = pipsqueakTag x
    , opponent = pipsqueakTag y
    , outcome = case comparing result x y of
        LT -> Win
        EQ -> Draw
        GT -> Loss
    }

-- | Given race results for multiple racers, generate all one-on-one matches
-- between them. Each combination of two players generates one match.
toFaceOffs :: Ord a => EloOptions -> NE.NonEmpty (Result p a) -> [FaceOff p]
toFaceOffs eopts xs = concat . transpose
    . maybe id (map . take) (eloRemoteCutoff eopts)
    . NE.toList
    $ xs =>> \(y :| ys) -> faceOff y <$> ys
-- The match matrix is transposed before concatenation so that, if we choose
-- not to batch matches per race, matches between neighbours on the scoreboard
-- are handled first when the matches are used to calculate the rating deltas.
-- Rating updates are not commutative. In particular, it appears that handling
-- neighbours first helps to tame rating spikes from atypical results.

-- | Difference between expected and actual scores for a match. Positive if
-- the score was higher than what was expected.
scoreDiscrepancy
    :: Double  -- ^ Relative rating: rx / (rx + ry).
    -> WDL     -- ^ Match outcome.
    -> Double  -- ^ Rating change.
scoreDiscrepancy rel otc = wdlScore otc - expectedScore
    where
    -- | Expected score, given the relative rating.
    expectedScore = rel^3*(6*rel^2 - 15*rel + 10)

-- | Is the player rating provisional?
--
-- Note the comparison should be pefromed with the number of entries before
-- the current event.
isProvisional :: EloOptions -> PipData -> Bool
isProvisional eopts rtg = entries rtg < eloProvisionalGraduation eopts


-- | The original version of core rating update engine, which only updates
-- ratings after computing all deltas.
updateRatingsSimple
    :: forall p. Ord p
    => EloOptions
    -> AtRace (Map p PipData)  -- ^ Ratings at the previous race.
    -> [FaceOff p]             -- ^ Matches of the current event.
    -> AtRace (Map p PipData)  -- ^ Updated ratings at the current race.
updateRatingsSimple eopts (AtRace ri rtgs) =
    AtRace ri' . updateCount ri' . L.fold applyCurrentChanges . toDeltas
    -- The index used to be computed with a seq here. That is unnecessary now
    -- that the corresponding AtRace field is strict.
    where
    -- | Index of the current event.
    ri' = ri + 1

    -- | Calculates individual rating changes from a list of matches.
    toDeltas :: [FaceOff p] -> [(p, Double)]
    toDeltas = concatMap $ \xy ->
        let (dx, dy) = toDelta eopts rtgs xy
        in [dx, dy]

    -- | Applies all changes for the current race.
    applyCurrentChanges :: L.Fold (p, Double) (Map p PipData)
    applyCurrentChanges = L.Fold (applyChange ri') rtgs id

-- | The non-batching version of the core rating update engine.
updateRatingsNoBatching
    :: forall p. Ord p
    => EloOptions
    -> AtRace (Map p PipData)  -- ^ Ratings at the previous race.
    -> [FaceOff p]             -- ^ Matches of the current event.
    -> AtRace (Map p PipData)  -- ^ Updated ratings at the current race.
updateRatingsNoBatching eopts (AtRace ri rtgs) =
    AtRace ri' . updateCount ri' . L.fold applyCurrentChanges
    -- The index used to be computed with a seq here. That is unnecessary now
    -- that the corresponding AtRace field is strict.
    where
    -- | Index of the current event.
    ri' = ri + 1

    -- | Applies all changes for the current race.
    applyCurrentChanges :: L.Fold (FaceOff p) (Map p PipData)
    applyCurrentChanges = L.Fold applyBothChanges rtgs id
        where
        applyBothChanges rtgs xy =
            let (dx, dy) = toDelta eopts rtgs xy
            in flip (applyChange ri') dy (flip (applyChange ri') dx rtgs)


-- | Calculates ratings for all players after the final event.
finalRatings
    :: Ord p
    => EloOptions
    -> L.Fold (NE.NonEmpty (Result p Int)) (AtRace (Map p PipData))
finalRatings eopts = L.Fold
    (\ar xs -> update ar (toFaceOffs eopts xs))
    (AtRace 0 Map.empty)
    id
    where
    update = case eloBatchingStrategy eopts of
        Batching -> updateRatingsSimple eopts
        NoBatching -> updateRatingsNoBatching eopts

-- | Calculates ratings for all players after each event.
allRatings
    :: Ord p
    => EloOptions
    -> LS.Scan (NE.NonEmpty (Result p Int)) (AtRace (Map p PipData))
allRatings eopts = LS.postscan (finalRatings eopts)

-- | Calculates ratings for all players before each event. Typically used for
-- meta-metrics, such as evaluating predictions.
previousRatings
    :: Ord p
    => EloOptions
    -> LS.Scan (NE.NonEmpty (Result p Int)) (AtRace (Map p PipData))
previousRatings eopts = LS.prescan (finalRatings eopts)


-- Functions that are part of the core algorithm.

-- | Calculates individual rating changes from a single match.
toDelta :: Ord p
        => EloOptions
        -> Map p PipData
        -> FaceOff p
        -> ((p, Double), (p, Double))
toDelta eopts rtgs xy =
    let px = Map.lookup (player xy) rtgs
        py = Map.lookup (opponent xy) rtgs
        rx = maybe initialRating rating px
        ry = maybe initialRating rating py
        rsum = rx + ry
        rel = rx / rsum
        (kx, ky)
            | provisionalCheck px && not (provisionalCheck py)
                = (kHi, kLo)
            | not (provisionalCheck px) && provisionalCheck py
                = (kLo, kHi)
            | otherwise = (kBase, kBase)
        baseDelta = rsum * scoreDiscrepancy rel (outcome xy)
    in ((player xy, kx * baseDelta), (opponent xy, -ky * baseDelta))
    where
    -- Provisional rating test, defaulting to True for new entrants. Note
    -- that for the purposes of rating calculation the test is applied before
    -- the update.
    provisionalCheck :: Maybe PipData -> Bool
    provisionalCheck = maybe True (isProvisional eopts)

    -- Modulation factors.
    kBase = eloModulation eopts
    kHi = kBase * eloProvisionalFactor eopts
    kLo = kBase / eloProvisionalFactor eopts


-- | Applies a rating change to the collection of ratings.
--
-- The last event index is updated here, as that should only be done for
-- players who took part in the current event.
applyChange :: Ord p => RaceIx -> Map p PipData -> (p, Double) -> Map p PipData
applyChange ri' rtgs (p, d) = Map.alter upsertPip p rtgs
    where
    upsertPip :: Maybe PipData -> Maybe PipData
    upsertPip =  Just . \case
        Nothing -> PipData (initialRating + d) 0 ri'
        Just (PipData rtg n _) -> PipData (rtg + d) n ri'

-- | Updates the event count for players who took part in the current
-- event.
--
-- Performed in a separate step, so that the count is only increased once
-- per player who took part in the current event.
updateCount :: Ord p => RaceIx -> Map p PipData -> Map p PipData
updateCount ri' = fmap (\(PipData rtg n i) ->
    PipData rtg (bool id (+ 1) (i == ri') n) i)
