{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
-- |
-- Module: Engine
--
-- The core Elo algorithm.
--
-- The fine tuning of algorithm parameters was partly guided by the
-- discussion on Glickman, Mark E.,
-- [/A Comprehensive Guide to Chess Ratings/](http://www.glicko.net/research/acjpaper.pdf)
-- (1995).
module Engine
    ( finalRatings
    , allRatings
    , isProvisional
    ) where

import Types

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
toFaceOffs :: Ord a => NE.NonEmpty (Result p a) -> [FaceOff p]
toFaceOffs xs = concat . transpose . NE.toList
    $ xs =>> \(y :| ys) -> faceOff y <$> ys
-- The match matrix is transposed before concatenation so that, when the
-- matches are used to calculate the rating deltas, matches between
-- neighbours on the scoreboard are handled first. Rating updates are not
-- commutative. In particular, it appears that handling neighoburs first
-- helps to tame rating spikes from atypical results.

-- | Difference between expected and actual scores for a match. Positive if
-- the score was higher than what was expected.
scoreDiscrepancy
    :: Double  -- ^ Difference between the ratings before the match.
    -> WDL     -- ^ Match outcome.
    -> Double  -- ^ Rating change.
scoreDiscrepancy gap otc = wdlScore otc - expectedScore
    where
    -- | Expected score, given the rating gap.
    expectedScore = 1 / (1 + 10**(-gap/w))
    -- | Logistic growth factor. The value defined here, 400, is standard for
    -- chess rating computations. Using it, a 0.6 expected score corresponds
    -- to a ~70.4 rating gap, and a 0.9 expected score, to a ~382 gap.
    w = 400

-- | Is the player rating provisional?
--
-- A rating is considered provisional until the player has taken part in five
-- events.
isProvisional :: PipData -> Bool
isProvisional rtg = entries rtg < 5

-- | The core rating update engine.
updateRatings
    :: forall p. Ord p
    => AtRace (Map p PipData)  -- ^ Ratings at the previous race.
    -> [FaceOff p]             -- ^ Matches of the current event.
    -> AtRace (Map p PipData)  -- ^ Updated ratings at the current race.
updateRatings (AtRace ri rtgs) =
    AtRace ri' . updateCount . L.fold applyCurrentChanges
    -- The index used to be computed with a seq here. That is unnecessary now
    -- that the corresponding AtRace field is strict.
    where
    -- | Modulation (or, as Glickman puts it, attenuation) factor for rating
    -- changes. The chosen value, 16, is lower than the standard one for
    -- chess, 24, in order to compensate for the high number of matches in a
    -- typical race.
    kBase = 16
    -- | Players with provisional ratings have a higher modulation factor, so
    -- that their ratings converge more quickly.
    kHi = 32
    -- | Opponents of players with provisional ratings have a lower
    -- modulation factor, so that their ratings are less affected by matches
    -- against players with uncertain ratings.
    --
    -- Using different modulation factors in a single match means a match can
    -- cause a net change on the accumulated rating of the player pool. On
    -- why that is less of a problem than it might seem at first, see
    -- Glickman (1995), p. 36.
    kLo = 8
    -- | Initial rating for new players.
    defRating = 1500
    -- | Index of the current event.
    ri' = ri + 1

    -- Provisional rating test, defaulting to True for new entrants. Note
    -- that for the purposes of rating calculation the test is applied before
    -- the update.
    provisionalCheck :: Maybe PipData -> Bool
    provisionalCheck = maybe True isProvisional

    -- | Calculates individual rating changes from a single match.
    toDelta :: Map p PipData -> FaceOff p -> ((p, Double), (p, Double))
    toDelta rtgs xy =
        let px = Map.lookup (player xy) rtgs
            py = Map.lookup (opponent xy) rtgs
            gap = maybe defRating rating px - maybe defRating rating py
            (kx, ky)
                | provisionalCheck px && not (provisionalCheck py) = (kHi, kLo)
                | not (provisionalCheck px) && provisionalCheck py = (kLo, kHi)
                | otherwise = (kBase, kBase)
            baseDelta = scoreDiscrepancy gap (outcome xy)
        in ((player xy, kx * baseDelta), (opponent xy, -ky * baseDelta))

    -- | Applies a rating change to the collection of ratings.
    --
    -- The last event index is updated here, as that should only be done for
    -- players who took part in the current event.
    applyChange :: Map p PipData -> (p, Double) -> Map p PipData
    applyChange rtgs (p, d) = Map.alter upsertPip p rtgs
        where
        upsertPip :: Maybe PipData -> Maybe PipData
        upsertPip =  Just . \case
            Nothing -> PipData (defRating + d) 0 ri'
            Just (PipData rtg n _) -> PipData (rtg + d) n ri'

    -- | Applies all changes for the current race.
    applyCurrentChanges :: L.Fold (FaceOff p) (Map p PipData)
    applyCurrentChanges = L.Fold applyBothChanges rtgs id
        where
        applyBothChanges rtgs xy =
            let (dx, dy) = toDelta rtgs xy
            in flip applyChange dy (flip applyChange dx rtgs)

    -- | Updates the event count for players who took part in the current
    -- event.
    --
    -- Performed in a separate step, so that the count is only increased once
    -- per player who took part in the current event.
    updateCount :: Map p PipData -> Map p PipData
    updateCount = fmap (\(PipData rtg n i) ->
        PipData rtg (bool id (+ 1) (i == ri') n) i)

-- | Calculates ratings for all players after the final event.
finalRatings
    :: Ord p
    => L.Fold (NE.NonEmpty (Result p Int)) (AtRace (Map p PipData))
finalRatings = L.Fold
    (\ar xs -> updateRatings ar (toFaceOffs xs))
    (AtRace 0 Map.empty)
    id

-- | Calculates ratings for all players after each event.
allRatings
    :: Ord p
    => LS.Scan (NE.NonEmpty (Result p Int)) (AtRace (Map p PipData))
allRatings = LS.postscan finalRatings




-- | The original version of core rating update engine, which only updates
-- ratings after computing all deltas.
updateRatingsSimple
    :: forall p. Ord p
    => AtRace (Map p PipData)  -- ^ Ratings at the previous race.
    -> [FaceOff p]             -- ^ Matches of the current event.
    -> AtRace (Map p PipData)  -- ^ Updated ratings at the current race.
updateRatingsSimple (AtRace ri rtgs) =
    AtRace ri' . updateCount . L.fold applyCurrentChanges . toDeltas
    -- The index used to be computed with a seq here. That is unnecessary now
    -- that the corresponding AtRace field is strict.
    where
    -- | Modulation (or, as Glickman puts it, attenuation) factor for rating
    -- changes. The chosen value, 16, is lower than the standard one for
    -- chess, 24, in order to compensate for the high number of matches in a
    -- typical race.
    kBase = 16
    -- | Players with provisional ratings have a higher modulation factor, so
    -- that their ratings converge more quickly.
    kHi = 32
    -- | Opponents of players with provisional ratings have a lower
    -- modulation factor, so that their ratings are less affected by matches
    -- against players with uncertain ratings.
    --
    -- Using different modulation factors in a single match means a match can
    -- cause a net change on the accumulated rating of the player pool. On
    -- why that is less of a problem than it might seem at first, see
    -- Glickman (1995), p. 36.
    kLo = 8
    -- | Initial rating for new players.
    defRating = 1500
    -- | Index of the current event.
    ri' = ri + 1

    -- Provisional rating test, defaulting to True for new entrants. Note
    -- that for the purposes of rating calculation the test is applied before
    -- the update.
    provisionalCheck :: Maybe PipData -> Bool
    provisionalCheck = maybe True isProvisional

    -- | Calculates individual rating changes from a list of matches.
    toDeltas :: [FaceOff p] -> [(p, Double)]
    toDeltas = concatMap $ \xy ->
        let  -- In principle, these lookups might be performed once per
             -- player, rather than twice per match.
            px = Map.lookup (player xy) rtgs
            py = Map.lookup (opponent xy) rtgs
            gap = maybe defRating rating px - maybe defRating rating py
            (kx, ky)
                | provisionalCheck px && not (provisionalCheck py) = (kHi, kLo)
                | not (provisionalCheck px) && provisionalCheck py = (kLo, kHi)
                | otherwise = (kBase, kBase)
            baseDelta = scoreDiscrepancy gap (outcome xy)
        in [(player xy, kx * baseDelta), (opponent xy, -ky * baseDelta)]

    -- | Applies a rating change to the collection of ratings.
    --
    -- The last event index is updated here, as that should only be done for
    -- players who took part in the current event.
    applyChange :: Map p PipData -> (p, Double) -> Map p PipData
    applyChange rtgs (p, d) = Map.alter upsertPip p rtgs
        where
        upsertPip :: Maybe PipData -> Maybe PipData
        upsertPip =  Just . \case
            Nothing -> PipData (defRating + d) 0 ri'
            Just (PipData rtg n _) -> PipData (rtg + d) n ri'

    -- | Applies all changes for the current race.
    applyCurrentChanges :: L.Fold (p, Double) (Map p PipData)
    applyCurrentChanges = L.Fold applyChange rtgs id

    -- | Updates the event count for players who took part in the current
    -- event.
    --
    -- Performed in a separate step, so that the count is only increased once
    -- per player who took part in the current event.
    updateCount :: Map p PipData -> Map p PipData
    updateCount = fmap (\(PipData rtg n i) ->
        PipData rtg (bool id (+ 1) (i == ri') n) i)
