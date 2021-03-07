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
    , previousRatings
    ) where

import Types
import Weighing (witch)
import Orbital (deltaWP, initialRating)

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
import Data.Default.Class
import Numeric.RootFinding
import Data.MemoTrie

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
faceOff :: Ord a
    => (a -> a -> Double)  -- ^ Function to compute the remoteness score.
    -> Result p a
    -> Result p a
    -> FaceOff p
faceOff fRemote x y = FaceOff
    { player = pipsqueakTag x
    , opponent = pipsqueakTag y
    , outcome = case comparing result x y of
        LT -> Win
        EQ -> Draw
        GT -> Loss
    , remoteness = fRemote (result x) (result y)
    }

-- | Given race results for multiple racers, generate all one-on-one matches
-- between them. Each combination of two players generates one match.
toFaceOffs :: Ord a
    => (a -> a -> Double)  -- ^ Function to compute the remoteness score.
    -> NE.NonEmpty (Result p a)
    -> [FaceOff p]
toFaceOffs fRemote xs = concat . NE.toList
    $ xs =>> \(y :| ys) -> faceOff fRemote y <$> ys
-- Note that rating updates are not commutative. That doesn't matter here, as
-- each list of faceoffs will be processed (in updateRatings) using the same
-- set of ratings, but it might in other circumsntances.

-- | Difference between expected and actual scores for a match. Positive if
-- the score was higher than what was expected.
scoreDiscrepancy
    :: Int     -- ^ Gamma shape.
    -> Double  -- ^ Difference between the ratings before the match.
    -> WDL     -- ^ Match outcome.
    -> Double  -- ^ Rating change.
scoreDiscrepancy gsh gap otc = wdlScore otc - expectedScore
    where
    -- | Expected score, given the rating gap.
    expectedScore = deltaWP gsh gap

-- | The core rating update engine.
updateRatings
    :: forall p. Ord p
    => EloOptions
    -> AtRace (Map p PipData)  -- ^ Ratings at the previous race.
    -> [FaceOff p]             -- ^ Matches of the current event.
    -> AtRace (Map p PipData)  -- ^ Updated ratings at the current race.
updateRatings eopts (AtRace ri rtgs) =
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

-- | Calculates ratings for all players after the final event.
finalRatings
    :: Ord p
    => EloOptions
    -> L.Fold (NE.NonEmpty (Result p Rank')) (AtRace (Map p PipData))
finalRatings eopts = L.Fold
    (\ar xs -> updateRatings eopts ar (toFaceOffs fRemote xs))
    (AtRace 0 Map.empty)
    id
    where
    fRemote x y = abs (x - y)

-- | Calculates ratings for all players after each event.
allRatings
    :: Ord p
    => EloOptions
    -> LS.Scan (NE.NonEmpty (Result p Rank')) (AtRace (Map p PipData))
allRatings eopts = LS.postscan (finalRatings eopts)

-- | Calculates ratings for all players before each event. Typically used for
-- meta-metrics, such as evaluating predictions.
previousRatings
    :: Ord p
    => EloOptions
    -> LS.Scan (NE.NonEmpty (Result p Rank')) (AtRace (Map p PipData))
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
        gap = maybe initialRating rating px - maybe initialRating rating py
        (kx, ky) = modulationFactors eopts px py
        -- Remoteness weight.
        w = maybe 1
            (\n -> witch (n/pi) (1/2) 0 (remoteness xy))
            (eloRemotenessModulation eopts)
        baseDelta = w * scoreDiscrepancy (eloGammaShape eopts) gap (outcome xy)
    in ((player xy, kx * baseDelta), (opponent xy, -ky * baseDelta))

-- | The modulation factors to use, accounting for the players possibly
-- having provisional ratings using any of the various provisional factor
-- strategies.
modulationFactors
    :: EloOptions
    -> Maybe PipData
    -> Maybe PipData
    -> (Double, Double)
modulationFactors eopts px py = case eloProvisionalStrategy eopts of
    Nothing -> (kBase, kBase)
    Just strat -> provisionalFactors strat px py
    where
    -- Note that both kHi and kLo here use the factor from whoever is the
    -- provisionally rated player.
    provisionalFactors strat px py
        | provisionalCheck px && not (provisionalCheck py)
            = (kHi strat px, kLo strat px)
        | not (provisionalCheck px) && provisionalCheck py
            = (kLo strat py, kHi strat py)
        | eloFullyProvisionalMatches eopts
            && provisionalCheck px && provisionalCheck py
            = (kHi strat px, kHi strat py)
        | otherwise = (kBase, kBase)

    kBase = eloModulation eopts

    kHi strat pz = case strat of
        FixedProvisional -> kBase * eloProvisionalFactor eopts
        SmoothProvisional -> kBase * provisionalDecay eopts (previousEntries pz)

    kLo strat pz = case strat of
        FixedProvisional -> kBase / eloProvisionalFactor eopts
        SmoothProvisional -> kBase / provisionalDecay eopts (previousEntries pz)

    previousEntries :: Maybe PipData -> Int
    previousEntries = maybe 0 entries

    -- Provisional rating test, defaulting to True for new entrants. Note
    -- that for the purposes of rating calculation the test is applied before
    -- the update.
    provisionalCheck :: Maybe PipData -> Bool
    provisionalCheck = maybe True (isProvisional eopts)

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

-- | Is the player rating provisional?
--
-- Note the comparison should be pefromed with the number of entries before
-- the current event.
isProvisional :: EloOptions -> PipData -> Bool
isProvisional eopts rtg = entries rtg < eloProvisionalGraduation eopts

-- | Exponential decay for smooth provisional modulation.
provisionalDecay
    :: EloOptions
    -> Int  -- ^ Number of previous entries.
    -> Double
provisionalDecay eopts nEntries = fctr ^ (grad - nEntries)
    where
    grad = eloProvisionalGraduation eopts
    fctr = memoLpf grad (decodeFloat (eloProvisionalFactor eopts))

-- | Obtains the smooth modulation provisional factor to use at the last event
-- before graduation.
lastProvisionalFactor
    :: Int     -- ^ Provisional window/events until graduation.
    -> Double  -- ^ Average factor over the window.
    -> Double
lastProvisionalFactor grad avgFctr =
    case ridders def (lowerBound, upperBound) fCrossing of
        NotBracketed -> error "Engine.lastProvisionalFactor: fCrossing not bracketed"
        SearchFailed -> error "Engine.lastProvisionalactor: root search failure"
        Root x -> x
    where
    grad' = fromIntegral grad
    fctrSum = grad' * avgFctr
    fCrossing x = x^(grad + 1) - (fctrSum + 1)*x + fctrSum
    lowerBound = ((fctrSum + 1) / (grad' + 1))**(1/grad')
    upperBound = avgFctr^2
-- In the implementation above, fCrossing originates from the equation
-- a * g = q * (q^g - 1) / (q - 1)
-- with a = avgFctr, g = grad and q = lastProvisionalFactor grad avgFctr.
-- lowerBound is the local minimum of fCrossing near the root.
-- upperBound arises from using sqrt q as an underestimate of (q - 1)/log q,
-- which is itself a lower bound for the average factor, and its limit as g
-- goes to infinity.

-- | Memoising version of lastProvisionalFactor. The average factor argument
-- is supplied as a 'decodeFloat' pair.
memoLpf :: Int -> (Integer, Int) -> Double
memoLpf = memo2 (\g a -> lastProvisionalFactor g (uncurry encodeFloat a))
