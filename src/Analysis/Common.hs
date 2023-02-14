{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Analysis.Common
--
-- Folding strategies for analysing historical race data, and other
-- utility functions for analysis
module Analysis.Common
    ( foldThroughLone
    , foldRatingsPerRace
    , foldRatingsAtSnapshot
    , isKeptByPP
    , isKeptRating
    , distillRatings
    , distillRatingsAssocList
    ) where

import Types
import Tidying
import Util.Lone

import Data.Profunctor
import Control.Comonad
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map

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

-- | Folds ratings-at-a-race, with no filtering.
foldRatingsAtSnapshot :: L.Fold PipData b -> AtRace Ratings -> AtRace b
foldRatingsAtSnapshot alg = fmap @AtRace (L.fold alg)
-- This is straightforward enough to be done without 'foldThroughLone'.

-- | Should this data be retained according to the post-processing
-- criteria?
isKeptByPP
    :: (d -> RaceIx)       -- ^ Obtain last race index for the data.
    -> (d -> Int)          -- ^ Obtain total entries for the data.
    -> PostProcessOptions
    -> RaceIx              -- ^ Current event index.
    -> d
    -> Bool
isKeptByPP fDLri fDEntr ppopts ri d =
    maybe True (\ac -> ri - fDLri d < ac) (activityCut ppopts)
        && maybe True (\pc -> fDEntr d >= pc) (provisionalCut ppopts)

-- | Should this rating be retained according to the post-processing
-- criteria?
isKeptRating
    :: PostProcessOptions
    -> RaceIx              -- ^ Current event index.
    -> PipData
    -> Bool
isKeptRating = isKeptByPP lastRace entries

-- | Apply the post-processing criteria to filter ratings (association list
-- version).
distillRatingsAssocList
    :: PostProcessOptions
    -> AtRace [(p, PipData)]
    -> AtRace [(p, PipData)]
distillRatingsAssocList ppopts = extend $
    \(AtRace ri rtgs) -> filter (isKeptRating ppopts ri . snd) rtgs

-- | Apply the post-processing criteria to filter ratings (Map version).
distillRatings
    :: PostProcessOptions
    -> AtRace Ratings
    -> AtRace Ratings
distillRatings ppopts = extend $
    \(AtRace ri rtgs) ->  Map.filter (isKeptRating ppopts ri) rtgs

