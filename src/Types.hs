{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- |
-- Module: Types
--
-- Shared domain types.
module Types (
    -- * Elementary synonyms
      PipId
    , RaceIx
    -- * Core algorithm types
    , PipData(..)
    , Ratings
    , Result(..)
    , Rank'
    , Standing
    , WDL(..)
    , FaceOff(..)
    , FaceOff'
    , EloOptions(..)
    , conventionalEloDefaults
    , EloProvStrategy(..)
    -- * Race index tagging
    , AtRace(..)
    , raceIx
    , fromAtRace
    , toAtRace
    , CoatRace(..)
    -- * Simulation engine types
    , SimM(..)
    , runSimM
    , evalSimM
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Comonad
import Data.Functor.Adjunction
import Data.Functor.Rep
import Data.Distributive
import Data.Default.Class
import System.Random.MWC
import Control.Monad.State.Strict

-- | Concrete identifier for the racers/players being ranked.
type PipId = T.Text

-- | Concrete index for locating and identifying races.
type RaceIx = Int

-- | Player data which is updated through the Elo ranking computation.
data PipData = PipData
    { rating :: !Double     -- ^ Elo rating.
    , entries :: !Int       -- ^ Total number of events the player entered.
    , lastRace :: !RaceIx   -- ^ Index of the last event the player entered.
    }
    deriving (Eq, Ord, Show)

-- | Key-value store of player data for retrieval and update.
type Ratings = Map.Map PipId PipData

-- | Result attained by a player in an event.
--
-- This type is necessary to handle events with more than two players, such as
-- races.
data Result p a = Result
    { pipsqueakTag :: !p  -- ^ Player identifier.
                          -- "pipsqueak" means racer in the Stunts community
                          -- jargon.
    , result :: !a        -- ^ Attained result.
    }
    deriving (Eq, Ord, Show, Functor)

-- | Race rank. Not an integer because draw ranks are adjusted to be
-- equidistant from the surrounding ranks.
type Rank' = Double
-- TODO: It might be a good idea to use a newtype to prevent directly doing
-- arithmetic with this type.

-- | Concrete race result.
type Standing = Result PipId Rank'

-- | Possible outcomes of a match between two players.
data WDL = Loss | Draw | Win
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Record of a match between two players.
data FaceOff p = FaceOff
    { player :: !p           -- ^ A player.
    , opponent :: !p         -- ^ Their opponent.
    , outcome :: !WDL        -- ^ The outcome, from the 'player''s point of
                             -- view.
    , remoteness :: !Double  -- ^ The magnitude of the difference betwen the
                             -- performance of the players in the match,
                             -- measured in some convenient form. It can be
                             -- used to give extra weight to matches between
                             -- racers close to each other in a scoreboard
                             -- or, in the opposite direction, to give extra
                             -- weight for matches with larger performance
                             -- differences.
    }
    deriving (Eq, Ord, Show)

-- | Concrete match record type.
type FaceOff' = FaceOff PipId

-- | Parameters for Elo and simulation engines.
data EloOptions = EloOptions
    {   -- | Modulation (or, as Glickman puts it, attenuation) factor for rating
        -- changes. A value, such as 18, lower than the typical for chess, 24
        -- might be used to compensate for the high number of matches in a
        -- race.
      eloModulation :: Double
        -- | How much weight should be given to matches between players far
        -- removed from each other in the event results. The supplied value,
        -- if any, amounts to the sum of all weights in the limit of an
        -- infinitely large field of players, so larger values mean less
        -- dampening of remote matches. 'Nothing' disables the weighing, so
        -- that all matches are counted equally.
        --
        -- This dampening for remote matches helps keeping outlier results and
        -- racer number variations from having an exaggerated effect on the
        -- rankings.
    , eloRemotenessModulation :: Maybe Double
        -- | Which modulation adjustment strategy to use for provisional
        -- ratings.
    , eloProvisionalStrategy :: Maybe EloProvStrategy
        -- | Whether to use the provisional modulation factor for both players
        -- when both have provisional status. 'Nothing' disables provisional
        -- modulation.
    , eloFullyProvisionalMatches :: Bool
        -- | The number of earlier events a player must have taken part in so
        -- that their rating isn't counted as provisional in the current event.
        -- For instance, the default value of 12 means the rating ceases being
        -- provisional after the twelfth event, while 0 disables provisional
        -- ratings.
    , eloProvisionalGraduation :: Int
        -- | Average correction factor for the modulation in matches involving
        -- one player with a provisional rating.
        --
        -- With values above 1, provisionally rated players will have a higher
        -- modulation factor, so that their ratings converge more quickly, and
        -- their opponents with non-provisional ratings will ratings have a
        -- lower modulation factor, so that their ratings are less affected by
        -- matches against players with uncertain ratings.
        --
        -- The value specified here is the average factor over the whole
        -- provisional window. The specific values at each point of the window
        -- depend on the choice for 'eloProvisionalStrategy'.
        --
        -- Using different modulation factors in a single match means a match
        -- can cause a net change on the accumulated rating of the player pool.
        -- On why that is less of a problem than it might seem at first, see
        -- Glickman (1995), p. 36.
    , eloProvisionalFactor :: Double
        -- Shape parameter to be used for the gamma distribution that underlies
        -- both the Elo and the simulation engines. A value of 1 corresponds
        -- exactly to the conventional Elo algorithm. Workable values range
        -- from 1 to 20.
    , eloGammaShape :: Int
    , simProbeRating :: Double -- ^ Rating of the probe that will be used
                               -- as a reference in the simulation-based
                               -- strength calculations.
    , simTarget :: Int         -- ^ Tally the top-n results attained by
                               -- the probe in the simulations.
    , simRuns :: Int           -- ^ How many times each race should be
                               -- simulated.
    }
    deriving (Eq, Show)

-- | The strategy for handling provisional ratings in the Elo engine.
data EloProvStrategy
    = FixedProvisional   -- ^ Changes modulations by a fixed factor.
    | LinearProvisional  -- ^ Changes modulations by a variable factor
                         -- that decays linearly.
    | SmoothProvisional  -- ^ Changes modulations by a variable factor
                         -- that decays exponentially.
    deriving (Eq, Enum, Show)


instance Default EloOptions where
    def = EloOptions
        { eloModulation = 18
        , eloRemotenessModulation = Just 22
        , eloProvisionalStrategy = Just SmoothProvisional
        , eloFullyProvisionalMatches = True
        , eloProvisionalGraduation = 12
        , eloProvisionalFactor = 1.5
        , eloGammaShape = 3
        , simProbeRating = 1500  -- TODO: Should this be Engine.initialRating?
        , simTarget = 5
        , simRuns = 10000
        }

-- | The former defaults, which set up a conventional Elo calculation with
-- a fixed provisional factor.
conventionalEloDefaults :: EloOptions
conventionalEloDefaults = def
    { eloModulation = 20
    , eloProvisionalStrategy = Just FixedProvisional
    , eloProvisionalGraduation = 5
    , eloProvisionalFactor = 2
    , eloGammaShape = 1
    }

-- | A value tagged with a race index. Uses include race identification and
-- application of player activity windows.
--
-- By convention, indexes corresponding to a race are positive integers. The
-- value '0' is reserved for tagging values from before the first race.
data AtRace a = AtRace !RaceIx a
    deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

-- | Gets the race index associated with a value.
--
-- To extract the value itself, use 'extract'.
raceIx :: AtRace a -> RaceIx
raceIx (AtRace ri _) = ri

-- | Conversion from a pair
toAtRace :: (RaceIx, a) -> AtRace a
toAtRace = uncurry AtRace

-- | Conversion from a pair
fromAtRace :: AtRace a -> (RaceIx, a)
fromAtRace (AtRace ri a) = (ri, a)

instance Comonad AtRace where
    extract (AtRace _ a) = a
    extend f ar@(AtRace ri _) = AtRace ri (f ar)

-- | An unremarkable right adjoint for 'AtRace'. The only reason it exists
-- here is so that a 'Lone' constraint on 'AtRace' can be satisfied.
--
-- It is not /entirely/ impossible there might be some use for a stream
-- encoding of 'CoatRace', though I'm not holding my breath.
newtype CoatRace a = CoatRace (RaceIx -> a)
    deriving (Functor)

instance Distributive CoatRace where
    collect f u = CoatRace $ \ri -> (\(CoatRace g) -> g ri) . f <$> u

instance Representable CoatRace where
    type Rep CoatRace = RaceIx
    tabulate = CoatRace
    index (CoatRace g) = g

instance Adjunction AtRace CoatRace where
    leftAdjunct uc = \a -> CoatRace $ \ri -> uc (AtRace ri a)
    rightAdjunct cr = \(AtRace ri a) -> let (CoatRace g) = cr a in g ri

-- | A state monad for threading generator state in simulations.
newtype SimM a = SimM { getSimM :: StateT Seed IO a }
    deriving (Functor, Applicative, Monad, MonadState Seed, MonadIO)

runSimM :: Maybe Seed -> SimM a -> IO (a, Seed)
runSimM mSeed sim = do
    seed <- maybe createSystemSeed return mSeed
    runStateT (getSimM sim) seed

evalSimM :: Maybe Seed -> SimM a -> IO a
evalSimM mSeed = fmap fst . runSimM mSeed
