{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- |
-- Module Orbital
--
-- A custom probability distribution for driver laptimes.
--
-- In the PDF, t = 0 is taken to be the ideal laptime in a track. The PDF
-- rises from zero at t = 0, reaches a peak and then tapers off asymptotically
-- to zero.
--
-- The name "orbital" alludes to this PDF being, modulo constant factors, the
-- radial probability density of the 1s hydrogen atom orbital.
module Orbital
    ( orbitalPDF
    , orbitalCDF
    , perfWP
    , eloWP
    , kFromRating
    , ratingFromK
    , OrbitalDistribution
    , orbitalDistr
    , orbitalK
    ) where

import Numeric.RootFinding
import Statistics.Distribution
import Statistics.Distribution.Gamma

import Data.Default.Class

-- | The base Elo exponent.
alpha :: Double
alpha = log 10 / 400

-- | Winning probability of a racer with k = u against a racer with k = v,
-- according to the performance model.
perfWP :: Double -> Double -> Double
--perfWP u v = (u^5 + 5*u^4*v + 10*u^3*v^2) / (u + v)^5
perfWP u v = w^3*(6*w^2 - 15*w + 10)
    where
    w = u / (u + v)

-- | Elo-based winning probability.
eloWP :: Double -> Double -> Double
eloWP ru rv = 1 / (1 + exp(-alpha*(ru-rv)))

referenceRating :: Double
referenceRating = 1800

-- Factoid: exp (1800/400) ~ 90
referenceK :: Double
referenceK = 18

-- | k-to-rating conversion (see step 3 in the Analysis.PerfModel
-- introduction).
ratingFromK :: Double -> Double
ratingFromK v = ru + log (1 / perfWP u v - 1) / alpha
    where
    ru = referenceRating
    u = referenceK

-- | rating-to-k conversion (see step 4 in the Analysis.PerfModel
-- introduction). Implemented through numerical root finding. The usable
-- range of inputs is between -inf and ~35000, which should suffice for
-- most practical purposes.
kFromRating :: Double -> Double
kFromRating rv =
    case ridders def range fCrossing of
        NotBracketed -> error $ errPfx ++ "fCrossing is not bracketed"
        SearchFailed -> error $ errPfx ++ "convergence failure"
        Root t -> t
    where
    ru = referenceRating
    u = referenceK
    -- range = (0, max 10 (2 * 10**(rv/1200)))
    range = (0, max 10 (exp ((5/4) * rv/400) / 5))
    fCrossing v = perfWP u v - eloWP ru rv
    errPfx = "Analysis.PerfModel.Orbital.kFromRating: "

-- | The model distribution is a special case of the gamma distribution, with
-- k = 3 and theta = 1/k.
newtype OrbitalDistribution = OrbitalDistribution
    { orbitalGamma :: GammaDistribution }
    deriving (Eq, Show, Distribution, ContDistr, ContGen, MaybeEntropy
        , Variance, MaybeVariance, Mean, MaybeMean)

-- | Sets up the model distribution given a k factor.
orbitalDistr
    :: Double -- ^ k factor of the model.
    -> OrbitalDistribution
orbitalDistr k = OrbitalDistribution (gammaDistr 3 (1/k))

-- | Recovers the k factor from the distribution.
orbitalK :: OrbitalDistribution -> Double
orbitalK distr = 1 / gdScale (orbitalGamma distr)

-- | PDF of the performance model.
orbitalPDF :: Double -> Double -> Double
orbitalPDF k t = density (orbitalDistr k) t

-- | CDF of the performance model.
orbitalCDF :: Double -> Double -> Double
orbitalCDF k t = cumulative (orbitalDistr k) t
