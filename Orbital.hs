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
    , eloAlpha
    , deltaWP
    , eloWP
    , kFromRating
    , ratingFromK
    , OrbitalDistribution
    , orbitalDistr
    , orbitalK
    ) where

import qualified Data.Vector as V
import Numeric.RootFinding
import Numeric.Polynomial
import Numeric.SpecFunctions (choose)
import Statistics.Distribution
import Statistics.Distribution.Gamma

import Data.Default.Class

-- | Winning probability of a racer with k = u against a racer with k = v,
-- according to the performance model.
perfWP :: Double -> Double -> Double
perfWP u v = ratioWP 3 w
    where
    w = u / (u + v)

-- | The base implementation of victory probabilities for performances
-- modelled by gamma distributions with the same (integer) shape.
ratioWP :: Int -> Double -> Double
ratioWP gsh = \w -> w^gsh * evaluatePolynomial w coeffs
    where
    coeffs = V.generate gsh $ \i ->
        (-1)^i * choose gsh i * choose (2*gsh-1) (gsh-1)
            * fromIntegral (gsh-i) / fromIntegral (gsh+i)

-- | Winning probabilities for (unscaled) rating differences. Ratings are
-- essentially logarithms of the gamma distribution rate parameters.
deltaWP :: Int -> Double -> Double
deltaWP gsh = \d -> ratioWP gsh (1 / (1 + exp(-d)))
-- Note that 1 / (1 + exp(-d)) = (1 + tanh (d/2)) / 2

-- | The base Elo exponent. In essence, a logistic growth factor for expected
-- scores/winning probabilities. The denominator used here, 400, is standard
-- for chess rating computations. Using it, a 0.6 expected score corresponds
-- to a ~70.4 rating gap, and a 0.9 expected score, to a ~382 gap.
eloAlpha :: Double
eloAlpha = log 10 / 400

-- | Elo-based winning probability. This version takes the ratings of the two
-- players.
eloWP :: Double -> Double -> Double
eloWP ru rv = deltaWP 1 (eloAlpha*(ru-rv))

referenceRating :: Double
referenceRating = 1800

-- Factoid: exp (1800/400) ~ 90
referenceK :: Double
referenceK = 18

-- | k-to-rating conversion (see step 3 in the Analysis.PerfModel
-- introduction).
ratingFromK :: Double -> Double
ratingFromK v = ru + log (1 / perfWP u v - 1) / eloAlpha
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
