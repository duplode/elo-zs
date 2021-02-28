{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- |
-- Module Orbital
--
-- A probability distribution for driver laptimes.
--
-- In the PDF, t = 0 is taken to be the ideal laptime in a track.
--
-- The name "orbital" alludes to this PDF originally having been, modulo
-- constant factors, the radial probability density of the 1s hydrogen atom
-- orbital (that is, a gamma distribution with shape parameter 3). That is no
-- longer the case in the current implementation, though the underlying
-- distribution is still a gamma one.
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
import Numeric.Polynomial
import Numeric.SpecFunctions (choose)
import Statistics.Distribution
import Statistics.Distribution.Gamma

-- | Winning probability of a racer with k = u against a racer with k = v,
-- according to the performance model. Only here for reference, at least for
-- now.
perfWP :: Double -> Double -> Double
perfWP u v = ratioWP 1 w
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
-- players. Only here for reference, at least for now.
eloWP :: Double -> Double -> Double
eloWP ru rv = deltaWP 1 (eloAlpha*(ru-rv))

-- As far as the results go, the effect of referenceRating and referenceK is
-- superficial. Since they adjust all ratings in the same manner, changing
-- them does not affect the overall results. Note, though, that large values
-- of k noticeably slow down the calculations, probably by making it harder
-- for the gamma distribution algorithms.
referenceRating :: Double
referenceRating = 1500

referenceK :: Double
referenceK = 1

-- | k-to-rating conversion.
ratingFromK :: Double -> Double
ratingFromK u = referenceRating + log (u / referenceK) / eloAlpha

-- | rating-to-k conversion.
kFromRating :: Double -> Double
kFromRating r = referenceK * exp ((r - referenceRating) * eloAlpha)

-- | The model distribution is a special case of the gamma distribution, with
-- k = 1 and theta = 1/k.
newtype OrbitalDistribution = OrbitalDistribution
    { orbitalGamma :: GammaDistribution }
    deriving (Eq, Show, Distribution, ContDistr, ContGen, MaybeEntropy
        , Variance, MaybeVariance, Mean, MaybeMean)

-- | Sets up the model distribution given a k factor.
orbitalDistr
    :: Double -- ^ k factor of the model.
    -> OrbitalDistribution
orbitalDistr k = OrbitalDistribution (gammaDistr 1 (1/k))

-- | Recovers the k factor from the distribution.
orbitalK :: OrbitalDistribution -> Double
orbitalK distr = 1 / gdScale (orbitalGamma distr)

-- | PDF of the performance model.
orbitalPDF :: Double -> Double -> Double
orbitalPDF k t = density (orbitalDistr k) t

-- | CDF of the performance model.
orbitalCDF :: Double -> Double -> Double
orbitalCDF k t = cumulative (orbitalDistr k) t
