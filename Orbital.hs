{-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- |
-- Module Orbital
--
-- A probability distribution for driver laptimes, initialised from
-- Elo-like ratings.
--
-- In the PDF, t = 0 is taken to be the ideal laptime in a track.
--
-- The name "orbital" alludes to this PDF originally having been, modulo
-- constant factors, the radial probability density of the 1s hydrogen atom
-- orbital. The corresponding distribution can be obtained by setting the
-- gamma shape parameter to 3.
module Orbital
    ( perfWP
    , deltaWP
    , eloWP
    , initialRating
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

-- | Initial rating for new racers. Defined as a constant here for
-- expediteness, taking into account that making it configurable isn't as
-- essential as it is making other engine parameters configurable.
initialRating :: Double
initialRating = 1500

-- | The base Elo exponent. In essence, a logistic growth factor for expected
-- scores/winning probabilities. The denominator used here, 400, is standard
-- for chess rating computations. Using it, a 0.6 expected score corresponds
-- to a ~70.4 rating gap, and a 0.9 expected score, to a ~382 gap.
eloAlpha :: Double
eloAlpha = log 10 / 400

-- | Adjusts the k values obtained from ratings depending on the gamma shape so
-- that the same rating gaps correspond to approximately the same winning
-- probabilities.
--
-- The peculiar formula used here was obtained emprically, by
-- verfiying which factors minimised the difference between Elo and gamma model
-- winning probabilities for a range of shapes, setting up a log-log chart,
-- fitting a quadratic equation to it and picking nearby pretty fractions for
-- the coefficients. Further analysis of the gamma winning probabilities may
-- eventually result in a less arbitrary formula.
eloGammaCorrection :: Int -> Double
eloGammaCorrection gsh = gsh'**(-16/25) * exp ((3*pi^2/1000)*log gsh'^2)
    where
    gsh' = fromIntegral gsh

-- | An alternative k value adjustment, which matches the derivative of
-- @deltaWP gsh@ to that of @deltaWP 1@ at zero. Compared to
-- 'eloGammaCorrection', this adjustment is more accurate for smaller rating
-- gaps (up to about 240, while 'eloGammaCorrection' is most accurate around
-- 400). It also produces winning proabilities that are always slightly
-- above the conventional Elo ones (whereas with 'eloGammaCorrection' the
-- curves cross at points other than zero).
eloGammaCorrectionAlt :: Int -> Double
eloGammaCorrectionAlt gsh =
    4^(gsh-1) / (fromIntegral gsh * choose (2*gsh-1) (gsh-1))

-- | Elo conversion factor. Amounts to eloAlpha for shape 1, in which case the
-- gamma model coincides with the conventional Elo system.
eloFactor :: Int -> Double
eloFactor gsh = eloGammaCorrection gsh * eloAlpha

-- | Winning probabilities for (scaled) rating differences. Ratings are
-- essentially logarithms of the gamma distribution rate parameters.
deltaWP :: Int -> Double -> Double
deltaWP gsh = \d -> ratioWP gsh (1 / (1 + exp(- eloFactor gsh * d)))
-- Note that 1 / (1 + exp(-x)) = (1 + tanh (x/2)) / 2

-- | Elo-based winning probability. This version takes the ratings of the two
-- racers. Only here for reference, at least for now.
eloWP :: Double -> Double -> Double
eloWP ru rv = deltaWP 1 (ru-rv)

-- As far as the results go, the effect of referenceRating and referenceK is
-- superficial. Since they adjust all ratings in the same manner, changing
-- them does not affect the overall results. Note, though, that large values
-- of k noticeably slow down the calculations, probably by making it harder
-- for the gamma distribution algorithms.
referenceRating :: Double
referenceRating = initialRating

referenceK :: Double
referenceK = 1

-- | k-to-rating conversion. Only for reference, at least for now.
ratingFromK :: Int -> Double -> Double
ratingFromK gsh u = referenceRating + log (u / referenceK) / eloFactor gsh

-- | rating-to-k conversion.
kFromRating :: Int -> Double -> Double
kFromRating gsh r = referenceK * exp ((r - referenceRating) * eloFactor gsh)

-- | The model distribution is a special case of the gamma distribution, with
-- theta = 1/k.
newtype OrbitalDistribution = OrbitalDistribution
    { orbitalGamma :: GammaDistribution }
    deriving (Eq, Show, Distribution, ContDistr, ContGen, MaybeEntropy
        , Variance, MaybeVariance, Mean, MaybeMean)

-- | Sets up the model distribution given a rating.
orbitalDistr
    :: Int    -- ^ Shape parameter to be used.
    -> Double -- ^ Elo-like rating.
    -> OrbitalDistribution
orbitalDistr gsh r = OrbitalDistribution
    (gammaDistr (fromIntegral gsh) (1 / kFromRating gsh r))

-- | Recovers the k factor from the distribution.
orbitalK :: OrbitalDistribution -> Double
orbitalK distr = 1 / gdScale (orbitalGamma distr)

-- | Recovers the shape parameter from the distribution.
orbitalShape :: OrbitalDistribution -> Double
orbitalShape distr = gdShape (orbitalGamma distr)
