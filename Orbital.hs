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
    ( deltaWP
    , eloWP
    , initialRating
    , kFromRating
    , ratingFromK
    , OrbitalDistribution
    , orbitalDistr
    , orbitalK
    ) where

import Numeric.SpecFunctions (choose)
import Statistics.Distribution
import Statistics.Distribution.Gamma
import Data.MemoTrie
import Data.List (foldl')

-- | The base implementation of victory probabilities for performances
-- modelled by gamma distributions with the same (integer) shape.
ratioWP :: Int -> Double -> Double
ratioWP gsh = \w -> w^gsh * evalPoly (memoWpCoefficient gsh) (gsh-1) w

-- | A strict pair type.
data SP a b = SP !a !b

-- | A polynomial evaluator. For our immediate purposes, it performs slightly
-- better than the one available in 'Numeric.Polynomial'.
evalPoly :: (Int -> Double) -> Int -> Double -> Double
evalPoly coef n x = y
    where
    SP xn y = foldl' alg (SP 1 0) [0..n]
    alg (SP xx s) m = SP (x * xx) (coef m * xx + s)

-- | Coefficients of the polynomial used in the gamma winning probability
-- calculation.
wpCoefficient :: Int -> Int -> Double
wpCoefficient gsh m =
    (-1)^m * choose gsh m * choose (2*gsh-1) (gsh-1)
        * fromIntegral (gsh-m) / fromIntegral (gsh+m)

-- | Memoised version of 'wpCoefficient'.
memoWpCoefficient :: Int -> Int -> Double
memoWpCoefficient = memo2 wpCoefficient

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
-- The formula used here is made of two factors:
--
-- - A base correction, which on its own would adjust the derivative of
--   @deltaWP gsh@ to match that of @deltaWP 1@ at zero. This factor, which is
--   the best adjustment for rating gaps close to zero, slightly increases
--   the absolute excess winning probabilities over the entire range of gaps.
--
-- - An additional correction, which reduces the aforementioned bias. Its value
--   is chosen to (approximately) minimise the sum of the squares of the
--   differences between @ratioWP gsh@ and @ratioWP 1@ over the entire range
--   of ratios.
eloGammaCorrection :: Int -> Double
eloGammaCorrection gsh = extraAdjustment * baseCorrection
    where
    gsh' = fromIntegral gsh
    baseCorrection = 4^(gsh-1) / (gsh' * choose (2*gsh-1) (gsh-1))
    fittedConstant = (2*sqrt 29 - 3)*pi
    extraAdjustment = 1 - (gsh'-1) / (gsh' * fittedConstant)

-- | Elo conversion factor. Amounts to eloAlpha for shape 1, in which case the
-- gamma model coincides with the conventional Elo system.
eloFactor :: Int -> Double
eloFactor gsh = eloGammaCorrection gsh * eloAlpha

-- | Memoised Elo conversion factor.
memoEloFactor :: Int -> Double
memoEloFactor = memo eloFactor

-- | Winning probabilities for (scaled) rating differences. Ratings are
-- essentially logarithms of the gamma distribution rate parameters.
deltaWP :: Int -> Double -> Double
deltaWP gsh = \d -> ratioWP gsh (1 / (1 + exp(- memoEloFactor gsh * d)))
-- If ru = log u, rv = log v, and d = ru - rv, then
-- u / (u + v) = 1 / (1 + exp (-d))
-- Another way of expressing the ratio is given by
-- 1 / (1 + exp(-d)) = (1 + tanh (d/2)) / 2

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
-- Using memoEloFactor instead of eloFactor here doesn't seem to improve
-- performance.

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
