-- |
-- Module Analysis.PerfModel.Orbital
--
-- A custom probability distribution for driver laptimes.
--
-- In the PDF, t = 0 is taken to be the ideal laptime in a track. The PDF
-- rises from zero at t = 0, reaches a peak and then tapers off asymptotically
-- to zero.
module Analysis.PerfModel.Orbital
    ( orbitalPDF
    , orbitalCDF
    , perfWP
    , eloWP
    , kFromRating
    , ratingFromK
    , OrbitalDistribution(..)
    ) where

import qualified Numeric.Interpolation.NodeList as NodeList
import qualified Numeric.Interpolation.Type as Interpolation.Type
import qualified Numeric.Interpolation.Piecewise as Piecewise
import Numeric.RootFinding
import Statistics.Distribution

import Data.Default.Class

-- | PDF of the performance model.
orbitalPDF :: Double -> Double -> Double
orbitalPDF k t = 4 * k^3 * t^2 * exp (-2*k*t)

-- | CDF of the performance model.
orbitalCDF :: Double -> Double -> Double
orbitalCDF k t = 1 - (2  * (k*t + 1) * k*t + 1) * exp (-2*k*t)

-- | Quantiles of the performance model.
orbitalQuantile :: Double -> Double -> Double
orbitalQuantile k p
    | p == 0 = 0
    | p == 1 = inf
    -- The "not bracketed" error can happen if p is too large and k is too
    -- low. Setting the upper bound of the root finding algorithm at 100/k
    -- appears sufficient to stave that off.
    | p > 0 && p < 1 = case ridders def (0, 100/k) (fCrossing p) of
        NotBracketed -> error $ errPfx ++ "fCrossing is not bracketed"
        SearchFailed -> error $ errPfx ++ "convergence failure"
        Root t -> t
    | otherwise = error $ errPfx ++ "p outside of [0, 1] range"
    where
    inf = 1/0
    fCrossing p t = orbitalCDF k t - p
    errPfx = "Analysis.PerfModel.Orbital.orbitalQuantile: "

-- | Winning probability of a racer with k = u against a racer with k = v,
-- according to the performance model.
perfWP :: Double -> Double -> Double
perfWP u v = (u^5 + 5*u^4*v + 10*u^3*v^2) / (u + v)^5

-- | Elo-based winning probability.
eloWP :: Double -> Double -> Double
eloWP ru rv = 1 / (1 + 10**(-(ru-rv)/400))

-- | k-to-rating conversion (see step 3 in the Analysis.PerfModel
-- introduction).
ratingFromK :: Double -> Double
ratingFromK v = ru + (400 / log 10)
    * log ((10*u^2*v^3 + 5*u*v^4 + v^5) / (u^5 + 5*u^4*v + 10*u^3*v^2))
    where
    ru = 1800
    u = 90

-- | rating-to-k interpolator in the [0..5000] range.
interpol :: NodeList.T Double Double
interpol = NodeList.fromList (zip rs ks)
    where
    rs = ratingFromK <$> ks
    ks = [0..5000]

-- | rating-to-k conversion (see step 4 in the Analysis.PerfModel
-- introduction).Workable input ratings range from -150 to 3500
kFromRating :: Double -> Double
kFromRating = kFromRatingI

kFromRatingI :: Double -> Double
kFromRatingI r = Piecewise.interpolate Interpolation.Type.linear interpol r

-- | Alternative implementation of rating-to-k conversion, using root
-- finding rather than interpolation.
kFromRatingR :: Double -> Double
kFromRatingR rv =
    case ridders def range fCrossing of
        NotBracketed -> error $ errPfx ++ "fCrossing is not bracketed"
        SearchFailed -> error $ errPfx ++ "convergence failure"
        Root t -> t
    where
    -- Factoid: exp (1800/400) ~ 90
    ru = 1800
    u = 90
    -- The range is suitable for rv between -inf and ~50000, which should
    -- suffice for most practical purposes.
    range = (0, max 50 (exp ((5/4) * rv/400)))
    fCrossing v = perfWP u v - eloWP ru rv
    errPfx = "Analysis.PerfModel.Orbital.kFromRating: "

newtype OrbitalDistribution = OrbitalDistribution { orbitalK :: Double }
    deriving (Eq, Show, Ord)

instance Distribution OrbitalDistribution where
    cumulative = orbitalCDF . orbitalK

instance ContDistr OrbitalDistribution where
    density = orbitalCDF . orbitalK
    quantile = orbitalQuantile . orbitalK

instance ContGen OrbitalDistribution where
    genContVar = genContinuous
