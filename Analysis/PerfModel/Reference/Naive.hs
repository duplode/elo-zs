-- |
-- Module: Analysis.PerfModel.Reference.Naive
--
-- Slow but known correct implementation of Analysis.PerfModel, for testing
-- purposes.
--
-- An experimental approach to obtaining strengths:
--
-- 1. Assume time results for racers fit a distribution with PDF
--    p = 4 * k^3 * t^2 * exp (-2*k*t) , with t ranging from 0 to positive
--    infinity, as defined in Analysis.PerfModel.Orbital .
--
-- 2. Calculate victory probabilities in a match from each racer's k
--     parameter.
--
-- 3. For given values of k and a fixed opponent, assume victory
--    probabilities from the Elo ratings and from the distribution coincide
--    and accordingly calculate the rating that corresponds to k. (The fixed
--    opponent is taken to have 1800 rating and an empirically chosen value of
--    k).
--
-- 4. Use interpolation to invert the function from the previous step, thus
--     obtaing a way of finding the k that corresponds to a rating.
--
-- 5. Use https://stats.stackexchange.com/a/44142 to obtain the victory
--    probability of a 1500-rated racer given a field of racers, and use that
--    as a measure of strength.
--
-- In the PDF, t = 0 is taken to be the ideal laptime in a track. The PDF
-- rises from zero at t = 0, reaches a peak and then tapers off asymptotically
-- towards zero. Higher k values mean an earlier peak, and thus laptimes
-- typically closer to the ideal, and also a narrower distribution, and thus
-- more consistent laptimes. It is a very simple model, but one that makes at
-- least some sense in the context of its application and is quite tractable
-- (notably, both the CDF and the probabilities in step 2 can be worked out
-- analytically).
module Analysis.PerfModel.Reference.Naive
    ( perfModelStrength
    , perfModelTopStrength
    ) where

import Analysis.PerfModel.Orbital

import qualified Numeric.Integration.TanhSinh as Integration

import Data.Profunctor
import qualified Control.Foldl as L
import Data.Bifunctor
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

-- | See step 5 of the introduction.
raceWinPDF :: [Double] -> Double -> Double -> Double
raceWinPDF rs r t = orbitalPDF (kFromRating r) t
    * L.fold (lmap winVersus L.product) rs
    where
    winVersus r' = 1 - orbitalCDF (kFromRating r') t

-- | Integrating the race win PDF gives the likelihood of victory, whose
-- reciprocal we use as a strength metric.
perfModelStrength :: [Double] -> Double
perfModelStrength rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (raceWinPDF rs 1500)

-- | From a list prefix of n elements, generates all combinations of k
-- elements, paired with the remaining n - k elements.
combinations
    :: Int  -- ^ Length of the list prefix from which the elements will be
            -- drawn.
    -> Int  -- ^ Size of the combinations to be generated.
    -> [a]
    -> [([a], [a])]
-- Asqing for the list prefix length is not terribly elegant, but with short
-- input lists it shouldn't be a big deal to compute the length upfront.
combinations n k as = go n k (take n as)
    where
    go 0 _ _ = [([], [])]
    go n 0 as = [([], as)]
    go _ _ [] = [([], [])]
    go n k (a : as)
        | n < k = [([], [])]
        | n == k = [(a : as, [])]
        | otherwise = map (first (a :)) (go (n - 1) (k - 1) as)
            ++ map (second (a :)) (go (n - 1) k as)

bnom n k = product [(n-k+1)..n] `div` product [1..k]

-- | Like 'raceWinPDF', but for an arbitrary position on the scoreboard.
positionPDF :: Int -> [Double] -> Double -> Double -> Double
positionPDF pos rs r t = orbitalPDF (kFromRating r) t
    * L.fold L.sum (L.fold L.product . toFactors t <$> combos)
    where
    n = length rs
    combos :: [([Double], [Double])]
    combos = combinations n (pos-1) rs
    toFactors :: Double -> ([Double], [Double]) -> [Double]
    toFactors t (above, below) =
        map loseVersus above ++ map winVersus below
    loseVersus r' = orbitalCDF (kFromRating r') t
    winVersus r' = 1 - loseVersus r'

-- | Like 'positionPDF', but for a top-N finish.
topPDF :: Int -> [Double] -> Double -> Double -> Double
topPDF pos rs r t = L.fold (lmap (posPDF t) L.sum) validPositions
    where
    validPositions = zipWith const [1..pos] (r : rs)
    posPDF t i = positionPDF i rs r t

-- | Like 'perfModelStrength', but for a top-N finish. Note that the
-- computational cost grows very quickly with the length of the list of
-- ratings.
perfModelTopStrength :: Int -> [Double] -> Double
perfModelTopStrength pos rs = 1 / integ
    where
    integ = Integration.result . Integration.absolute 1e-6 $
        Integration.nonNegative Integration.trap (topPDF pos rs 1500)

example :: [Double]
example = [2200,2100,1900,1870,1850]

-- >$> (length $ combinations 12 6 [1..12]) == bnom 12 6
--
-- >$> perfModelStrength [2200,2100,1900,1870,1850]
--
-- >$> perfModelTopStrength 5 [2200,2100,1900,1870,1850]

{-
-- This implementation is not ideal. In particular, it is insufficiently lazy,
-- as can be shown by specifying a long enough prefix. Since the prefixes used
-- here are all relatively short, we can live with that for our current
-- purposes.
combinations
    :: Int  -- ^ Length of the list prefix from which the elements will be
            -- drawn.
    -> Int  -- ^ Size of the combinations to be generated.
    -> [a]
    -> [([a], [a])]
combinations n k = map (bimap snd snd) . foldr op [((0, []), (0, []))] . take n
    where
    op a = concatMap (\ (((sy, yea), (sn, nay))) ->
        let sy' = sy + 1
            sn' = sn + 1
        in if sy < k && sn < n - k
        then [((sy, yea), (sn', a : nay)), ((sy', a : yea), (sn, nay))]
        else if sy == k
        then [((sy, yea), (sn', a : nay))]
        else if sn == n - k
        then [((sy', a : yea), (sn, nay))]
        else [])
        -}
