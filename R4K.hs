-- {-# LANGUAGE TypeApplications, LambdaCase #-}
-- |
-- Module: R4K
--
-- Demo functions that generate results from the R4K sample data and display
-- them. This module should eventually be cleaned up and unified with 'Zak'
-- under a common arrangement.
module R4K where

import Analysis
import Engine
import Types
import Tidying
import Tabular
import R4K.Results
import Util.Lone

import qualified Data.Map.Strict as Map
import Data.Ord
import Data.List
import qualified Data.Text as T
import Control.Arrow
import Data.Default.Class
import qualified Text.Tabular as Tab
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Comonad
import qualified Control.Scanl as LS
import qualified Control.Foldl as L
import Data.Semigroupoid
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad
import Data.Maybe

demoHighest :: IO (Tab.Table String String String)
demoHighest = testData <&> \td -> td
    & L.fold
        (highestPerPip `o`
            (distillRatings def {excludeProvisional=True} <$> finalRatings))
    & Map.toList & sortBy (comparing (Down . extract . snd))
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Race", "Rating"]
        (\(p, AtRace ri rtg) -> [T.unpack p, show ri, show rtg])

-- | Current rating and past peak rating for racers active within the last
-- 12 races.
demoCurrent :: IO (Tab.Table String String String)
demoCurrent = testData <&> \td -> td
    -- Using a prescan for highestPerPeak is intentional, as it gives the
    -- peak before the last update, which is an interesting information.
    -- To check: is the composed scan here sufficiently strict?
    & LS.scan (returnA &&& LS.prescan highestPerPip <<< ratingsFold)
    & (fmap rating . extract *** fmap extract) . last
    & uncurry (Map.intersectionWith (,))
    & Map.toList
    & sortBy (comparing (Down . snd))
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Rating", "Prev. Peak"]
        (\(p, (rtg, pk)) -> [T.unpack p, show rtg, show pk])
    where
    ratingsFold
        = distillRatings def {activityCut=Just 12, excludeProvisional=True}
        <$> allRatings

demoPerfTopStrength' :: IO (Tab.Table String String String)
demoPerfTopStrength' = testData
    >>= LS.scanM perfTopStrength'
    >>= \res -> res & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (show . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])
    & return
