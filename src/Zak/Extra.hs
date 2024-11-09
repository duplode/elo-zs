-- |
-- Module: Zak.Extra
--
-- Experimental or superseded demos.
module Zak.Extra where

import Types
import Tabular
import Zak.Results
import Zak.Misc
import Analysis.Extra
import Util.Lone

import Data.Maybe
import Data.List
import Data.Ord
import Control.Comonad
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.Tabular as Tab
import qualified Control.Scanl as LS
import qualified Data.List.NonEmpty as N
import Data.Default.Class

demoWeighedScores :: EloOptions -> RaceIx -> Tab.Table String String String
demoWeighedScores eopts selRaceIx = testData def
    & LS.scan (weighedScores eopts)
    & (!! selRaceIx)
    & N.toList . extract
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(Result p sc) -> [T.unpack p, show sc])

-- | Sum of weighed scores over a span of races, discarding the three worst
-- results.
demoWeighedSeason
    :: EloOptions
    -> RaceIx -- ^ Initial race.
    -> Int -- ^ Number of races.
    -> Tab.Table String String String
demoWeighedSeason eopts start delta  = testData def
    & LS.scan (Map.fromList . fmap (\(Result p sc) -> (p, sc))
            . N.toList . extract
        <$> weighedScores eopts)
    & sortBy (comparing (Down . snd)) . Map.toList
        . fmap (sum . take (delta - 3) . sortBy (comparing Down))
        . foldr (Map.unionWith (++)) Map.empty
        . fmap (fmap (:[]))
        . take delta . drop start
    & arrangeTable
        (fmap show . zipWith const [1..])
        ["Racer", "Score"]
        (\(p, sc) -> [T.unpack p, show sc])

demoScoreHistory :: EloOptions -> PipId -> Tab.Table String String String
demoScoreHistory eopts p = testData def
    & LS.scan (fmap (map result
                . filter (\r -> pipsqueakTag r == p) . N.toList)
        <$> weighedScores eopts)
    & catMaybes . fmap (listToMaybe . codistributeL)
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        [T.unpack p]
        ((:[]) . show . extract)

demoWeighedStrength :: EloOptions -> Tab.Table String String String
demoWeighedStrength eopts = testData def
    & LS.scan (weighedStrength eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])

demoReinvertedStrength :: EloOptions -> Tab.Table String String String
demoReinvertedStrength eopts = testData def
    & LS.scan (reinvertedStrength eopts)
    & sortBy (comparing (Down . extract))
    & arrangeTable
        (fmap (toZakLabel . raceIx))
        ["Ix", "Strength"]
        (\(AtRace ri x) -> [show ri, show x])
