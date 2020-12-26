module Main where

import Zak
import Analysis.Simulation (SimOptions(..))

import qualified Text.Tabular.Csv
import Data.Function
import Data.Default.Class
import System.Random.MWC
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    seed <- save =<< createSystemRandom
    let simOpts = def
            { simSeed = Just seed
            }
    tab <- demoSimStrength simOpts
    tab & Text.Tabular.Csv.render id id id
        & writeFile "test.csv"
    T.writeFile "last-seed.txt" ((T.pack . show . fromSeed) seed)
