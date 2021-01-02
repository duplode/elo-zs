module Main where

import Zak
import Analysis.Simulation (SimOptions(..), SimM(..), runSimM)
import Tabular

import Data.Function
import Data.Default.Class
import System.Random.MWC
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = perfMain

simMain =  do
    seed <- save =<< createSystemRandom
    let simOpts = def -- { simRuns = 100000 }
    (tab, newSeed) <- runSimM (Just seed) $ demoSimStrength simOpts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)

perfMain = do
    tab <- demoPerfTopStrength'
    demoToCsv "test2.csv" tab
