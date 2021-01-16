module Main where

import Zak
import Analysis.Simulation (SimOptions(..), SimM(..), runSimM)
import Types
import Tabular

import Data.Function
import Data.Default.Class
import System.Random.MWC
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = ndcgSimMain

simMain = do
    seed <- save =<< createSystemRandom
    let simOpts = def -- { simRuns = 100000 }
    (tab, newSeed) <- runSimM (Just seed) $ demoSimStrength def simOpts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)

perfMain = do
    let eopts = def
    tab <- demoPerfTopStrength' eopts
    demoToCsv "test2.csv" tab

ndcgSimMain = do
    seed <- save =<< createSystemRandom
    let simOpts = def { simRuns = 1000 }
        eopts = def { eloModulation = 12, eloRemoteCutoff = Nothing }
    (tab, newSeed) <- runSimM (Just seed) $ demoNdcgSim eopts simOpts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)
