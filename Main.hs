module Main where

import Zak
import Types
import Tabular

import Data.Function
import Data.Default.Class
import System.Random.MWC
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = perfMain

simMain = do
    seed <- save =<< createSystemRandom
    let eopts = eoptsSmoothTest -- { simRuns = 100000 }
    (tab, newSeed) <- runSimM (Just seed) $ demoSimStrength eopts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)

perfMain = do
    let eopts = eoptsSmoothTest  -- def
    tab <- demoPerfTopStrength' eopts
    demoToCsv "test2.csv" tab

ndcgSimMain = do
    seed <- save =<< createSystemRandom
    let eopts = eoptsSmoothTest { simRuns = 1000 }
    (tab, newSeed) <- runSimM (Just seed) $ demoNdcgSim eopts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)

-- Options for the experiments with smooth provisional factors.
eoptsSmoothTest = def
    { eloModulation = 18
    , eloProvisionalGraduation = 12
    , eloProvisionalStrategy = SmoothFactorProvisional
    , eloProvisionalFactor = 1.5
    }
