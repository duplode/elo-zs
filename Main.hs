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
    let eopts = def -- { simRuns = 100000 }
        -- eopts = eoptsDirect
    (tab, newSeed) <- runSimM (Just seed) $ demoSimStrength eopts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)

perfMain = do
    let eopts = eoptsSmoothTest  -- def
    -- let eopts = eoptsDirect
    tab <- demoPerfTopStrength' eopts
    demoToCsv "test2.csv" tab

ndcgSimMain = do
    seed <- save =<< createSystemRandom
    let eopts = def { simRuns = 1000 }
        -- eopts = eoptsDirect
    (tab, newSeed) <- runSimM (Just seed) $ demoNdcgSim eopts
    demoToCsv "test.csv" tab
    T.writeFile "last-seed.txt" $ T.pack . show $
        (fromSeed seed, fromSeed newSeed)

-- A reasonable default when using the Direct modules.
eoptsDirect = def { eloModulation = 4/75 }

-- Options for the experiments with smooth provisional factors.
eoptsSmoothTest = def
    { eloModulation = 18
    , eloProvisionalGraduation = 12
    , eloProvisionalStrategy = SmoothFactorProvisional
    , eloProvisionalFactor = 2.034
    }
