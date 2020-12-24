module Main where

import Zak

import qualified Text.Tabular.Csv
import Data.Function

main :: IO ()
main = demoSimStrength 10000
    >>= (\tab -> tab & Text.Tabular.Csv.render id id id
        & writeFile "test.csv")
