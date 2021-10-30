{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
-- |
-- Module: Zak.Results
--
-- Race results from zak.stunts.hu, used as sample data.
module Zak.Results
    ( testData
    ) where

import Types
import Tidying

import qualified Data.List.NonEmpty as NE
import Data.Bool

import System.FilePath
import Data.FileEmbed
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Csv as Csv
import Data.Function
import Data.List
import Data.Maybe

-- | Preprocessed sample data, ready for consumption.
testData :: DataPreparationOptions -> [NE.NonEmpty Standing]
testData opts = processRaceResults <$> zakParsedCsv
    where
    processRaceResults = tidyRanks
        . NE.fromList
        . bool id (ghostbuster ghostList) (removeGhosts opts)

-- | List of player identifiers to remove from the raw data.
ghostList :: [PipId]
ghostList =
    [ "AlanChuytrix70"
    , "Andrea Sacchi"
    , "BeerBor"
    , "Bolo Yeung"
    , "Boller Jani"
    , "Chicken in the oven"
    , "CrazyDriver"
    , "Darius Wojtek"
    , "Das Fass"
    , "Desert Ice"
    , "Dottore"
    , "Dragonfly"
    , "Esteban"
    , "Freeza"
    , "FTC Stormy Scamps"
    , "Gagarin"
    , "Gelato Baker"
    , "gigi"
    , "H-Bomb"
    , "Herr Lambo"
    , "Izirajder"
    , "Ladislao77"
    , "Milka Hakkinen"
    , "Mr. Combo"
    , "Navras"
    , "Nils"
    , "Otzi"
    , "Paliklaci"
    , "Pasquale"
    , "Quissi"
    , "Rocky Racer"
    , "Rokker Zsolti"
    , "Super Pursuit Mode"
    , "twyll"
    , "Ukyo Katayama"
    , "Ursin"
    , "Varalica"
    , "X-tian"
    , "Youri"
    , "Werda"
    , "Wrecking Punk"
    , "XTorres"
    , "Zweigelt"

    , "Miro"
    , "Mark Nailwood (ghost)"
    , "Geovani da Silva (ghost)"
    , "oh yeah"
    , "Xianthi"

    , "Vector"
    , "Chicago Striker"
    , "Short Cutfinder"

    , "Bernie Rubber"
    , "Herr Otto Partz"
    , "Joe Stallin"
    , "Cherry Chassis"
    , "Helen Wheels"
    , "Skid Vicious"
    ]

zakRawData :: B.ByteString
zakRawData = $(embedFile ("Zak" </> "zakstunts-race-positions.csv"))

-- | Data from the ZakStunts results Csv, grouped by race and processed up
-- to the removal of excluded results (ghosts aren't handled here yet).
zakParsedCsv :: [[Result PipId Int]]
zakParsedCsv = V.toList zakDecoded
    & groupBy ((==) `on` (\(_,t,_,_,_,_) -> t))
    & fmap (mapMaybe mkResult)
    where
    zakDecoded :: V.Vector (Int, T.Text, PipId, Int, Int, Maybe T.Text)
    zakDecoded = case Csv.decode Csv.HasHeader (BL.fromStrict zakRawData) of
        Right dat -> dat
        Left _ -> error "Bad data from Zak/zakstunts-race-positions.csv"
    mkResult (_, t, p, x, _, ex) = case ex of
        Nothing -> Just (Result p x)
        Just "MSC" -> Just (Result p x)
        Just _ -> Nothing

-- Meaning of the status codes in the CSV:
--
-- - DSQ: Disqualification, from a 00's ZakStunts race (back then,
--   disqualifications were handed by setting the laptime to 9:99.99 rather
--   than deleting the race entry outright).
--
-- - INV: Invalid replay, discovered upon review.
--
-- - MSC: CTG's 2014 races.
--
-- - EX1: Laptime beyond the "300% and 2 SD" cutoff.
--
-- - EX2: Lap driven under an alternative ruleset, such as GAR.
--
-- - EX3: Lap driven with a clearly uncompetitive car.
