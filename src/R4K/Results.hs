{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: R4K.Results
--
-- Race results from www.raceforkicks.com, used as sample data. For the
-- moment, the code here is very unpolished.
module R4K.Results
    ( testData
    ) where

import Types
import Tidying

import Data.Ini

import qualified Data.List.NonEmpty as NE
import Data.Bool
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Ord
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (fromJust)  -- YOLO
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Control.Monad

-- Note that for the moment what follows is strictly for testing. At a
-- minimum, we'd have to fix the hardcoded paths.

-- | Preprocessed sample data, ready for consumption.
testData :: IO [NE.NonEmpty Standing]
testData = (traverse
        (fmap tidyRanks . parseScoreboard . (</> "thisrace.sb")) =<< raceDirs)
    -- <&> (++ extraRaces)
    where
    baseDir = "R4K" </> "data"
    raceDirs = do
        dirs <- map (baseDir </>) <$> listDirectory baseDir
        List.sort <$> filterM doesDirectoryExist dirs

parseScoreboard :: String -> IO (NE.NonEmpty (Result PipId Int))
parseScoreboard path = do
    eIni <- readIniFile path
    case eIni of
        Left _ -> error ("R4K.Results.parseScoreboard: failure at file " ++ path)
        Right ini -> return $ ini
            & iniSections
            & HashMap.elems
            & filter shouldBeIncluded
            & map toPreEntry
            & unifyAndSort
            & makeEntries
    where
    shouldBeIncluded sect = List.lookup "competing" sect == Nothing
        -- && List.lookup "style" sect == Just "RH"
    toPreEntry sect =
        ( fromJust $ List.lookup "name" sect
        , read . Text.unpack . fromJust $ List.lookup "lap" sect :: Int
        )
    unifyAndSort = List.sortBy (comparing snd)
        . HashMap.toList . HashMap.fromListWith min
    makeEntries = maybe (error "R4K.Results.parseScoreboard: no entries") id
        . NE.nonEmpty . drop 1 . map fst
        . List.scanl' op (Result "" 0, 0)
        where
        op (prevRes, prevLap) (pip, lap)
            | lap == prevLap = (Result pip (result prevRes), lap)
            | otherwise  = (Result pip (result prevRes + 1), lap)

    {-
extraRaces :: [NE.NonEmpty (Result PipId Int)]
extraRaces = NE.fromList <$> [r202005, r202006]

r202005 =
    [ flip Result 1 "KyLiE"
    , flip Result 2 "Seeker1982"
    , flip Result 3 "Duplode"
    , flip Result 4 "dreadnaut"
    , flip Result 5 "afullo"
    , flip Result 6 "Cas"
    , flip Result 7 "Ralphy"
    , flip Result 8 "Ayrton"
    ]

r202006 =
    [ flip Result 1 "Seeker1982"
    , flip Result 2 "KyLiE"
    , flip Result 3 "dreadnaut"
    , flip Result 4 "Duplode"
    , flip Result 5 "Cas"
    , flip Result 6 "Igor"
    , flip Result 7 "afullo"
    , flip Result 8 "Overdrijf"
    , flip Result 9 "Ralphy"
    , flip Result 10 "Octavius"
    , flip Result 11 "Ayrton"
    , flip Result 12 "Lulisa"
    ]
    -}
