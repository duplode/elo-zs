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

import Data.Ini

import qualified Data.List.NonEmpty as NE
import Data.Bool
import Data.Function ((&))
import Data.Ord
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (fromJust)  -- YOLO
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import Control.Monad

-- | Preprocessed sample data, ready for consumption.
testData :: IO [NE.NonEmpty Standing]
testData = traverse (parseScoreboard . (</> "thisrace.sb")) =<< raceDirs
    where
    baseDir = "R4K" </> "data"
    raceDirs = do
        dirs <- map (baseDir </>) <$> listDirectory baseDir
        List.sort <$> filterM doesDirectoryExist dirs

parseScoreboard :: String -> IO (NE.NonEmpty Standing)
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
    shouldBeIncluded sect = List.lookup "style" sect == Just "RH"
        && List.lookup "competing" sect == Nothing
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
