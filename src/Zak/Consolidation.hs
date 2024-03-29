{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- |
-- Module: Zak.Consolidation
--
-- Race results from zak.stunts.hu, used as sample data.
--
-- This is an old version of Zak.Results, used for the initial export of
-- Zak/zakstunts-race-positions.csv. It is kept around for the sake of
-- documentation, as the comments here point out racer aliases and other
-- information we might also want to consolidadte at some point.
module Zak.Consolidation
    ( testData
    ) where

import Types
import Tidying
import Zak.Misc
import Tabular

import qualified Data.List.NonEmpty as NE
import Data.Bool
import qualified Data.Text as T
import qualified Text.Tabular as Tab

-- | Preliminary result list for a race, subject to preprocessing.
data ScratchResults
    = NoDraws [PipId]               -- ^ If there were no draws, the results
                                    -- of each racer can be left implied by
                                    -- their order.
    | WithDraws [Result PipId Int]  -- ^ If there were draws, explicitly
                                    -- specifying the results of each racer
                                    -- is necessary.

-- | Preprocessed sample data, ready for consumption.
testData :: DataPreparationOptions -> [NE.NonEmpty Standing]
testData opts = processRaceResults <$> crudeData
    where
    processRaceResults = tidyRanks
        . NE.fromList
        . bool id (ghostbuster ghostList) (removeGhosts opts)
        . mkResults
    mkResults = \case
        NoDraws ps -> zipWith Result ps [1..]
        WithDraws ps -> ps

-- Usage: demoToCsv "all-results.csv" resultsTable
-- Note the resulting CSV has row headers, unlike the one expected by
-- Zak.Results.
resultsTable :: Tab.Table String String String
resultsTable = processRaceResults crudeData
    where
    processRaceResults = id
        . mkTable
        . concat
        . fmap sequenceA
        . zip (toZakLabel <$> [1..])
        . fmap mkResults
    mkResults = \case
        NoDraws ps -> zipWith Result ps [1..]
        WithDraws ps -> ps
    mkTable = arrangeTable
        (fmap show . zipWith const [1..])
        ["Track", "Racer", "Rank", "Ghost", "Status"]
        (\(t, Result p rk) -> [t, T.unpack p, show rk
            , show (fromEnum (p `elem` ghostList)), ""])

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

-- | Unprocessed race scoreboards to be used as sample data.
crudeData :: [ScratchResults]
crudeData =
    -- Each line in the code layout below corresponds to a ZakStunts season.
    [ c001, c002, c003, c004, c005, c006, c007, c008, c009
    , c010, c011, c012, c013, c014, p001, c015, p002, c016, p003, c017, c018
      , c019, c020
    , c021, c022, c023, c024, c025, c026, c027, c028, c029, c030, c031, c032
      , c033
    , c034, c035, c036, c037, c038, c039, c040, c041, c042, c043, c044, c045
      , c046
    , c047, c048, c049, c050, c051, c052, c053, c054, c055, c056, c057, c058
    , c059, c060, c061, c062, c063, c064, c065, c066, c067, c068, c069, c070
    , c071, c072, c073, c074, c075, c076, c077, c078, c079
    , c080, c081, c082, c083, c084, c085, c086, c087, c088, c089, c090
    , c091, c092, c093, c094, c095, c096, c097, c098, c099, c100, c101, c102
    , c103, c104, c105, c106, c107, c108, c109, c110, c111, c112, c113, c114
    , c115, c116, c117, c118, c119, c120, c121, c122, c123, c124, c125
    , c126, c127, c128, c129, c130, c131, c132, c133, c134, c135, c136, c137
    , c138, c139, c140, c141, c142, c143, c144, c145, c146, c147, c148, c149
    , c150, c151, c152, c153, c154, c155, c156, c157, c158, c159, c160, c161
    , c162, c163, c164, c165, c166, c167, c168, c169, c170, c171, c172, c173
    , c174, c175, c176, c177, c178, c179, c180, c181, c182, c183, c184, c185
    , c186, c187, c188, c189, c190, c191, c192, c193, c194, c195, c196, c197
    , c198, c199, c200, c201, c202, c203, c204, c205, c206, c207, c208, c209
    , c210, c211, c212, c213, c214, c215, c216, c217, c218, c219, c220, c221
    , c222, c223, c224, c225, c226, c227, c228, c229, c230, c231, c232, c233
    , c234, c235, c236, c237, c238, c239, c240, c241, c242, c243
    ]

-- Events in ZakStunts history which are relevant for evaluating how the
-- rating algorithm behaves: 
-- c010 (lots of new racers)
-- p002 (throwaway race by Roy)
-- c017 (surprise podium by Zak)
-- c023 (Rotoi throwaway)
-- c049 (BJ goes from 2nd to 11th) (dampen the delta after ~16 racers?)
-- c051 (ghost infestation)
-- c055 (ghost infestation, BJ's comeback to 8th) (lots of points for Zak)
-- c058 (Ayrton reaches 2400)
-- c068 (BJ's joke replay)
-- c070 (Ayrton still 1st)
-- c079 (springbreak)
-- c082 (Ayrton reaches 2500)
-- c083 (Duplode reaches 2000)
-- c086 (Duplode reaches 2100)
-- c087 (Krys dips below 1500)
-- c092 (Duplode reaches 2200 -- there's a dip in c093)
-- c096 (Duplode reaches 2300)
-- c100 (Ayrton reaches 2600, Renato reaches 2400)
-- c101 (Duplode reaches 2400 -- dips at c102)
-- c102 (Renato reaches 2500, Gutix reaches 2400)
-- c111 (Gutix reaches 2600 -- dips at c112)
-- c122 (Gutix drops 200 points)
-- c148 (Akoss reaches 2200)
-- c153 (another BJ listfiller)
-- c160 (Akoss reaches 2300)
-- c173 (Marco reaches 2000)
-- c182 (Marco reaches 2100)
-- c184 (Marco reaches 2200)
-- c186 (FinRok reaches 2000)
-- c188 (FinRok reaches 2100)
-- c191 (Marco with only a GAR lap)
-- c193 (FinRok reaches 2200)
-- c209 (FinRok reaches 2300)
-- c210 (dreadnaut reaches 2000)
-- c214 (Overdrijf reaches 2000)

-- ZakStunts scoreboards.
--
-- The comments occasionally next to racer names indicate the non-ghost alt
-- which actually entered the race. Note that Shoegazing Leo raced under the
-- name Leo Ramone before c162.

c001 = NoDraws
        [ "Fdzierva"
        , "Ben Snel"
        , "Zak McKracken"
        , "Trx"
        , "KHR"
        , "JTK"
        , "Petmorgo"
        , "Charles"
        , "PCK"
        ]

c002 = NoDraws
        [ "Fdzierva"
        , "Zak McKracken"
        , "Ben Snel"
        , "Roy Wiegerinck"
        , "Thijs Wiegerinck"
        , "Bonzai Joe"
        , "KHR"
        , "JTK"
        , "Trx"
        , "Adam"
        , "Petmorgo"
        ]

c003 = NoDraws
        [ "Roy Wiegerinck"
        , "Bonzai Joe"
        , "Ben Snel"
        , "Zak McKracken"
        , "Petmorgo"
        , "Hot Shot"
        ]

c004 =WithDraws
        [flip Result 1 "Roy Wiegerinck"
        ,flip Result 2 "Ben Snel"
        ,flip Result 2 "Bonzai Joe"
        ,flip Result 4 "Zak McKracken"
        ,flip Result 5 "KHR"
        ,flip Result 6 "JTK"
        ,flip Result 7 "The Never Sleeping"
        ]
c005 =NoDraws
        [ "Roy Wiegerinck"
        , "Zak McKracken"
        , "Ben Snel"
        , "Sylvain Chabert"
        , "Nathan"
        , "Trx"
        , "KHR"
        ]
c006 =NoDraws
        [ "Roy Wiegerinck"
        , "Sylvain Chabert"
        , "Argammon"
        , "Gus"
        , "Zak McKracken"
        , "Paulo Ribeiro"
        , "Ben Snel"
        , "Brambi"
        , "Trx"
        , "Nick"
        ]
c007 =NoDraws
        [ "Roy Wiegerinck"
        , "Zak McKracken"
        , "Paulo Ribeiro"
        , "Ben Snel"
        , "Argammon"
        , "Ravager"
        , "Tangent"
        , "Trx"
        , "Idiot"
        ]
c008 =NoDraws
        [ "Roy Wiegerinck"
        , "Bismarck"
        , "Ben Snel"
        , "Bonzai Joe"
        , "Zak McKracken"
        , "Iribaar"
        , "Idiot"
        , "Paulo Ribeiro"
        , "Trx"
        ]
c009 =NoDraws
        [ "Bismarck"
        , "Argammon"
        , "Bonzai Joe"
        , "Ben Snel"
        , "Zak McKracken"
        , "Juank_23"
        , "Paulo Ribeiro"
        , "Idiot"
        , "Trx"
        , "Iribaar"
        ]
c010 = NoDraws
        [ "Argammon"
        , "Roy Wiegerinck"
        , "Bismarck"
        , "Usrin"
        , "Bonzai Joe"
        , "Pershing II"
        , "Ben Snel"
        , "Dario"  -- > 150%
        , "Zak McKracken"
        , "Skid Vicious"
        , "J-J"
        , "Juank_23"
        , "Ravager"
        , "Idiot"
        , "Herr Otto Partz"
        , "Trx"
        , "Joe Stallin"
        , "Helen Wheels"
        , "Bernie Rubber"  -- > 200%
        , "Cherry Chassis"
        ]
c011 = NoDraws
        [ "Roy Wiegerinck"
        , "Bonzai Joe"
        , "Pershing II"
        , "Argammon"
        , "Bismarck"
        , "Ben Snel"
        , "Dario"
        , "Usrin"
        , "Juank_23"
        , "Zak McKracken"
        , "Rui Figueiredo"
        , "Roberto"
        , "Skid Vicious"  -- > 150%
        , "Herr Otto Partz"
        , "Helen Wheels"
        , "Joe Stallin"
        , "Cherry Chassis"
        , "Bernie Rubber"  -- > 200%
        ]
c012 = NoDraws
        [ "Roy Wiegerinck"
        , "Argammon"
        , "Bonzai Joe"
        , "Ben Snel"
        , "Usrin"
        , "Akoss Poo"
        , "Bismarck"
        , "Pershing II"
        , "Dario"
        , "Fred Merc"
        , "Judge Greg"
        , "Skid Vicious"
        , "Helen Wheels"
        , "Joe Stallin"
        , "Zak McKracken"
        , "Eszter"
        , "Herr Otto Partz"
        , "Cordi MontSerrat"
        , "Roberto"
        , "Charles"
        , "Cherry Chassis"
        , "Bernie Rubber"
        ]
c013 = NoDraws
        [ "Bonzai Joe"
        , "Roy Wiegerinck"
        , "Pershing II"
        , "Akoss Poo"
        , "Ben Snel"
        , "Usrin"
        , "Bismarck"
        , "Zak McKracken"
        , "CTG"
        , "Cordi MontSerrat"
        , "Ukyo Katayama"
        , "Eszter"
        , "Short Cutfinder"  -- > 250%
        ]
c014 = WithDraws
        [flip Result 1 "Roy Wiegerinck"
        ,flip Result 2 "Ben Snel"
        ,flip Result 3 "Bonzai Joe"
        ,flip Result 4 "Bismarck"
        ,flip Result 5 "Alan Rotoi"
        ,flip Result 6 "Usrin"
        ,flip Result 7 "Akoss Poo"
        ,flip Result 7 "Zak McKracken"
        ,flip Result 9 "CTG"
        ,flip Result 10 "Pershing II"
        ,flip Result 11 "Cordi MontSerrat"
        ,flip Result 12 "Juank_23"
        ,flip Result 13 "Ukyo Katayama"
        ,flip Result 14 "Mingva"
        ,flip Result 15 "Skid Vicious"
        ,flip Result 16 "Herr Otto Partz"
        ,flip Result 17 "Eddie Brother"
        ,flip Result 18 "Helen Wheels"
        ,flip Result 19 "Joe Stallin"
        ,flip Result 20 "Eszter"
        ,flip Result 21 "Bernie Rubber"
        ,flip Result 22 "Peter Szabo"
        ,flip Result 23 "Cherry Chassis"
        ,flip Result 24 "SDK"  -- > 150%
        ]
p001 = NoDraws
        [ "Roy Wiegerinck"
        , "Bonzai Joe"
        , "Mingva"
        , "Alan Rotoi"
        , "Pershing II"
        , "Alain"
        , "Ben Snel"
        , "Usrin"
        , "Akoss Poo"
        , "Eddie Brother"
        , "CTG"
        , "Zak McKracken"
        ]
c015 = NoDraws
        [ "Roy Wiegerinck"
        , "Bonzai Joe"
        , "Alan Rotoi"
        , "Akoss Poo"
        , "Bismarck"
        , "Usrin"
        , "Ben Snel"
        , "CTG"
        , "Alain"
        , "Mingva"
        , "Pershing II"
        , "Ukyo Katayama"
        , "Zak McKracken"
        , "Juank_23"
        , "Eddie Brother"
        , "Judge Greg"
        ]
p002 = WithDraws
        [flip Result 1 "Mingva"
        ,flip Result 2 "Alan Rotoi"
        ,flip Result 3 "Alain"
        ,flip Result 4 "Akoss Poo"
        ,flip Result 4 "Usrin"
        ,flip Result 6 "Bismarck"
        ,flip Result 7 "Roy Wiegerinck"
        ,flip Result 8 "Judge Greg"
        ,flip Result 9 "CTG"
        ]
c016 = NoDraws
        [ "Alan Rotoi"
        , "Alain"
        , "Bonzai Joe"
        , "Akoss Poo"
        , "Mingva"
        , "Roy Wiegerinck"
        , "Bismarck"
        , "Zak McKracken"
        , "Krys TOFF"
        , "Eddie Brother"
        , "Usrin"
        , "Ben Snel"  -- > 150%
        , "Rui Figueiredo"
        , "Judge Greg"
        , "S@mpi"
        , "Evair E. Monenegro"  -- > 200%
        ]
p003 = NoDraws
        [ "Usrin"
        , "Akoss Poo"
        , "Alain"
        , "Mingva"
        , "Krys TOFF"
        , "Alan Rotoi"
        , "Bonzai Joe"
        , "CMG"
        , "Zak McKracken"
        , "Eddie Brother"
        , "LQR"
        , "S@mpi"  -- > 200%
        ]
c017 = WithDraws
        [flip Result 1 "Alan Rotoi"
        ,flip Result 2 "Bonzai Joe"
        ,flip Result 3 "Zak McKracken"
        ,flip Result 4 "Roy Wiegerinck"
        ,flip Result 5 "Akoss Poo"
        ,flip Result 6 "Alain"
        ,flip Result 7 "LQR"
        ,flip Result 8 "Mingva"
        ,flip Result 9 "Usrin"
        ,flip Result 9 "Ben Snel"
        ,flip Result 11 "Krys TOFF"
        ,flip Result 12 "Rui Figueiredo"
        ,flip Result 13 "CMG"
        ,flip Result 14 "Skid Vicious"
        ,flip Result 15 "Herr Otto Partz"
        ,flip Result 16 "Shoegazing Leo"
        ,flip Result 17 "Joe Stallin"  -- > 150%
        ,flip Result 17 "Helen Wheels"
        ,flip Result 19 "Dave"
        ,flip Result 20 "S@mpi"
        ,flip Result 21 "Bernie Rubber"
        ,flip Result 22 "Cherry Chassis"
        ]
c018 = NoDraws
        [ "Alan Rotoi"
        , "Alain"
        , "Bonzai Joe"
        , "Ben Snel"
        , "Zak McKracken"
        , "Mingva"
        , "Roy Wiegerinck"
        , "CTG"
        , "Bolo Yeung"
        , "Usrin"
        , "Krys TOFF"
        , "Ukyo Katayama"
        , "Shoegazing Leo"
        , "Andre Geo"
        ]
c019 = WithDraws
        [flip Result 1 "Bonzai Joe"
        ,flip Result 2 "Alan Rotoi"
        ,flip Result 3 "Argammon"
        ,flip Result 3 "Zak McKracken"
        ,flip Result 5 "Alain"
        ,flip Result 6 "Mingva"
        ,flip Result 7 "Akoss Poo"
        ,flip Result 8 "Krys TOFF"
        ,flip Result 9 "Ben Snel"
        ,flip Result 10 "CTG"
        ,flip Result 11 "Usrin"
        ,flip Result 12 "Ukyo Katayama"
        ,flip Result 13 "Bolo Yeung"
        ,flip Result 14 "Andre Geo"
        ,flip Result 15 "Shoegazing Leo"
        ,flip Result 16 "Neil McRae"
        ,flip Result 17 "Szabee"
        ,flip Result 18 "Vlad"  -- > 150%
        ]
c020 = WithDraws
        [flip Result 1 "Bonzai Joe"
        ,flip Result 2 "Alan Rotoi"
        ,flip Result 3 "Alain"
        ,flip Result 4 "Akoss Poo"
        ,flip Result 5 "Zak McKracken"
        ,flip Result 5 "Usrin"
        ,flip Result 7 "Krys TOFF"
        ,flip Result 8 "Mingva"
        ,flip Result 9 "CTG"
        ,flip Result 10 "Bolo Yeung"
        ,flip Result 11 "Shoegazing Leo"
        ,flip Result 12 "Andre Geo"
        ,flip Result 13 "Manowar"  -- > 150%
        ,flip Result 14 "Vlad"
        ,flip Result 15 "Koenovic"
        ]
c021 = NoDraws
        [ "Bonzai Joe"
        , "Alan Rotoi"
        , "Akoss Poo"
        , "Argammon"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Alain"
        , "CTG"
        , "Mingva"
        , "Bismarck"
        , "PedroAntonio"
        , "Usrin"
        , "Andre Geo"
        , "LQR"
        , "Dayan"
        , "Rudo"
        , "Doelloos"
        , "Shoegazing Leo"
        , "A.J."
        , "XDude_NL"
        , "Eric Rucker"
        , "Koenovic"  -- > 150%
        ]
c022 = WithDraws
        [flip Result 1 "Alan Rotoi"
        ,flip Result 2 "Bonzai Joe"
        ,flip Result 3 "Alain"
        ,flip Result 3 "Akoss Poo"
        ,flip Result 5 "Argammon"
        ,flip Result 6 "Zak McKracken"
        ,flip Result 7 "CTG"
        ,flip Result 8 "XDude_NL"
        ,flip Result 9 "Mingva"
        ,flip Result 10 "Doelloos"
        ,flip Result 11 "Usrin"
        ,flip Result 12 "Krys TOFF"
        ,flip Result 13 "Dayan"
        ,flip Result 14 "Rudo"
        ,flip Result 15 "Bismarck"
        ,flip Result 16 "Shoegazing Leo"
        ,flip Result 17 "PedroAntonio"
        ,flip Result 18 "Norbert Schneider"  -- > 150%
        ]
c023 = NoDraws
        [ "Bonzai Joe"
        , "Argammon"
        , "Akoss Poo"
        , "XDude_NL"
        , "Doelloos"
        , "Icecube"
        , "Mingva"
        , "Zak McKracken"
        , "Usrin"
        , "Alan Rotoi"
        , "Krys TOFF"
        , "CTG"
        , "PedroAntonio"
        , "Shoegazing Leo"
        , "Daffy Duck"  -- > 150%
        , "Tweety Bird"
        ]
c024 = WithDraws
        [flip Result 1 "Argammon"
        ,flip Result 2 "Alain"
        ,flip Result 3 "Alan Rotoi"
        ,flip Result 4 "Akoss Poo"
        ,flip Result 5 "Krys TOFF"
        ,flip Result 6 "Bonzai Joe"
        ,flip Result 7 "Usrin"
        ,flip Result 7 "CTG"
        ,flip Result 9 "Zak McKracken"
        ,flip Result 10 "Mingva"
        ,flip Result 11 "XDude_NL"
        ,flip Result 12 "Doelloos"
        ,flip Result 13 "Shoegazing Leo"
        ,flip Result 14 "Neil McRae"
        ,flip Result 15 "Daffy Duck"  -- > 150%
        ,flip Result 16 "Tweety Bird"
        ]
c025 = NoDraws
        [ "Bonzai Joe"
        , "Alain"
        , "Akoss Poo"
        , "CTG"
        , "Bolo Yeung"
        , "Usrin"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Myron"
        , "Shoegazing Leo"
        , "Mingva"
        , "Lupuszka"
        , "JTK"
        , "Neil McRae"
        , "Tweety Bird"  -- > 150%
        , "Daffy Duck"
        ]
c026 = NoDraws
        [ "Alain"
        , "Akoss Poo"
        , "Alan Rotoi"
        , "Bonzai Joe"
        , "CTG"
        , "Krys TOFF"
        , "Usrin"
        , "Zak McKracken"
        , "XDude_NL"
        , "Doelloos"
        , "Argammon"
        , "Mingva"
        , "Myron"
        , "Shoegazing Leo"
        , "Böbszlee"
        , "Dabuek"
        , "JTK"
        , "Neil McRae"
        , "Daffy Duck"  -- > 150%
        , "Tweety Bird"
        , "Schila"  -- > 200%
        ]
c027 = NoDraws
        [ "Alain"
        , "Akoss Poo"
        , "XDude_NL"
        , "Myron"
        , "Doelloos"
        , "Bonzai Joe"
        , "CTG"
        , "Zak McKracken"
        , "Usrin"
        , "Krys TOFF"
        , "satanziege"
        , "Vasilij Orlov"
        , "Dabuek"
        , "Shoegazing Leo"
        , "Böbszlee"
        , "JTK"
        , "Schila"
        , "Neil McRae"
        , "Daffy Duck"  -- > 150%
        , "Tweety Bird"
        ]
c028 = NoDraws
        [ "Alain"
        , "Akoss Poo"
        , "XDude_NL"
        , "Usrin"
        , "Doelloos"
        , "Bonzai Joe"
        , "Zak McKracken"
        , "Alan Rotoi"
        , "satanziege"
        , "Krys TOFF"
        , "Fernando Brito"
        , "Dabuek"
        , "Shoegazing Leo"
        , "Vasilij Orlov"  -- > 150%
        , "JTK"
        , "Schila"
        ]
c029 = WithDraws
        [flip Result 1 "Alain"
        ,flip Result 1 "Argammon"
        ,flip Result 3 "Krys TOFF"
        ,flip Result 4 "Mingva"
        ,flip Result 5 "Akoss Poo"
        ,flip Result 6 "Fernando Brito"
        ,flip Result 7 "Zak McKracken"
        ,flip Result 8 "Usrin"
        ,flip Result 9 "Bonzai Joe"
        ,flip Result 10 "satanziege"
        ,flip Result 11 "Alan Rotoi"  -- > 150%
        ,flip Result 12 "XDude_NL"  -- > 250%
        ,flip Result 13 "Mushi"
        ,flip Result 14 "Shoegazing Leo"
        ,flip Result 15 "Ranger"
        ,flip Result 16 "JTK"  -- > 300%
        ,flip Result 17 "Neil McRae"
        ,flip Result 18 "Dabuek"
        ,flip Result 19 "KHR"
        ]
c030 = NoDraws
        [ "Bonzai Joe"
        , "Alain"
        , "Akoss Poo"
        , "Usrin"
        , "Zak McKracken"
        , "Krys TOFF"
        , "Mingva"
        , "satanziege"
        , "Fernando Brito"
        , "XDude_NL"
        , "THORGAL"
        , "Kert Rift"
        , "Shoegazing Leo"
        , "Mushi"
        , "Ranger"  -- > 150%
        , "KHR"
        , "JTK"
        , "Daffy Duck"
        , "Tweety Bird"  -- > 200%
        ]
c031 = NoDraws
        [ "Alain"
        , "Argammon"
        , "Akoss Poo"
        , "Mingva"
        , "CTG"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Bonzai Joe"
        , "satanziege"
        , "Fernando Brito"
        , "Ranger"
        , "XDude_NL"
        , "Floflo81"
        , "Looger"
        , "Shoegazing Leo"
        , "JTK"
        ]
c032 = NoDraws
        [ "Mingva"
        , "Alain"
        , "Argammon"
        , "Bonzai Joe"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Akoss Poo"
        , "CTG"
        , "Fernando Brito"
        , "Guga"
        , "Nigno"
        , "satanziege"
        , "Floflo81"
        , "Ranger"
        , "Shoegazing Leo"
        , "JTK"
        , "Daffy Duck"
        , "Tweety Bird"
        ]
c033 = NoDraws
        [ "Argammon"
        , "Bonzai Joe"
        , "Akoss Poo"
        , "Mingva"
        , "CTG"
        , "Zak McKracken"
        , "Krys TOFF"
        , "Usrin"
        , "Guga"
        , "satanziege"
        , "Alain"
        , "Gutix"
        , "Ranger"
        , "Shoegazing Leo"
        ]
c034 = NoDraws
        [ "Argammon"
        , "CTG"
        , "Akoss Poo"
        , "Zak McKracken"
        , "Alain"
        , "Gutix"
        , "Bonzai Joe"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Krys TOFF"
        , "Usrin"
        , "satanziege"
        , "DieselJoe"
        , "Mingva"
        , "Guga"
        , "caesar"
        , "Shoegazing Leo"
        , "Ford Perfect"
        , "Daffy Duck"
        , "Tweety Bird"
        , "Lupuszka"
        , "Ranger"
        ]
c035 = NoDraws
        [ "Argammon"
        , "Alain"
        , "Mingva"
        , "Akoss Poo"
        , "CTG"
        , "Gutix"
        , "Bonzai Joe"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Usrin"
        , "Zak McKracken"
        , "DarkChaser"
        , "satanziege"
        , "Shoegazing Leo"
        , "caesar"
        , "Guga"
        , "DieselJoe"
        , "Krys TOFF"
        , "JTK"
        , "Tweety Bird"
        ]
c036 = NoDraws
        [ "Gutix"
        , "CTG"
        , "Akoss Poo"
        , "Bonzai Joe"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Usrin"
        , "Krys TOFF"
        , "Zak McKracken"
        , "DarkChaser"
        , "DieselJoe"
        , "Mingva"
        , "Alain"
        , "satanziege"
        , "Shoegazing Leo"
        , "Guga"
        ]
c037 = NoDraws
        [ "Gutix"
        , "Akoss Poo"
        , "CTG"
        , "Krys TOFF"
        , "Usrin"
        , "Argammon"
        , "Bonzai Joe"
        , "Zak McKracken"
        , "Alain"
        , "DarkChaser"
        , "satanziege"
        , "Shoegazing Leo"
        , "CM"
        ]
c038 = NoDraws
        [ "Gutix"
        , "Akoss Poo"
        , "CTG"
        , "Bonzai Joe"
        , "Argammon"
        , "Mingva"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Usrin"
        , "DarkChaser"
        , "satanziege"
        , "Shoegazing Leo"
        ]
c039 = NoDraws
        [ "Alain"
        , "Bonzai Joe"
        , "Gutix"
        , "Mingva"
        , "Akoss Poo"
        , "Usrin"
        , "CTG"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "DarkChaser"
        , "satanziege"
        , "DieselJoe"
        , "Mr.Hiccup"
        , "Shoegazing Leo"
        , "lise"  -- Rule #1 exclusion.
        ]
c040 = NoDraws
        [ "Alain"
        , "Gutix"
        , "Akoss Poo"
        , "CTG"
        , "Zak McKracken"
        , "Bonzai Joe"
        , "Krys TOFF"
        , "Mingva"
        , "satanziege"
        , "DarkChaser"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "DieselJoe"
        , "TurboJack"
        , "Shoegazing Leo"
        , "DiegoSM"
        , "Cyberman"
        , "Orgona"
        ]
c041 = NoDraws
        [ "Gutix"
        , "Akoss Poo"
        , "Alain"
        , "Bonzai Joe"
        , "Usrin"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Krys TOFF"
        , "CTG"
        , "DieselJoe"
        , "Zak McKracken"
        , "satanziege"
        , "DarkChaser"
        , "TurboJack"
        , "Shoegazing Leo"
        , "Mr.Hiccup"
        , "Cyberman"
        , "Trx"  -- "TrX"
        ]
c042 = NoDraws
        [ "Bonzai Joe"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Akoss Poo"
        , "Gutix"
        , "Usrin"
        , "Krys TOFF"
        , "DarkChaser"
        , "DieselJoe"
        , "Zak McKracken"
        , "Alain"
        , "satanziege"
        , "Shoegazing Leo"
        , "lise"
        , "Dinmor"
        , "KHR"
        , "DarkLady"
        , "JTK"
        ]
c043 = WithDraws
        [ flip Result 1 "Gutix"
        , flip Result 2 "Akoss Poo"
        , flip Result 3 "Mingva"
        , flip Result 4 "Bonzai Joe"
        , flip Result 5 "Zak McKracken"
        , flip Result 6 "Alan Rotoi"  -- "Stunts Oracle"
        , flip Result 7 "DarkChaser"
        , flip Result 8 "CTG"
        , flip Result 9 "Usrin"
        , flip Result 10 "Krys TOFF"
        , flip Result 10 "DieselJoe"
        , flip Result 12 "satanziege"
        , flip Result 13 "phoxetis"
        , flip Result 14 "lise"
        , flip Result 15 "Dinmor"
        , flip Result 16 "Shoegazing Leo"
        ]
c044 = WithDraws
        [ flip Result 1 "Gutix"
        , flip Result 2 "Bonzai Joe"
        , flip Result 3 "CTG"
        , flip Result 4 "Akoss Poo"
        , flip Result 5 "Alan Rotoi"  -- "Stunts Oracle"
        , flip Result 6 "Krys TOFF"
        , flip Result 7 "DieselJoe"
        , flip Result 7 "Zak McKracken"
        , flip Result 9 "Usrin"
        , flip Result 10 "Mingva"
        , flip Result 11 "DarkChaser"
        , flip Result 12 "satanziege"
        , flip Result 13 "lise"
        , flip Result 14 "Alain"
        ]
c045 = NoDraws
        [ "CTG"
        , "Akoss Poo"
        , "Gutix"
        , "Usrin"
        , "Bonzai Joe"
        , "DieselJoe"
        , "Krys TOFF"
        , "Mingva"
        , "DarkChaser"
        , "Zak McKracken"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Alain"
        , "satanziege"
        , "caesar"
        , "Blind Ryan"
        ]
c046 = NoDraws
        [ "Gutix"
        , "Alan Rotoi"  -- "Stunts Oracle"
        , "Bonzai Joe"
        , "Alain"
        , "Argammon"
        , "CTG"
        , "Zak McKracken"
        , "Usrin"
        , "Krys TOFF"
        , "DarkChaser"
        , "DieselJoe"
        , "Dottore"
        , "Shoegazing Leo"
        , "AbuRaf70"
        , "Ranger"
        , "Dinmor"
        , "caesar"
        , "satanziege"
        , "offthewall"
        , "Tweety Bird"
        ]
c047 = NoDraws
        [ "Alan Rotoi"
        , "Argammon"
        , "Alain"
        , "Gutix"
        , "Bonzai Joe"
        , "CTG"
        , "DarkChaser"
        , "Zak McKracken"
        , "DieselJoe"
        , "Akoss Poo"
        , "Krys TOFF"
        , "Paleke"
        , "AbuRaf70"
        , "Shoegazing Leo"
        , "Stunts GrimReaper"
        , "Dinmor"  -- "dinmor"
        , "lise"
        , "offthewall"
        , "N. Oliveira"
        , "NicoPelu"  -- DSQ
        ]
c048 = NoDraws
        [ "CTG"
        , "Bonzai Joe"
        , "Alain"
        , "Argammon"
        , "Ayrton"
        , "DarkChaser"
        , "Krys TOFF"
        , "Zak McKracken"
        , "Alan Rotoi"
        , "Dottore"
        , "DieselJoe"
        , "Usrin"
        , "satanziege"
        , "Paleke"
        , "AbuRaf70"
        , "Shoegazing Leo"
        , "Jufictus"
        , "Umberto"
        , "lise"
        , "Dinmor"  -- "dinmor"
        , "NicoPelu"
        , "Chulk"  -- DSQ
        ]
c049 = NoDraws
        [ "CTG"
        , "Gutix"
        , "Ayrton"
        , "Alain"
        , "vamologocomisso"
        , "Argammon"
        , "Chulk"
        , "DarkChaser"
        , "Dottore"
        , "Zak McKracken"
        , "Krys TOFF"
        , "Usrin"
        , "DieselJoe"
        , "Paleke"
        , "AbuRaf70"
        , "von_SPEK"
        , "Dinmor"  -- "dinmor"
        , "Bonzai Joe"
        , "SergioBaro"
        , "Shoegazing Leo"
        , "lise"
        ]
c050 = NoDraws
        [ "Alain"
        , "DarkChaser"
        , "vamologocomisso"
        , "Krys TOFF"
        , "Ayrton"
        , "Usrin"
        , "Chulk"
        , "Dottore"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Johnny-K"
        , "SergioBaro"
        , "Paleke"
        , "Shoegazing Leo"
        , "Dinmor"  -- "dinmor"
        , "satanziege"
        ]
c051 = NoDraws
        [ "Gutix"
        , "Chulk"
        , "Ayrton"
        , "Alain"
        , "Argammon"
        , "CTG"
        , "DarkChaser"
        , "Usrin"
        , "Zak McKracken"
        , "vamologocomisso"
        , "Dottore"
        , "Krys TOFF"
        , "AbuRaf70"
        , "Mingva"  -- "mingva"
        , "Dragonfly"
        , "Freeza"
        , "SergioBaro"
        , "Paleke"
        , "Johnny-K"
        , "Bonzai Joe"
        , "Shoegazing Leo"
        , "StolenBike"
        , "Chicken in the oven"
        , "satanziege"
        , "lise"
        , "Tirgen"  -- DSQ
        ]
c052 = NoDraws
        [ "Ayrton"
        , "Argammon"
        , "Akoss Poo"
        , "Gutix"
        , "CTG"
        , "Chulk"
        , "Alain"
        , "Krys TOFF"
        , "DarkChaser"
        , "Zak McKracken"
        , "vamologocomisso"
        , "Dottore"
        , "Dragonfly"
        , "AbuRaf70"
        , "Usrin"
        , "Navras"
        , "Johnny-K"
        , "SergioBaro"
        , "StolenBike"
        , "DieselJoe"
        , "Paleke"
        , "Shoegazing Leo"
        , "Tirgen"
        , "Wrecking Punk"
        , "HappyWorm"
        , "attis"
        , "TurboJack"
        , "AlanChuytrix70"  -- DSQ
        ]
c053 = NoDraws
        [ "Akoss Poo"
        , "Ayrton"
        , "Gutix"
        , "CTG"
        , "Dragonfly"
        , "Chulk"
        , "Alain"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Navras"
        , "Krys TOFF"
        , "SergioBaro"
        , "DieselJoe"
        , "Johnny-K"
        , "Paleke"
        , "Dinmor"
        , "Andrea Sacchi"
        , "satanziege"
        , "Desert Ice"
        , "Shoegazing Leo"
        , "Tirgen"
        , "FTC Stormy Scamps"
        ]
c054 = NoDraws
        [ "Ayrton"
        , "Chulk"
        , "CTG"
        , "Akoss Poo"
        , "Gutix"
        , "Usrin"
        , "DarkChaser"
        , "vamologocomisso"
        , "Alan Rotoi"
        , "Dottore"
        , "Alain"
        , "Zak McKracken"
        , "Krys TOFF"
        , "AbuRaf70"
        , "Johnny-K"
        , "Navras"
        , "Paleke"
        , "SergioBaro"
        , "Milka Hakkinen"
        , "mislav"
        , "DieselJoe"
        , "satanziege"
        , "Shoegazing Leo"
        , "lise"
        , "testerlnx"
        ]
c055 = WithDraws
        [ flip Result 1 "Bonzai Joe"
        , flip Result 2 "Ayrton"
        , flip Result 3 "CTG"
        , flip Result 4 "Alain"
        , flip Result 5 "Chulk"
        , flip Result 6 "Zak McKracken"
        , flip Result 7 "Akoss Poo"
        , flip Result 8 "Dottore"
        , flip Result 8 "Usrin"
        , flip Result 10 "DarkChaser"
        , flip Result 11 "Alan Rotoi"
        , flip Result 12 "Krys TOFF"
        , flip Result 13 "Super Pursuit Mode"
        , flip Result 14 "DieselJoe"
        , flip Result 15 "Navras"
        , flip Result 16 "Dragonfly"
        , flip Result 17 "AbuRaf70"
        , flip Result 18 "H-Bomb"
        , flip Result 19 "XTorres"
        , flip Result 20 "mislav"
        , flip Result 21 "Freeza"
        , flip Result 22 "Tirgen"
        , flip Result 23 "satanziege"
        , flip Result 24 "SergioBaro"
        , flip Result 25 "Paleke"
        , flip Result 26 "Shoegazing Leo"
        , flip Result 27 "Flying Samba Racer"
        , flip Result 28 "lise"
        , flip Result 29 "Zweigelt"
        , flip Result 30 "Ursin"  -- DSQ
        , flip Result 30 "docu"  -- DSQ
        ]
c056 = WithDraws
        [ flip Result 1 "Ayrton"
        , flip Result 2 "Gutix"
        , flip Result 3 "Alain"
        , flip Result 4 "CTG"
        , flip Result 5 "DarkChaser"
        , flip Result 6 "Dottore"
        , flip Result 7 "Bonzai Joe"
        , flip Result 8 "Akoss Poo"
        , flip Result 9 "mislav"
        , flip Result 10 "Zak McKracken"
        , flip Result 11 "Chulk"
        , flip Result 12 "Navras"
        , flip Result 13 "DieselJoe"
        , flip Result 14 "Usrin"
        , flip Result 15 "Krys TOFF"
        , flip Result 16 "AbuRaf70"
        , flip Result 17 "Freeza"
        , flip Result 18 "Shoegazing Leo"
        , flip Result 19 "Paleke"
        , flip Result 19 "BeerBor"
        , flip Result 20 "SergioBaro"
        , flip Result 21 "Marci"  -- Rule #1 exclusion.
        ]
c057 = NoDraws
        [ "Chulk"
        , "Ayrton"
        , "CTG"
        , "Alain"
        , "Mingva"
        , "Navras"
        , "Dottore"
        , "Zak McKracken"
        , "DarkChaser"
        , "mislav"
        , "Usrin"
        , "Bonzai Joe"
        , "Krys TOFF"
        , "AbuRaf70"
        , "DieselJoe"
        , "Fuoco"
        , "SergioBaro"
        , "MiDiaN"
        , "Shoegazing Leo"
        , "Marci"
        , "Paleke"
        ]
c058 = NoDraws
        [ "Ayrton"
        , "Alain"
        , "Renato Biker"
        , "Chulk"
        , "CTG"
        , "DarkChaser"
        , "Jules"
        , "vamologocomisso"
        , "Zak McKracken"
        , "Bonzai Joe"
        , "mislav"
        , "AbuRaf70"
        , "Usrin"
        , "Krys TOFF"
        , "Paleke"
        , "Shoegazing Leo"
        , "hek"
        , "Johnny-K"
        , "Marci"
        ]
c059 = NoDraws
        [ "Ayrton"
        , "Renato Biker"
        , "Alain"
        , "Bonzai Joe"
        , "CTG"
        , "Chulk"
        , "DarkChaser"
        , "Jules"
        , "Zak McKracken"
        , "mislav"  -- "Mislav"
        , "vamologocomisso"
        , "Alan Rotoi"
        , "Krys TOFF"
        , "AbuRaf70"
        , "SergioBaro"
        , "Guste"
        , "Paleke"
        , "Jr"
        , "hek"
        , "Shoegazing Leo"
        , "JTK"
        , "satanziege"
        , "lise"  -- "Lise"
        ]
c060 = WithDraws
        [ flip Result 1 "Argammon"
        , flip Result 2 "Ayrton"
        , flip Result 3 "Renato Biker"
        , flip Result 4 "Chulk"
        , flip Result 5 "Alain"
        , flip Result 6 "Bonzai Joe"
        , flip Result 7 "CTG"
        , flip Result 8 "mislav"  -- "Mislav"
        , flip Result 9 "Zak McKracken"
        , flip Result 10 "Navras"
        , flip Result 11 "DarkChaser"
        , flip Result 12 "Alan Rotoi"
        , flip Result 13 "AbuRaf70"
        , flip Result 14 "Usrin"
        , flip Result 15 "Mark L. Rivers"
        , flip Result 16 "Krys TOFF"
        , flip Result 17 "SergioBaro"
        , flip Result 18 "Shoegazing Leo"
        , flip Result 19 "Jules"
        , flip Result 20 "Paleke"
        , flip Result 21 "lise"  -- "Lise"
        , flip Result 22 "hek"
        , flip Result 23 "JTK"
        , flip Result 24 "Gelato Baker"  -- DSQ
        , flip Result 24 "Chicago Striker"  -- DSQ
        ]
c061 = NoDraws
        [ "Ayrton"
        , "CTG"
        , "Argammon"
        , "Alain"
        , "Chulk"
        , "Renato Biker"
        , "Zak McKracken"
        , "vamologocomisso"
        , "DarkChaser"
        , "Mark L. Rivers"
        , "Alan Rotoi"
        , "Bonzai Joe"
        , "AbuRaf70"
        , "mislav"  -- "Mislav"
        , "SuperBrian"
        , "SergioBaro"
        , "Krys TOFF"
        , "Paleke"
        , "satanziege"
        , "Lene"
        , "JTK"
        ]
c062 = NoDraws
        [ "Bonzai Joe"
        , "Gutix"
        , "CTG"
        , "Renato Biker"
        , "mislav"  -- "Mislav"
        , "DarkChaser"
        , "Zak McKracken"
        , "Navras"
        , "Chulk"
        , "AbuRaf70"
        , "Mark L. Rivers"
        , "SergioBaro"
        , "Krys TOFF"
        , "Alain"
        , "SuperBrian"
        , "hek"
        , "Paleke"
        , "Shoegazing Leo"
        , "Marci"
        , "Lene"
        , "JTK"
        ]
c063 = WithDraws
        [ flip Result 1 "Renato Biker"
        , flip Result 2 "Zak McKracken"
        , flip Result 3 "Bonzai Joe"
        , flip Result 4 "Ayrton"
        , flip Result 5 "Argammon"
        , flip Result 6 "DarkChaser"
        , flip Result 7 "CTG"
        , flip Result 8 "Alain"
        , flip Result 9 "Navras"
        , flip Result 10 "Chulk"
        , flip Result 11 "mislav"  -- "Mislav"
        , flip Result 12 "Mark L. Rivers"
        , flip Result 13 "vamologocomisso"
        , flip Result 14 "Krys TOFF"
        , flip Result 15 "AbuRaf70"
        , flip Result 16 "SuperBrian"
        , flip Result 17 "PedroAntonio"
        , flip Result 18 "SergioBaro"
        , flip Result 19 "Paleke"
        , flip Result 19 "hek"
        , flip Result 21 "Johnny-K"
        , flip Result 22 "JTK"
        , flip Result 23 "Marci"
        , flip Result 24 "Shoegazing Leo"
        , flip Result 25 "Lene"
        , flip Result 26 "4.44.45"  -- Rule #1 exclusion
        ]
c064 = WithDraws
        [ flip Result 1 "Renato Biker"
        , flip Result 2 "CTG"
        , flip Result 3 "Zak McKracken"
        , flip Result 4 "Bonzai Joe"
        , flip Result 5 "DarkChaser"
        , flip Result 6 "mislav"  -- "Mislav"
        , flip Result 7 "Chulk"
        , flip Result 7 "Alain"
        , flip Result 9 "AbuRaf70"
        , flip Result 10 "Krys TOFF"
        , flip Result 11 "Mark L. Rivers"
        , flip Result 12 "SuperBrian"
        , flip Result 13 "AMG"
        , flip Result 14 "DieselJoe"
        , flip Result 15 "SergioBaro"
        , flip Result 16 "hek"
        , flip Result 17 "Johnny-K"
        , flip Result 18 "PedroAntonio"
        , flip Result 19 "Marci"
        , flip Result 20 "JTK"
        ]
c065 = WithDraws
        [ flip Result 1 "Renato Biker"
        , flip Result 2 "Ayrton"
        , flip Result 3 "Bonzai Joe"
        , flip Result 4 "CTG"
        , flip Result 5 "Chulk"
        , flip Result 6 "DarkChaser"
        , flip Result 7 "Alain"
        , flip Result 8 "Mark L. Rivers"
        , flip Result 9 "AbuRaf70"
        , flip Result 10 "Zak McKracken"
        , flip Result 11 "Jules"
        , flip Result 12 "mislav"  -- "Mislav"
        , flip Result 13 "DieselJoe"
        , flip Result 13 "Krys TOFF"
        , flip Result 15 "SuperBrian"
        , flip Result 16 "AMG"
        , flip Result 17 "SergioBaro"
        , flip Result 18 "Paleke"
        , flip Result 19 "Johnny-K"
        , flip Result 20 "PedroAntonio"
        , flip Result 21 "Shoegazing Leo"
        , flip Result 22 "Quanto"
        , flip Result 23 "corneta"  -- DSQ
        ]
c066 = NoDraws
        [ "CTG"
        , "Ayrton"
        , "Bonzai Joe"
        , "Renato Biker"
        , "Alan Rotoi"
        , "Chulk"
        , "DarkChaser"
        , "Zak McKracken"
        , "mislav"  -- "Mislav"
        , "Dottore"
        , "AbuRaf70"
        , "DieselJoe"
        , "Krys TOFF"
        , "SuperBrian"
        , "Dinmor"
        , "hek"
        , "JTK"
        , "Bolo Yeung"  -- DSQ
        ]
c067 = NoDraws
        [ "Ayrton"
        , "Renato Biker"
        , "CTG"
        , "Bonzai Joe"
        , "Krys TOFF"
        , "mislav"  -- "Mislav"
        , "Chulk"
        , "AbuRaf70"
        , "Zak McKracken"
        , "SuperBrian"
        , "DieselJoe"
        , "Shoegazing Leo"
        , "Dinmor"
        , "SergioBaro"
        , "JTK"
        , "locura2584"
        ]
c068 = WithDraws
        [ flip Result 1 "Renato Biker"
        , flip Result 2 "Alain"
        , flip Result 3 "Ayrton"
        , flip Result 4 "Chulk"
        , flip Result 5 "DarkChaser"
        , flip Result 5 "CTG"
        , flip Result 7 "mislav"  -- "Mislav"
        , flip Result 8 "Dottore"
        , flip Result 9 "Zak McKracken"
        , flip Result 10 "Mark L. Rivers"
        , flip Result 11 "AbuRaf70"
        , flip Result 12 "Krys TOFF"
        , flip Result 13 "SuperBrian"
        , flip Result 14 "SergioBaro"
        , flip Result 15 "Shoegazing Leo"
        , flip Result 16 "Lene"
        , flip Result 17 "JTK"
        , flip Result 18 "Bonzai Joe"  -- Rule #1 exclusion
        ]
c069 = NoDraws
        [ "Renato Biker"
        , "CTG"
        , "Alain"
        , "vamologocomisso"
        , "Dottore"
        , "DarkChaser"
        , "Zak McKracken"
        , "Bonzai Joe"
        , "SuperBrian"
        , "Chulk"
        , "Krys TOFF"
        , "AbuRaf70"
        , "Shoegazing Leo"
        , "ThugBone"
        , "JTK"
        , "Lene"
        ]
c070 = NoDraws
        [ "Gutix"
        , "CTG"
        , "Bonzai Joe"
        , "Renato Biker"
        , "Zak McKracken"
        , "AbuRaf70"
        , "SuperBrian"
        , "Krys TOFF"
        , "hek"
        , "Shoegazing Leo"
        , "JTK"
        ]
c071 = NoDraws
        [ "Ayrton"
        , "Bonzai Joe"
        , "Dottore"
        , "Zak McKracken"
        , "SuperBrian"
        , "AbuRaf70"
        , "phoxetis"
        , "ADPunk"
        , "Paleke"
        , "Krys TOFF"
        , "JTK"
        , "AMG"  -- DSQ
        ]
c072 = NoDraws
        [ "Ayrton"
        , "Mark L. Rivers"
        , "Zak McKracken"
        , "CTG"
        , "mislav"  -- M. Schumacher
        , "Bonzai Joe"
        , "Chulk"
        , "DieselJoe"
        , "AbuRaf70"
        , "Krys TOFF"
        , "SuperBrian"
        , "Paleke"
        , "JTK"
        ]
c073 = NoDraws
        [ "CTG"
        , "Zak McKracken"
        , "Krys TOFF"
        , "SuperBrian"
        , "Chulk"
        , "AbuRaf70"
        , "Paleke"
        , "ADPunk"
        , "HTE"
        , "JTK"
        ]
c074 = WithDraws
        [ flip Result 1 "Mark L. Rivers"
        , flip Result 2 "CTG"
        , flip Result 3 "Zak McKracken"
        , flip Result 4 "Krys TOFF"
        , flip Result 5 "Chulk"
        , flip Result 6 "SuperBrian"
        , flip Result 7 "AbuRaf70"
        , flip Result 8 "JTK"
        , flip Result 9 "Paleke"
        , flip Result 10 "Mark Nailwood (ghost)"  -- DSQ
        , flip Result 10 "Geovani da Silva (ghost)"  -- DSQ
        ]
c075 = NoDraws
        [ "CTG"
        , "Chulk"
        , "Mark L. Rivers"
        , "Navras"
        , "Dottore"
        , "AbuRaf70"
        , "Krys TOFF"
        , "Paleke"
        , "jey"
        , "JTK"
        ]
c076 = NoDraws
        [ "CTG"
        , "Mark L. Rivers"
        , "Chulk"
        , "Duplode"
        , "AbuRaf70"
        , "Krys TOFF"
        , "Zak McKracken"
        , "SergioBaro"
        , "Paleke"
        , "JTK"
        ]
c077 = NoDraws
        [ "CTG"
        , "Dottore"
        , "Mark L. Rivers"
        , "Duplode"
        , "Krys TOFF"
        , "Chulk"
        , "JTK"
        ]
c078 = NoDraws
        [ "CTG"
        , "Mark L. Rivers"
        , "Duplode"
        , "AbuRaf70"
        , "Krys TOFF"
        , "Chulk"
        , "SuperBrian"
        , "Paleke"
        , "JTK"
        ]
c079 = NoDraws
        [ "Ayrton"
        , "Duplode"
        , "Chulk"
        , "Mark L. Rivers"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "mislav"
        , "Krys TOFF"
        , "Zak McKracken"
        , "SuperBrian"
        , "SergioBaro"
        , "Simon82"  -- Rule #2 exclusion
        , "satanziege"
        , "JTK"
        , "Bonzai Joe"  -- Rule #2 exclusion
        ]
c080 = NoDraws
        [ "Ayrton"
        , "Mark L. Rivers"
        , "Duplode"
        , "Chulk"
        , "AbuRaf70"
        , "Zak McKracken"
        , "SuperBrian"
        , "Usrin"
        , "Krys TOFF"
        , "AMG"
        , "satanziege"
        , "SergioBaro"
        , "JTK"
        ]
c081 = NoDraws
        [ "Ayrton"
        , "Chulk"
        , "Mark L. Rivers"
        , "Bonzai Joe"
        , "Duplode"
        , "DarkChaser"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Krys TOFF"
        , "SuperBrian"
        , "Turkey"
        , "AMG"
        , "Freddy"  -- Rule #3 exclusion
        ]
c082 = NoDraws
        [ "Ayrton"
        , "Bonzai Joe"
        , "CTG"
        , "Mark L. Rivers"
        , "AbuRaf70"
        , "Duplode"
        , "DarkChaser"
        , "Zak McKracken"
        , "SuperBrian"
        , "Chulk"
        , "Krys TOFF"
        , "Pavekiller"
        , "Freddy"
        , "Böbszlee"
        , "Turkey"
        , "AMG"
        , "Lene"  -- Rule #3 exclusion
        , "Marci"  -- Rule #3 exclusion
        ]
c083 = NoDraws
        [ "Ayrton"
        , "Duplode"
        , "Mark L. Rivers"
        , "DarkChaser"
        , "Bonzai Joe"
        , "SuperBrian"
        , "Chulk"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Krys TOFF"
        , "Pavekiller"
        , "dstien"
        , "Antti"  -- Rule #3 exclusion
        , "Lene"  -- Rule #3 exclusion
        ]
c084 = NoDraws
        [ "Ayrton"
        , "Duplode"
        , "Mark L. Rivers"
        , "CTG"
        , "Bonzai Joe"
        , "AbuRaf70"
        , "SuperBrian"
        , "Zak McKracken"
        , "Krys TOFF"
        , "dstien"
        , "Antti"
        ]
c085 = NoDraws
        [ "Ayrton"
        , "Alan Rotoi"
        , "CTG"
        , "Duplode"
        , "Mark L. Rivers"
        , "Bonzai Joe"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Krys TOFF"
        , "dstien"
        , "Antti"
        ]
c086 = NoDraws
        [ "Ayrton"
        , "Duplode"
        , "Mark L. Rivers"
        , "CTG"
        , "Akoss Poo"
        , "Alan Rotoi"
        , "SuperBrian"
        , "Bonzai Joe"
        , "AbuRaf70"
        , "Antti"
        , "Zak McKracken"
        , "dstien"
        , "Krys TOFF"
        ]
c087 = NoDraws
        [ "Ayrton"
        , "CTG"
        , "Bonzai Joe"
        , "Mark L. Rivers"
        , "Akoss Poo"
        , "Duplode"
        , "AbuRaf70"
        , "SuperBrian"
        , "Pavekiller"
        , "dstien"
        , "Antti"
        , "Zak McKracken"
        , "Krys TOFF"
        ]
c088 = NoDraws
        [ "Ayrton"
        , "CTG"
        , "Mark L. Rivers"
        , "Duplode"
        , "Bonzai Joe"
        , "Navras"
        , "Zak McKracken"
        , "SuperBrian"
        , "AbuRaf70"
        , "Krys TOFF"
        , "dstien"
        , "Pavekiller"
        , "Fredmen67"
        , "tgm"
        , "Böbszlee"
        , "Lene"
        ]
c089 = NoDraws
        [ "Ayrton"
        , "Duplode"
        , "CTG"
        , "Bonzai Joe"
        , "Zak McKracken"
        , "SuperBrian"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "Pavekiller"
        , "dstien"
        , "Krys TOFF"
        , "tgm"
        , "Fredmen67"  -- DSQ
        ]
c090 = NoDraws
        [ "Ayrton"
        , "CTG"
        , "Duplode"
        , "Alan Rotoi"
        , "SuperBrian"
        , "Zak McKracken"
        , "Mark L. Rivers"
        , "Bonzai Joe"
        , "dstien"
        , "AbuRaf70"
        , "Goblin"
        , "Krys TOFF"
        , "Fredmen67"  -- Rule #3 exclusion.
        ]
c091 = NoDraws
        [ "Bonzai Joe"
        , "CTG"
        , "Duplode"
        , "Friker"
        , "Mark L. Rivers"
        , "Zak McKracken"
        , "SuperBrian"
        , "satanziege"
        , "Krys TOFF"
        , "Pitman"
        , "AbuRaf70"
        , "dstien"
        , "DieselJoe"
        , "Pavekiller"
        , "Cyberman"
        , "firefoxie"
        , "Ano"
        , "Antti"
        , "Böbszlee"
        , "Fredmen67"
        , "Lene"
        ]
c092 = NoDraws
        [ "Ayrton"
        , "Duplode"
        , "Bonzai Joe"
        , "CTG"
        , "AbuRaf70"
        , "Zak McKracken"
        , "SuperBrian"
        , "Chulk"
        , "Friker"
        , "dstien"
        , "Cyberman"
        , "Pitman"
        , "Pavekiller"
        , "satanziege"
        , "Overdrijf"
        , "YtseDan"
        , "Blackpulsie"
        , "firefoxie"
        , "Antti"
        , "Goblin"
        ]
c093 = WithDraws
        [ flip Result 1 "Bonzai Joe"
        , flip Result 2 "CTG"
        , flip Result 3 "Zak McKracken"
        , flip Result 3 "Duplode"
        , flip Result 5 "Friker"
        , flip Result 6 "Pitman"
        , flip Result 7 "AbuRaf70"
        , flip Result 8 "Overdrijf"
        , flip Result 9 "SuperBrian"
        , flip Result 10 "firefoxie"
        , flip Result 11 "dstien"
        , flip Result 12 "satanziege"
        , flip Result 13 "HTE"
        , flip Result 14 "SisterOfMercy"
        , flip Result 15 "YtseDan"
        , flip Result 16 "Cyberman"
        , flip Result 17 "Antti"
        ]
c094 = NoDraws
        [ "Duplode"
        , "Bonzai Joe"
        , "CTG"
        , "Dottore"
        , "Navras"
        , "Friker"
        , "Zak McKracken"
        , "Chulk"
        , "SuperBrian"
        , "Overdrijf"
        , "AbuRaf70"
        , "Pitman"
        , "JoSi"
        , "Ice"
        , "dstien"
        , "satanziege"
        , "firefoxie"
        , "HTE"
        , "Cyberman"  -- Rule #3 exclusion.
        ]
c095 = NoDraws
        [ "Duplode"
        , "Bonzai Joe"
        , "CTG"
        , "Friker"
        , "Dottore"
        , "SuperBrian"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Overdrijf"
        , "Alan Rotoi"
        , "Chulk"
        , "JoSi"
        , "Ice"
        , "dstien"
        ]
c096 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "Mingva"  -- "Paradisio"
        , "Bonzai Joe"
        , "Friker"
        , "AbuRaf70"
        , "SuperBrian"
        , "Overdrijf"
        , "dstien"
        , "Usrin"
        , "JoSi"
        , "Fredmen67"
        ]
c097 = NoDraws
        [ "CTG"
        , "Duplode"
        , "Mingva"  -- "Paradisio"
        , "Friker"
        , "Bonzai Joe"
        , "SuperBrian"
        , "Zak McKracken"
        , "Usrin"
        , "Alan Rotoi"
        , "Overdrijf"
        , "Ice"
        , "AbuRaf70"
        , "JoSi"
        , "dstien"
        ]
c098 = NoDraws
        [ "Duplode"
        , "Mingva"  -- "Paradisio"
        , "CTG"
        , "SuperBrian"
        , "Roy Wiegerinck"  -- "Reiger"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Dottore"
        , "Friker"
        , "Zak McKracken"
        , "dstien"
        , "Overdrijf"
        , "Usrin"
        , "Pavekiller"
        ]
c099 = NoDraws
        [ "Duplode"
        , "Renato Biker"
        , "Mingva"  -- "Paradisio"
        , "dstien"
        , "SuperBrian"
        , "Roy Wiegerinck"  -- "Reiger"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "JoSi"
        , "Zak McKracken"
        , "PedroAntonio"
        , "satanziege"  -- Rule #3 exclusion
        , "Overdrijf"  -- DSQ
        ]
c100 = NoDraws
        [ "Ayrton"
        , "Renato Biker"
        , "Gutix"
        , "Duplode"
        , "Mark L. Rivers"
        , "Bonzai Joe"
        , "CTG"
        , "Mingva"  -- "Paradisio"
        , "Alain"
        , "Alan Rotoi"
        , "SuperBrian"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Chulk"
        , "Roy Wiegerinck"  -- "Reiger"
        , "dstien"
        , "Pavekiller"
        , "PedroAntonio"
        , "Overdrijf"
        , "SergioBaro"
        , "Usrin"
        , "Friker"
        , "Lene"
        ]
c101 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "Gutix"
        , "Mingva"  -- "Paradisio"
        , "Alan Rotoi"
        , "Alain"
        , "CTG"
        , "Zak McKracken"
        , "SuperBrian"
        , "AbuRaf70"
        , "Mark L. Rivers"
        , "JoSi"
        , "Bonzai Joe"
        , "dstien"
        , "PedroAntonio"
        ]
c102 = NoDraws
        [ "Renato Biker"
        , "Gutix"
        , "Mingva"  -- "Paradisio"
        , "Mark L. Rivers"
        , "Duplode"
        , "AbuRaf70"
        , "SuperBrian"
        , "Zak McKracken"
        , "Roy Wiegerinck"  -- "Reiger"
        , "dstien"  -- Rule #3 exclusion
        ]
c103 = NoDraws
        [ "Duplode"
        , "Gutix"
        , "SuperBrian"
        , "Renato Biker"
        , "Mingva"  -- "Paradisio"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Alan Rotoi"
        , "dstien"
        , "satanziege"
        , "Pitman"
        , "Nubarow"
        , "Fredmen67"
        , "Lene"
        ]
c104 = NoDraws
        [ "Gutix"
        , "Renato Biker"
        , "CTG"
        , "Duplode"
        , "Mingva"  -- "Paradisio"
        , "SuperBrian"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Pitman"
        , "PedroAntonio"
        , "dstien"
        , "satanziege"  -- Rule #3 exclusion
        ]
c105 = NoDraws
        [ "Gutix"
        , "Duplode"
        , "Bonzai Joe"
        , "CTG"
        , "Renato Biker"
        , "Mingva"  -- "Paradisio"
        , "SuperBrian"
        , "Zak McKracken"
        , "AbuRaf70"
        , "PedroAntonio"
        , "Pitman"
        , "dstien"
        , "satanziege"  -- Rule #3 exclusion
        ]
c106 = NoDraws
        [ "Gutix"
        , "Renato Biker"
        , "SuperBrian"
        , "Duplode"
        , "AbuRaf70"
        , "Mingva"  -- "Paradisio"
        , "Pitman"
        , "dstien"
        , "satanziege"
        , "PedroAntonio"
        , "Usrin"
        ]
c107 = NoDraws
        [ "Gutix"
        , "Renato Biker"
        , "CTG"
        , "SuperBrian"
        , "AbuRaf70"
        , "Duplode"
        , "Mingva"  -- "Paradisio"
        , "Pitman"
        , "Crazy Jimmy"
        , "dstien"
        , "satanziege"
        , "Lene"
        , "Fredmen67"  -- Rule #3 exclusion
        ]
c108 = NoDraws
        [ "Gutix"
        , "Renato Biker"
        , "CTG"
        , "SuperBrian"
        , "Duplode"
        , "AbuRaf70"
        , "Zak McKracken"
        , "dstien"
        , "Pitman"
        , "Crazy Jimmy"
        , "satanziege"
        ]
c109 = WithDraws
        [ flip Result 1 "Gutix"
        , flip Result 1 "CTG"
        , flip Result 3 "Renato Biker"
        , flip Result 4 "AbuRaf70"
        , flip Result 5 "Duplode"
        , flip Result 6 "Zak McKracken"
        , flip Result 7 "satanziege"
        , flip Result 8 "Pitman"
        , flip Result 9 "dstien"
        , flip Result 10 "Crazy Jimmy"
        , flip Result 11 "Fredmen67"
        ]
c110 = NoDraws
        [ "Gutix"
        , "AbuRaf70"
        , "Duplode"
        , "CTG"
        , "SuperBrian"
        , "Alan Rotoi"
        , "Mingva"
        , "JoSi"
        , "Overdrijf"
        , "dstien"
        , "Zak McKracken"
        , "Pitman"
        , "satanziege"  -- Rule #3 exclusion
        , "darkmaster"  -- DSQ
        ]
c111 = NoDraws
        [ "Gutix"
        , "Duplode"
        , "CTG"
        , "AbuRaf70"
        , "Renato Biker"
        , "Bonzai Joe"
        , "SuperBrian"
        , "Overdrijf"
        , "Zak McKracken"
        , "dstien"
        , "satanziege"
        , "Pitman"
        ]
c112 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "Gutix"
        , "CTG"
        , "Xianthi"
        , "AbuRaf70"
        , "Overdrijf"
        , "dstien"
        , "Zak McKracken"
        , "Pitman"
        , "Faundu"
        , "satanziege"
        ]
c113 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "Gutix"
        , "SuperBrian"
        , "Bonzai Joe"
        , "CTG"
        , "AbuRaf70"
        , "Xianthi"
        , "Alan Rotoi"
        , "dstien"
        , "Pitman"
        , "Overdrijf"
        , "satanziege"
        ]
c114 = NoDraws
        [ "Renato Biker"
        , "Bonzai Joe"
        , "SuperBrian"
        , "Mingva"
        , "Duplode"
        , "CTG"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Overdrijf"
        , "Das Fass"
        , "Alan Rotoi"
        , "dstien"
        , "Thodran"
        , "Pitman"
        , "satanziege"
        ]
c115 = NoDraws
        [ "Duplode"
        , "Gutix"
        , "Renato Biker"
        , "SuperBrian"
        , "Mingva"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Krys TOFF"
        , "dstien"
        , "Pitman"
        , "satanziege"
        , "Faundu"
        ]
c116 = NoDraws
        [ "CTG"
        , "Duplode"
        , "Renato Biker"
        , "SuperBrian"
        , "oh yeah"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "dstien"
        , "Mingva"
        , "satanziege"
        , "Pitman"  -- Rule #3 exclusion
        ]
c117 = NoDraws
        [ "Duplode"
        , "Overdrijf"
        , "AbuRaf70"
        , "Zak McKracken"
        , "dstien"
        , "Varalica"
        , "Alan Rotoi"
        , "Crazy Jimmy"  -- "Flying Dutchman"
        , "Pitman"
        , "satanziege"  -- Rule #3 exclusion
        ]
c118 = NoDraws
        [ "Alan Rotoi"
        , "Gutix"
        , "AbuRaf70"
        , "Duplode"
        , "Zak McKracken"
        , "dstien"
        , "twyll"
        , "Overdrijf"
        , "Crazy Jimmy"  -- "Flying Dutchman"
        , "satanziege"
        ]
c119 = NoDraws
        [ "Gutix"
        , "Chulk"
        , "AbuRaf70"
        , "Duplode"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "dstien"
        , "satanziege"
        , "Sanity"  -- Rule #3 exclusion
        , "Fredmen67"  -- Rule #3 exclusion
        ]
c120 = NoDraws
        [ "Duplode"
        , "Gutix"
        , "dstien"
        , "AbuRaf70"
        , "satanziege"
        , "Pitman"
        , "Shorty"  -- Rule #3 exclusion
        , "Sanity"  -- DSQ
        ]
c121 = NoDraws
        [ "Duplode"
        , "Gutix"
        , "AbuRaf70"
        , "dstien"
        , "Mingva"
        , "Pitman"
        , "Shorty"
        , "Zak McKracken"
        , "satanziege"
        ]
c122 = NoDraws
        [ "Bonzai Joe"
        , "Duplode"
        , "CTG"  -- Dexter Morgan
        , "Mingva"
        , "Shorty"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Faundu"
        , "dstien"
        , "Gutix"
        , "Pitman"
        , "grisznik"
        , "satanziege"
        ]
c123 = NoDraws
        [ "Gutix"
        , "Duplode"
        , "Bonzai Joe"
        , "SuperBrian"
        , "AbuRaf70"
        , "Shorty"
        , "Pitman"
        , "oscar"
        , "dstien"
        , "satanziege"
        , "grisznik"  -- Rule #3 exclusion
        ]
c124 = NoDraws
        [ "Duplode"
        , "CTG"
        , "SuperBrian"
        , "Gutix"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Bonzai Joe"
        , "Mingva"
        , "Shorty"
        , "dstien"
        , "Pitman"
        , "Thodran"
        , "satanziege"
        ]
c125 = NoDraws
        [ "Gutix"
        , "Duplode"
        , "AbuRaf70"
        , "Mingva"
        , "CTG"
        , "dstien"
        , "SuperBrian"
        , "Chulk"
        , "Pitman"
        , "Zak McKracken"
        , "Gagarin"
        , "Shorty"
        , "satanziege"
        , "N. Oliveira"  -- Rule #3 exclusion
        ]
c126 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Gutix"
        , "Friker"
        , "Zak McKracken"
        , "SuperBrian"
        , "Chulk"
        , "Gagarin"
        , "AbuRaf70"
        , "Pitman"
        , "Usrin"
        , "satanziege"
        , "dstien"
        , "Fredmen67"
        , "N. Oliveira"
        ]
c127 = NoDraws
        [ "Duplode"
        , "Bonzai Joe"
        , "CTG"
        , "Friker"
        , "Gutix"
        , "Gagarin"
        , "Zak McKracken"
        , "SuperBrian"
        , "Usrin"
        , "Alan Rotoi"
        , "dstien"
        , "Pitman"
        , "Shorty"  -- Rule #3 exclusion
        ]
c128 = NoDraws
        [ "Duplode"
        , "Bonzai Joe"
        , "Friker"
        , "Gutix"
        , "CTG"
        , "Gagarin"
        , "SuperBrian"
        , "AbuRaf70"
        , "Zak McKracken"
        , "dstien"
        , "Pitman"  -- Rule #3 exclusion
        , "Usrin"  -- Rule #3 exclusion
        , "Shorty"  -- Rule #3 exclusion
        ]
c129 = NoDraws
        [ "Gutix"
        , "Friker"
        , "AbuRaf70"
        , "CTG"
        , "Duplode"
        , "Zak McKracken"
        , "Usrin"
        , "Alan Rotoi"
        , "Overdrijf"
        , "Pitman"
        , "dstien"
        , "satanziege"
        ]
c130 = NoDraws
        [ "Gutix"
        , "AbuRaf70"
        , "Duplode"
        , "Overdrijf"
        , "Friker"
        , "Zak McKracken"
        , "Usrin"
        , "Alan Rotoi"
        , "dstien"
        ]
c131 = WithDraws
        [ flip Result 1 "Renato Biker"
        , flip Result 2 "Gutix"
        , flip Result 2 "Duplode"
        , flip Result 4 "CTG"
        , flip Result 5 "Usrin"
        , flip Result 6 "AbuRaf70"
        , flip Result 7 "Zak McKracken"
        , flip Result 8 "Overdrijf"
        , flip Result 9 "Friker"
        , flip Result 10 "dstien"
        , flip Result 11 "Ivan"
        , flip Result 12 "satanziege"
        , flip Result 13 "Alecu"  -- Rule #3 exclusion
        ]
c132 = NoDraws
        [ "Renato Biker"
        , "Alan Rotoi"
        , "CTG"
        , "Duplode"
        , "Chulk"
        , "Zak McKracken"
        , "Usrin"
        , "AbuRaf70"
        , "Friker"
        , "dstien"
        , "satanziege"
        ]
c133 = NoDraws
        [ "Renato Biker"
        , "Friker"
        , "CTG"
        , "Akoss Poo"
        , "Gutix"
        , "Usrin"
        , "Duplode"
        , "Zak McKracken"
        , "SuperBrian"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "dstien"
        ]
c134 = NoDraws
        [ "Renato Biker"
        , "Bonzai Joe"
        , "Duplode"
        , "CTG"
        , "Gutix"
        , "Friker"
        , "Akoss Poo"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Alan Rotoi"
        , "Usrin"
        , "dstien"
        ]
c135 = NoDraws
        [ "Duplode"
        , "Renato Biker"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "CTG"
        , "Gutix"
        , "SuperBrian"
        , "Zak McKracken"
        , "Bonzai Joe"
        , "dreadnaut"
        , "Friker"
        , "Usrin"
        , "dstien"
        ]
c136 = WithDraws
        [ flip Result 1 "SuperBrian"
        , flip Result 1 "Duplode"
        , flip Result 3 "CTG"
        , flip Result 4 "Friker"
        , flip Result 5 "Renato Biker"
        , flip Result 6 "Usrin"
        , flip Result 7 "dreadnaut"
        , flip Result 8 "dstien"
        , flip Result 9 "Krys TOFF"
        ]
c137 = NoDraws
        [ "Friker"
        , "CTG"
        , "Duplode"
        , "SuperBrian"
        , "Zak McKracken"
        , "dreadnaut"
        , "Renato Biker"
        , "dstien"
        , "Pavekiller"
        , "Usrin"
        ]
c138 = NoDraws
        [ "CTG"
        , "Chulk"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Renato Biker"
        , "Akoss Poo"
        , "Usrin"
        , "Duplode"
        , "dreadnaut"
        , "SuperBrian"
        , "Friker"
        , "Zak McKracken"
        , "dstien"
        , "ADPunk"
        , "satanziege"
        , "Fredmen67"
        , "Goyo"
        ]
c139 = NoDraws
        [ "Renato Biker"
        , "Friker"
        , "Duplode"
        , "CTG"
        , "Akoss Poo"
        , "dreadnaut"
        , "Chulk"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "ADPunk"
        , "dstien"
        , "SuperBrian"
        , "satanziege"
        ]
c140 = NoDraws
        [ "Renato Biker"
        , "CTG"
        , "Chulk"
        , "Alan Rotoi"
        , "Duplode"
        , "Friker"
        , "Zak McKracken"
        , "Akoss Poo"
        , "AbuRaf70"
        , "ADPunk"
        , "dreadnaut"
        , "dstien"
        , "JoSi"  -- Rule #3 exclusion
        ]
c141 = NoDraws
        [ "Renato Biker"
        , "CTG"
        , "Chulk"
        , "AbuRaf70"
        , "Duplode"
        , "Alan Rotoi"
        , "Miro"
        , "ADPunk"
        , "Friker"
        , "Zak McKracken"
        , "dreadnaut"
        , "Mr. P.C."
        , "JoSi"
        , "dstien"
        ]
c142 = NoDraws
        [ "CTG"
        , "Renato Biker"
        , "Friker"
        , "dreadnaut"
        , "Duplode"
        , "Miro"
        , "ADPunk"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "dstien"
        , "Mr. P.C."
        ]
c143 = NoDraws
        [ "Renato Biker"
        , "CTG"
        , "Duplode"
        , "Usrin"
        , "Friker"
        , "AbuRaf70"
        , "dreadnaut"
        , "Alan Rotoi"
        , "ADPunk"
        , "dstien"
        ]
c144 = NoDraws
        [ "Duplode"
        , "Renato Biker"
        , "CTG"
        , "Akoss Poo"
        , "DieselJoe"
        , "Miro"
        , "dreadnaut"
        , "AbuRaf70"
        , "Usrin"
        , "Zak McKracken"
        , "Alan Rotoi"
        , "dstien"
        ]
c145 = NoDraws
        [ "CTG"
        , "Renato Biker"
        , "Akoss Poo"
        , "Duplode"
        , "SuperBrian"
        , "Chulk"
        , "AbuRaf70"
        , "Usrin"
        , "Alan Rotoi"
        , "dreadnaut"
        , "Zak McKracken"
        , "ADPunk"
        , "dstien"
        , "Friker"
        ]
c146 = NoDraws
        [ "CTG"
        , "Renato Biker"
        , "Akoss Poo"
        , "Duplode"
        , "Alan Rotoi"
        , "SuperBrian"
        , "dreadnaut"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Friker"
        , "dstien"
        ]
c147 = NoDraws
        [ "CTG"
        , "Renato Biker"
        , "Akoss Poo"
        , "Duplode"
        , "Chulk"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "dreadnaut"
        , "Zak McKracken"
        , "dstien"  -- Rule #3 exclusion
        ]
c148 = NoDraws
        [ "Renato Biker"
        , "Akoss Poo"
        , "Duplode"
        , "CTG"
        , "dreadnaut"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Alan Rotoi"
        , "dstien"
        , "satanziege"  -- Rule #3 exclusion
        ]
c149 = NoDraws
        [ "Renato Biker"
        , "CTG"
        , "Akoss Poo"
        , "Duplode"
        , "Zak McKracken"
        , "SuperBrian"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "dreadnaut"
        , "Kalifa"
        , "dstien"
        ]
c150 = NoDraws
        [ "CTG"
        , "Renato Biker"
        , "Alan Rotoi"
        , "Akoss Poo"
        , "SuperBrian"
        , "Chulk"
        , "AbuRaf70"
        , "Duplode"
        , "Usrin"
        , "Zak McKracken"
        , "dreadnaut"
        , "dstien"
        , "ColonelJ"
        , "Böbszlee"
        , "Rokker Zsolti"
        , "Boller Jani"
        ]
c151 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "CTG"
        , "Akoss Poo"
        , "dreadnaut"
        , "SuperBrian"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Usrin"
        , "Rokker Zsolti"
        , "Boller Jani"
        , "dstien"
        , "afullo"
        , "sd4000"  -- "skydive4000"
        ]
c152 = NoDraws
        [ "Duplode"
        , "Renato Biker"
        , "CTG"
        , "Akoss Poo"
        , "Bonzai Joe"
        , "Zak McKracken"
        , "SuperBrian"
        , "Mingva"
        , "AbuRaf70"
        , "Rokker Zsolti"
        , "Alan Rotoi"
        , "dreadnaut"
        , "Boller Jani"
        , "Usrin"
        , "dstien"
        , "afullo"
        , "satanziege"
        ]
c153 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "Alan Rotoi"
        , "CTG"
        , "Akoss Poo"
        , "Chulk"
        , "SuperBrian"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Rokker Zsolti"
        , "Mingva"
        , "Boller Jani"
        , "dreadnaut"
        , "dstien"
        , "afullo"
        , "Alecu"  -- Alecu323  -- Rule #3 exclusion
        , "Bonzai Joe"  -- Rule #1 exclusion
        ]
c154 = NoDraws
        [ "CTG"
        , "Akoss Poo"
        , "Renato Biker"
        , "Duplode"
        , "dreadnaut"
        , "Rokker Zsolti"
        , "Boller Jani"
        , "SuperBrian"
        , "Usrin"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Mingva"
        , "dstien"
        , "afullo"  -- Rule #3 exclusion
        ]
c155 = NoDraws
        [ "CTG"
        , "Akoss Poo"
        , "Usrin"
        , "Rokker Zsolti"
        , "Boller Jani"
        , "Duplode"
        , "dreadnaut"
        , "Mingva"
        , "Zak McKracken"
        , "dstien"
        , "AbuRaf70"
        , "afullo"
        , "Alan Rotoi"  -- Rule #3 exclusion
        ]
c156 = NoDraws
        [ "CTG"
        , "Duplode"
        , "Akoss Poo"
        , "Rokker Zsolti"
        , "SuperBrian"
        , "Usrin"
        , "Boller Jani"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Mingva"
        , "dreadnaut"
        , "dstien"
        , "Alan Rotoi"  -- Rule #3 exclusion
        , "afullo"  -- Rule #3 exclusion
        ]
c157 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Bonzai Joe"
        , "Akoss Poo"
        , "Usrin"
        , "dreadnaut"
        , "Zak McKracken"
        , "Rokker Zsolti"
        , "Boller Jani"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Mingva"
        , "afullo"
        , "dstien"
        , "satanziege"
        ]
c158 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Akoss Poo"
        , "Bonzai Joe"
        , "Rokker Zsolti"
        , "Boller Jani"
        , "AbuRaf70"
        , "dreadnaut"
        , "Alan Rotoi"
        , "SuperBrian"
        , "Zak McKracken"
        , "dstien"
        , "afullo"
        , "Böbszlee"
        , "Usrin"
        , "gigi"
        ]
c159 = NoDraws
        [ "Akoss Poo"
        , "CTG"
        , "Duplode"  -- Clairwil
        , "SuperBrian"
        , "Boller Jani"
        , "Rokker Zsolti"
        , "dreadnaut"
        , "Usrin"
        , "AbuRaf70"
        , "dstien"
        , "Mingva"
        , "afullo"
        , "Alan Rotoi"  -- Rule #3 exclusion
        ]
c160 = NoDraws
        [ "CTG"
        , "Akoss Poo"
        , "Rokker Zsolti"
        , "Duplode"  -- Clairwil
        , "Boller Jani"
        , "dreadnaut"
        , "AbuRaf70"
        , "dstien"
        , "afullo"  -- Rule #3 exclusion
        , "Usrin"  -- Rule #3 exclusion
        ]
c161 = NoDraws
        [ "Akoss Poo"
        , "Duplode"  -- Clairwil
        , "SuperBrian"
        , "dreadnaut"
        , "Rokker Zsolti"
        , "Boller Jani"
        , "Mingva"
        , "AbuRaf70"
        , "dstien"
        , "afullo"
        ]
c162 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Akoss Poo"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "Usrin"
        , "Marco"
        , "dreadnaut"
        , "dstien"
        , "Shoegazing Leo"
        , "AbuRaf70"
        , "kahdra"
        , "afullo"
        , "gtament"
        ]
c163 = NoDraws
        [ "Akoss Poo"
        , "Duplode"
        , "CTG"
        , "Marco"
        , "Usrin"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "AbuRaf70"
        , "dreadnaut"
        , "Shoegazing Leo"
        , "dstien"
        , "afullo"
        , "Mr. Stunt"  -- Rule #3 exclusion
        , "Vector"  -- Rule #1 exclusion
        ]
c164 = NoDraws
        [ "Akoss Poo"
        , "Bonzai Joe"
        , "Duplode"
        , "Marco"
        , "Zak McKracken"
        , "AbuRaf70"
        , "Usrin"
        , "SuperBrian"
        , "Shoegazing Leo"
        , "Cas"
        , "dreadnaut"
        , "dstien"
        , "afullo"
        , "Alan Rotoi"
        , "AMG"
        , "Mr. Stunt"
        ]
c165 = NoDraws
        [ "Akoss Poo"
        , "Duplode"
        , "Marco"
        , "Usrin"
        , "Zak McKracken"
        , "AbuRaf70"
        , "dreadnaut"
        , "dstien"
        , "Shoegazing Leo"
        , "Alan Rotoi"
        , "Cas"
        , "afullo"
        ]
c166 = NoDraws
        [ "Duplode"
        , "Akoss Poo"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "Usrin"
        , "Marco"
        , "Cas"
        , "dstien"
        , "Shoegazing Leo"
        , "dreadnaut"
        , "Svenne"
        , "afullo"
        ]
c167 = NoDraws
        [ "Duplode"
        , "Akoss Poo"
        , "Marco"
        , "Shorty"
        , "AbuRaf70"
        , "dreadnaut"
        , "Shoegazing Leo"
        , "Usrin"
        , "dstien"
        , "Cas"
        , "Svenne"
        , "afullo"
        , "kahdra"
        ]
c168 = WithDraws
        [ flip Result 1 "Akoss Poo"
        , flip Result 1 "Duplode"
        , flip Result 3 "Marco"
        , flip Result 4 "Zak McKracken"
        , flip Result 5 "Usrin"
        , flip Result 6 "dreadnaut"
        , flip Result 7 "AbuRaf70"
        , flip Result 8 "dstien"
        , flip Result 9 "Shoegazing Leo"
        , flip Result 10 "PJ"
        , flip Result 11 "Alan Rotoi"
        , flip Result 12 "Shorty"
        , flip Result 13 "afullo"
        , flip Result 14 "kahdra"
        ]
c169 = NoDraws
        [ "Akoss Poo"
        , "Marco"
        , "AbuRaf70"
        , "Duplode"
        , "Usrin"
        , "Zak McKracken"
        , "Shoegazing Leo"
        , "Shorty"
        , "dreadnaut"
        , "dstien"
        , "PJ"
        , "Cas"
        , "afullo"
        , "Kjotleik"
        ]
c170 = NoDraws
        [ "Duplode"
        , "Akoss Poo"
        , "Marco"
        , "Zak McKracken"
        , "AbuRaf70"
        , "dreadnaut"
        , "Cas"
        , "dstien"
        , "Usrin"
        , "Shoegazing Leo"
        , "PJ"
        , "Kjotleik"
        , "afullo"
        , "SimpleM"
        , "Shorty"  -- Rule #3 exclusion
        ]
c171 = NoDraws
        [ "Duplode"
        , "Alan Rotoi"
        , "AbuRaf70"
        , "Akoss Poo"
        , "Marco"
        , "dreadnaut"
        , "Shoegazing Leo"
        , "dstien"
        , "Shorty"
        , "Kjotleik"
        , "Cas"
        , "afullo"
        ]
c172 = NoDraws
        [ "Duplode"
        , "Akoss Poo"
        , "Usrin"
        , "Marco"
        , "dreadnaut"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "Shoegazing Leo"
        , "dstien"
        , "PJ"
        , "afullo"
        , "Alecu"  -- Rule #1 exclusion. "Drathas"
        ]
c173 = NoDraws
        [ "Duplode"
        , "Marco"
        , "Akoss Poo"
        , "Usrin"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "dreadnaut"
        , "Zak McKracken"
        , "dhoine"
        , "Shoegazing Leo"
        , "dstien"
        , "afullo"
        , "satanziege"
        ]
c174 = NoDraws
        [ "Renato Biker"
        , "Friker"
        , "Marco"
        , "Duplode"
        , "Zak McKracken"
        , "dreadnaut"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Shoegazing Leo"
        , "dstien"
        , "dhoine"
        , "afullo"
        ]
c175 = NoDraws
        [ "Renato Biker"
        , "Friker"
        , "Duplode"
        , "Marco"
        , "dreadnaut"
        , "Zak McKracken"
        , "dosbox92"
        , "AbuRaf70"
        , "dstien"
        , "Shoegazing Leo"
        , "Cas"
        , "afullo"
        ]
c176 = NoDraws
        [ "Renato Biker"
        , "Friker"
        , "dreadnaut"
        , "Duplode"
        , "Marco"
        , "Alan Rotoi"
        , "Zak McKracken"
        , "FinRok"
        , "dosbox92"
        , "AbuRaf70"
        , "dstien"
        , "Shoegazing Leo"
        , "afullo"
        ]
c177 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "Friker"
        , "Marco"
        , "FinRok"
        , "Zak McKracken"
        , "AbuRaf70"
        , "dreadnaut"
        , "Shoegazing Leo"
        , "dstien"
        , "afullo"
        ]
c178 = NoDraws
        [ "Duplode"
        , "Renato Biker"
        , "Friker"
        , "AbuRaf70"
        , "Marco"
        , "dstien"
        , "dreadnaut"
        , "Shoegazing Leo"  -- Rule #3 exclusion
        , "FinRok"  -- Rule #2 exclusion
        , "afullo"  -- Rule #3 exclusion
        ]
c179 = NoDraws
        [ "Renato Biker"
        , "Bonzai Joe"
        , "Duplode"
        , "Marco"
        , "FinRok"
        , "Zak McKracken"
        , "AbuRaf70"
        , "dreadnaut"
        , "Shoegazing Leo"
        , "afullo"
        ]
c180 = NoDraws
        [ "Renato Biker"
        , "Duplode"
        , "Friker"
        , "Marco"
        , "dreadnaut"
        , "FinRok"
        , "AbuRaf70"
        , "Zak McKracken"
        , "dstien"
        , "Shoegazing Leo"
        , "ninja750"
        , "Wanted77"  -- Rule #2b
        , "afullo"  -- Rule #3 exclusion
        ]
c181 = NoDraws
        [ "Duplode"
        , "Renato Biker"
        , "Marco"
        , "Akoss Poo"
        , "AbuRaf70"
        , "FinRok"
        , "dreadnaut"
        , "Cas"
        , "Shoegazing Leo"
        , "afullo"  -- Rule #3 exclusion
        ]
c182 = NoDraws
        [ "Marco"
        , "Duplode"
        , "Renato Biker"
        , "FinRok"
        , "Akoss Poo"
        , "Zak McKracken"
        , "dreadnaut"
        , "AbuRaf70"
        , "Alan Rotoi"
        , "Cas"
        , "Shoegazing Leo"
        , "afullo"
        ]
c183 = NoDraws
        [ "Marco"
        , "FinRok"
        , "Renato Biker"
        , "Duplode"
        , "dreadnaut"
        , "Cas"
        , "NachitoBP"
        , "AbuRaf70"
        , "Zak McKracken"
        , "Shoegazing Leo"
        , "Gilmash"
        , "OctaLM"
        , "Lulisa"
        , "afullo"  -- Rule #3 exclusion
        ]
c184 = NoDraws
        [ "Marco"
        , "Duplode"
        , "FinRok"
        , "CTG"  -- "THC"
        , "AbuRaf70"
        , "Cas"
        , "dreadnaut"
        , "Shoegazing Leo"
        , "afullo"
        , "Alecu"  -- Rule #3 exclusion
        ]
c185 = NoDraws
        [ "Marco"
        , "FinRok"
        , "Duplode"
        , "dreadnaut"
        , "Renato Biker"
        , "Zak McKracken"
        , "Shoegazing Leo"
        , "Cas"
        , "AbuRaf70"
        , "satanziege"
        , "afullo"
        ]
c186 = NoDraws
        [ "FinRok"
        , "Friker"
        , "Duplode"
        , "Alan Rotoi"
        , "dreadnaut"
        , "dosbox92"
        , "Zak McKracken"
        , "MOE"
        , "Cas"
        , "Imperas"
        , "Shoegazing Leo"
        , "afullo"
        ]
c187 = NoDraws
        [ "Marco"
        , "FinRok"
        , "Friker"
        , "Duplode"
        , "Alan Rotoi"
        , "dreadnaut"
        , "Imperas"
        , "Zak McKracken"
        , "Shoegazing Leo"
        , "Cas"
        , "arturbmallman"
        , "afullo"
        ]
c188 = NoDraws
        [ "FinRok"
        , "Duplode"
        , "Marco"
        , "dreadnaut"
        , "Zak McKracken"
        , "Imperas"
        , "Alan Rotoi"
        , "Cas"
        , "arturbmallman"
        , "Shoegazing Leo"
        , "afullo"
        , "emandrada"  -- Rule #1 exclusion
        ]
c189 = NoDraws
        [ "FinRok"
        , "Duplode"
        , "Marco"
        , "Bonzai Joe"
        , "Alan Rotoi"
        , "dreadnaut"
        , "Imperas"
        , "Cas"
        , "Shoegazing Leo"
        , "arturbmallman"
        , "Motig"
        , "afullo"
        ]
c190 = NoDraws
        [ "Duplode"
        , "FinRok"
        , "dreadnaut"
        , "Imperas"
        , "arturbmallman"
        , "KaoS"  -- Rule #2b
        , "Cas"
        , "Shoegazing Leo"
        , "afullo"  -- Rule #3 exclusion
        , "Motig"  -- Rule #3 exclusion
        ]
c191 = NoDraws
        [ "Duplode"
        , "FinRok"
        , "dreadnaut"
        , "Imperas"
        , "Zak McKracken"
        , "arturbmallman"
        , "Shoegazing Leo"
        , "Cas"
        , "Marco"  -- Rule #3 exclusion
        , "KaoS"  -- Rule #3 exclusion
        , "afullo"  -- Rule #2 exclusion
        ]
c192 = NoDraws
        [ "Marco"
        , "FinRok"
        , "Duplode"
        , "Imperas"
        , "arturbmallman"
        , "dreadnaut"
        , "Seeker1982"
        , "Nilex"
        , "Shoegazing Leo"
        , "Motig"
        , "afullo"  -- Rule #1 exclusion
        ]
c193 = NoDraws
        [ "FinRok"
        , "Duplode"
        , "dreadnaut"
        , "Imperas"
        , "Seeker1982"
        , "Marco"  -- Rule #2b
        , "Cas"
        , "Shoegazing Leo"
        , "dosbox92"  -- Rule #2b
        , "afullo"
        ]
c194 = NoDraws
        [ "dreadnaut"
        , "Duplode"
        , "FinRok"
        , "Imperas"
        , "Marco"
        , "Cas"
        , "arturbmallman"
        , "Seeker1982"
        , "Shoegazing Leo"
        , "dosbox92"
        , "afullo"
        ]
c195 = NoDraws
        [ "FinRok"
        , "dreadnaut"
        , "Duplode"
        , "Seeker1982"
        , "Cas"
        , "Shoegazing Leo"
        , "Motig"
        , "Motorhead86"
        , "afullo"
        ]
c196 = NoDraws
        [ "FinRok"
        , "Marco"
        , "Seeker1982"
        , "Duplode"
        , "dreadnaut"
        , "Cas"
        , "Smrtka"
        , "Shoegazing Leo"
        , "afullo"
        , "satanziege"
        ]
c197 = NoDraws
        [ "Duplode"
        , "FinRok"
        , "Marco"
        , "Seeker1982"
        , "dreadnaut"
        , "Cas"
        , "Shoegazing Leo"
        , "afullo"
        , "Smrtka"
        , "Motig"
        ]
c198 = NoDraws
        [ "Duplode"
        , "Imperas"
        , "dreadnaut"
        , "Seeker1982"
        , "afullo"
        , "Cas"
        , "Shoegazing Leo"
        , "Motig"
        , "Svenne"  -- Rule #3 exclusion
        ]
c199 = NoDraws
        [ "Duplode"
        , "Seeker1982"
        , "dreadnaut"
        , "afullo"
        , "Cas"
        , "Shoegazing Leo"
        , "Motig"
        , "chenlei"
        , "BernieR"
        ]
c200 = NoDraws
        [ "CTG"
        , "Duplode"
        , "FinRok"
        , "Seeker1982"
        , "Zak McKracken"
        , "dreadnaut"
        , "arturbmallman"
        , "afullo"
        , "Cas"
        , "Shoegazing Leo"
        , "Alan Rotoi"  -- Rule #3 exclusion
        , "Motig"  -- Rule #3 exclusion
        ]
c201 = NoDraws
        [ "FinRok"
        , "Duplode"
        , "CTG"
        , "dreadnaut"
        , "Seeker1982"
        , "afullo"
        , "Cas"
        , "Alan Rotoi"
        , "arturbmallman"
        , "Shoegazing Leo"
        , "GTAMan15"
        , "KaoS"  -- Rule #2 exclusion
        , "Motig"
        ]
c202 = NoDraws
        [ "Alan Rotoi"
        , "FinRok"
        , "Duplode"
        , "Seeker1982"
        , "afullo"
        , "Cas"
        , "dreadnaut"
        , "Swotl"
        , "arturbmallman"
        , "Shoegazing Leo"
        , "GTAMan15"
        , "Marco"  -- Rule #2 exclusion
        , "Motig"
        , "Chacan"
        ]
c203 = NoDraws
        [ "Duplode"
        , "Seeker1982"
        , "Alan Rotoi"
        , "dreadnaut"
        , "arturbmallman"
        , "afullo"
        , "Cas"
        , "Swotl"
        , "Shoegazing Leo"
        , "GTAMan15"
        , "Motig"
        ]
c204 = NoDraws
        [ "Duplode"
        , "Seeker1982"
        , "dreadnaut"
        , "Zak McKracken"
        , "afullo"
        , "Shoegazing Leo"
        , "Cas"
        , "GTAMan15"
        , "Alan Rotoi"  -- Rule #3 exclusion
        ]
c205 = NoDraws
        [ "CTG"
        , "Seeker1982"
        , "dreadnaut"
        , "Duplode"
        , "Cas"
        , "afullo"
        , "Shoegazing Leo"
        , "Motig"
        ]
c206 = NoDraws
        [ "Duplode"
        , "CTG"
        , "dreadnaut"
        , "Overdrijf"
        , "Seeker1982"
        , "afullo"
        , "Cas"
        , "arturbmallman"
        , "Shoegazing Leo"
        ]
c207 = NoDraws
        [ "CTG"
        , "Duplode"
        , "Akoss Poo"
        , "dreadnaut"
        , "Overdrijf"
        , "Seeker1982"
        , "dosbox92"
        , "afullo"
        , "Cas"
        , "KaoS"
        , "Shoegazing Leo"
        ]
c208 = NoDraws
        [ "CTG"
        , "Duplode"
        , "dreadnaut"
        , "Alan Rotoi"
        , "Seeker1982"
        , "Overdrijf"
        , "afullo"
        , "Shoegazing Leo"
        , "Cas"
        , "Motig"
        , "GTAMan15"
        ]
c209 = WithDraws
        [ flip Result 1 "FinRok"
        , flip Result 2 "CTG"
        , flip Result 3 "Duplode"
        , flip Result 4 "dreadnaut"
        , flip Result 5 "Seeker1982"
        , flip Result 6 "Akoss Poo"
        , flip Result 7 "Alan Rotoi"
        , flip Result 8 "Overdrijf"
        , flip Result 9 "Cas"
        , flip Result 9 "afullo"
        , flip Result 11 "Heretic"
        , flip Result 12 "Shoegazing Leo"
        , flip Result 13 "KaoS"  -- Rule #2b
        , flip Result 14 "Motig"
        , flip Result 15 "GTAMan15"
        , flip Result 16 "Stan 286XT"  -- Rule #3 exclusion
        ]
c210 = NoDraws
        [ "dreadnaut"
        , "Marco"
        , "CTG"
        , "Overdrijf"
        , "Alan Rotoi"
        , "Duplode"
        , "Seeker1982"
        , "Heretic"
        , "afullo"
        , "Cas"
        , "Shoegazing Leo"
        , "Smrtka"
        , "Motig"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c211 = NoDraws
        [ "Marco"
        , "Seeker1982"
        , "Duplode"
        , "dreadnaut"
        , "Overdrijf"
        , "Heretic"
        , "afullo"
        , "sd4000"
        , "Cas"
        , "Usrin"
        , "arturbmallman"
        , "Shoegazing Leo"
        , "fx_undertaker"
        , "Motig"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c212 = NoDraws
        [ "FinRok"
        , "Marco"
        , "CTG"
        , "Duplode"
        , "Seeker1982"
        , "Overdrijf"
        , "dreadnaut"
        , "afullo"
        , "Heretic"
        , "Cas"
        , "sd4000"
        , "Shoegazing Leo"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c213 = NoDraws
        [ "FinRok"
        , "Marco"
        , "Duplode"
        , "Overdrijf"
        , "dreadnaut"
        , "Seeker1982"
        , "CTG"
        , "afullo"
        , "Cas"
        , "sd4000"
        , "Shoegazing Leo"
        , "Heretic"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c214 = NoDraws
        [ "FinRok"
        , "Overdrijf"
        , "Marco"
        , "Duplode"
        , "Gutix"
        , "dreadnaut"
        , "Seeker1982"
        , "Heretic"
        , "Cas"
        , "afullo"
        , "KaoS"
        , "Shoegazing Leo"
        , "GTAMan15"
        , "Stan 286XT"  -- Rule #2 exclusion
        , "sd4000"
        ]
c215 = NoDraws
        [ "Overdrijf"
        , "Seeker1982"
        , "Marco"
        , "dreadnaut"
        , "Duplode"
        , "Alan Rotoi"
        , "afullo"
        , "Shoegazing Leo"
        , "GTAMan15"
        , "Cas"
        , "sd4000"
        , "Heretic"
        , "Stan 286XT"  -- Rule #2b
        ]
c216 = NoDraws
        [ "Marco"
        , "Duplode"
        , "Seeker1982"
        , "Overdrijf"
        , "afullo"
        , "dreadnaut"
        , "Cas"
        , "GTAMan15"
        , "Heretic"
        , "Shoegazing Leo"
        , "sd4000"
        , "Motig"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c217 = NoDraws
        [ "Duplode"
        , "Marco"
        , "Overdrijf"
        , "Seeker1982"
        , "dreadnaut"
        , "dosbox92"
        , "Cas"
        , "afullo"
        , "Heretic"
        , "sd4000"
        , "Shoegazing Leo"
        , "satanziege"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c218 = NoDraws
        [ "Marco"
        , "dreadnaut"
        , "Alan Rotoi"
        , "Duplode"
        , "Seeker1982"
        , "Overdrijf"
        , "dosbox92"
        , "afullo"
        , "Heretic"
        , "Cas"
        , "sd4000"
        , "Shoegazing Leo"
        , "Stan 286XT"  -- Rule #2 exclusion
        , "Motig"
        ]
c219 = WithDraws
        [flip Result 1 "Alan Rotoi"
        ,flip Result 2 "Duplode"
        ,flip Result 3 "Seeker1982"
        ,flip Result 4 "Overdrijf"
        ,flip Result 5 "Marco"
        ,flip Result 6 "dreadnaut"
        ,flip Result 7 "Cas"
        ,flip Result 8 "Stingray86"
        ,flip Result 8 "afullo"
        ,flip Result 10 "Heretic"
        ,flip Result 11 "sd4000"
        ,flip Result 12 "Shoegazing Leo"
        ,flip Result 13 "Stan 286XT"  -- Rule #3 exclusion
        ]
c220 = NoDraws
        [ "CTG"
        , "Seeker1982"
        , "Overdrijf"
        , "dreadnaut"
        , "Duplode"
        , "Stingray86"
        , "Cas"
        , "afullo"
        , "Heretic"
        , "sd4000"
        , "Shoegazing Leo"
        , "Alan Rotoi"  -- Rule #3 exclusion
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c221 = NoDraws
        [ "FinRok"
        , "Overdrijf"
        , "CTG"
        , "Duplode"
        , "Seeker1982"
        , "Stingray86"
        , "dreadnaut"
        , "Heretic"
        , "afullo"
        , "sd4000"
        , "Shoegazing Leo"
        , "Marco"  -- Rule #2 exclusion
        , "Cas"
        , "GTAMan15"  -- Rule #3 exclusion
        , "Stan 286XT"  -- Rule #3 exclusion
        ]
c222 = WithDraws
        [flip Result 1 "Duplode"
        ,flip Result 2 "Seeker1982"
        ,flip Result 3 "dreadnaut"
        ,flip Result 3 "Overdrijf"
        ,flip Result 5 "Stingray86"
        ,flip Result 6 "Heretic"
        ,flip Result 7 "afullo"
        ,flip Result 8 "KaoS"  -- Rule #2b
        ,flip Result 9 "Shoegazing Leo"
        ,flip Result 10 "sd4000"
        ,flip Result 11 "Cas"
        ,flip Result 12 "satanziege"
        ,flip Result 13 "Stan 286XT"  -- Rule #3 exclusion
        ]
c223 = NoDraws
        [ "Duplode"
        , "Seeker1982"
        , "CTG"
        , "Stingray86"
        , "Overdrijf"
        , "Marco"  -- Rule #2b
        , "dreadnaut"
        , "afullo"
        , "KaoS"  -- Rule #2b
        , "Cas"
        , "sd4000"
        , "Shoegazing Leo"
        , "HL96T"
        , "Stan 286XT"  -- Rule #2b
        ]
c224 = NoDraws
        [ "Duplode"
        , "Alan Rotoi"
        , "CTG"
        , "dreadnaut"
        , "Stingray86"
        , "Overdrijf"
        , "Heretic"
        , "Seeker1982"
        , "afullo"
        , "Cas"
        , "KaoS"  -- Rule #2b
        , "sd4000"
        , "HL96T"
        , "Shoegazing Leo"
        , "Stan 286XT"
        ]
c225 = NoDraws
        [ "Seeker1982"
        , "Overdrijf"
        , "Duplode"
        , "CTG"
        , "Stingray86"
        , "dreadnaut"
        , "Heretic"
        , "afullo"
        , "Cas"
        , "sd4000"
        , "Shoegazing Leo"
        , "KaoS"  -- Rule #3 exclusion
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c226 = NoDraws
        [ "Duplode"
        , "Seeker1982"
        , "Overdrijf"
        , "CTG"
        , "dreadnaut"
        , "Stingray86"
        , "Marco"  -- Rule #2b
        , "KaoS"  -- Rule #2b
        , "afullo"
        , "Heretic"
        , "Alan Rotoi"
        , "Cas"  -- Rule #3 exclusion
        , "Shoegazing Leo"
        , "Stan 286XT"  -- Rule #3 exclusion
        , "Chacan"  -- Rule #3 exclusion
        ]

c227 = NoDraws
        [ "CTG"
        , "Seeker1982"
        , "Duplode"
        , "dreadnaut"
        , "Overdrijf"
        , "Stingray86"
        , "Heretic"
        , "afullo"
        , "Cas"
        , "JoSi"
        , "Shoegazing Leo"
        , "Stan 286XT"
        ]

c228 = NoDraws
        [ "Duplode"
        , "Seeker1982"
        , "dreadnaut"
        , "Alan Rotoi"
        , "Stingray86"
        , "Overdrijf"
        , "Heretic"
        , "afullo"
        , "Shoegazing Leo"
        , "Cas"  -- Rule #3 exclusion
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c229 = NoDraws
        [ "FinRok"
        , "Duplode"
        , "Marco"
        , "CTG"
        , "Seeker1982"
        , "Alan Rotoi"
        , "Stingray86"
        , "Heretic"
        , "Overdrijf"
        , "dreadnaut"
        , "afullo"
        , "KyLiE"
        , "KaoS"  -- Rule #2b
        , "Straycat"
        , "Cas"
        , "Shoegazing Leo"
        , "Zak McKracken"  -- Rule #2b
        , "GTAMan15"
        , "Stan 286XT"
        ]

c230 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Seeker1982"
        , "KyLiE"
        , "Heretic"
        , "dreadnaut"
        , "Stingray86"
        , "Overdrijf"
        , "Alan Rotoi"
        , "afullo"
        , "Zak McKracken"  -- Rule #2b
        , "Shoegazing Leo"
        , "Cas"
        , "Straycat"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c231 = NoDraws
        [ "CTG"
        , "Alan Rotoi"
        , "Overdrijf"
        , "Seeker1982"
        , "dreadnaut"
        , "Duplode"
        , "Stingray86"
        , "Heretic"
        , "KyLiE"
        , "afullo"
        , "Straycat"
        , "Cas"
        , "KaoS"  -- Rule #2b
        , "Shoegazing Leo"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c232 = NoDraws
        [ "CTG"
        , "Duplode"
        , "dreadnaut"
        , "Overdrijf"
        , "Seeker1982"
        , "afullo"
        , "Heretic"
        , "KyLiE"
        , "Stingray86"
        , "KaoS"  -- Rule #2b
        , "Straycat"
        , "Shoegazing Leo"
        , "Cas"
        , "GTAMan15"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c233 = NoDraws
        [ "Duplode"
        , "CTG"
        , "Seeker1982"
        , "Overdrijf"
        , "dreadnaut"
        , "Stingray86"
        , "KyLiE"
        , "Heretic"
        , "afullo"
        , "Straycat"
        , "Cas"
        , "Zak McKracken"
        , "Shoegazing Leo"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c234 = NoDraws
        [ "Duplode"
        , "Alan Rotoi"
        , "CTG"
        , "Akoss Poo"
        , "Marco"
        , "Overdrijf"
        , "Zak McKracken"
        , "dreadnaut"
        , "KyLiE"
        , "Friker"
        , "Heretic"
        , "afullo"
        , "Zapper"
        , "Cas"
        , "Usrin"
        , "Shoegazing Leo"
        , "Igor"
        , "matamo"
        , "Daniel3D"
        , "KaoS"  -- # Rule #3 exclusion
        , "Straycat"  -- Rule #3 exclusion
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c235 = NoDraws
        [ "CTG"
        , "Duplode"
        , "Alan Rotoi"
        , "KyLiE"
        , "Overdrijf"
        , "Marco"
        , "dreadnaut"
        , "GTAMan15"
        , "Zapper"
        , "afullo"
        , "Heretic"
        , "Zak McKracken"  -- Rule #2b
        , "Cool Gaddafi"
        , "Shoegazing Leo"
        , "Cas"
        , "KaoS"  -- Rule #2b
        , "Straycat"
        , "matamo"
        , "Daniel3D"
        , "Igor"
        , "ZdnBurns"
        , "Stan 286XT"  -- Rule #2b
        ]

c236 = NoDraws
        [ "CTG"
        , "Akoss Poo"
        , "Duplode"
        , "Overdrijf"
        , "Alan Rotoi"
        , "KyLiE"
        , "dreadnaut"
        , "Zapper"
        , "Heretic"
        , "GTAMan15"
        , "dosbox92"
        , "afullo"
        , "Cas"
        , "Shoegazing Leo"
        , "Seeker1982"  -- Rule #2b
        , "Marco"  -- Rule #2b
        , "KaoS"  -- Rule #2b
        , "Igor"
        , "Straycat"  -- Rule #3 exclusion
        , "Johnny Alaska"
        , "Stan 286XT"  -- Rule #3 exclusion
        ]

c237 = NoDraws
        [ "Duplode"
        , "Akoss Poo"
        , "Seeker1982"
        , "CTG"
        , "Alan Rotoi"
        , "KyLiE"
        , "dreadnaut"
        , "Zapper"
        , "Ryoma"
        , "Heretic"
        , "GTAMan15"
        , "afullo"
        , "Overdrijf"
        , "Cas"
        , "Shoegazing Leo"
        , "Igor"
        , "matamo"
        , "KaoS"  -- Rule #2
        , "Marco"  -- Rule #2
        , "Stan 286XT"
        ]

c238 = NoDraws
        [ "Alan Rotoi"
        , "Duplode"
        , "Akoss Poo"
        , "Friker"
        , "KyLiE"
        , "Ryoma"
        , "dreadnaut"
        , "Overdrijf"
        , "Zapper"
        , "afullo"
        , "Heretic"
        , "Igor"
        , "GTAMan15"
        , "Cas"
        , "matamo"
        , "Stan 286XT"
        , "Shoegazing Leo"
        , "KaoS"  -- Rule #2
        , "AMG"
        , "satanziege"
        , "Straycat"  -- Rule #3
        ]

c239 = NoDraws
        [ "Alan Rotoi"
        , "Duplode"
        , "KyLiE"
        , "Overdrijf"
        , "Friker"
        , "Zapper"
        , "dreadnaut"
        , "Ryoma"
        , "Heretic"
        , "afullo"
        , "Cas"
        , "1337haxoryt"
        , "Shoegazing Leo"
        , "KaoS"  -- Rule #2
        , "Igor"
        , "Stan 286XT"  -- Rule #3
        ]

c240 = NoDraws
        [ "Alan Rotoi"
        , "Zapper"
        , "Duplode"
        , "Overdrijf"
        , "Friker"
        , "KyLiE"
        , "Heretic"
        , "afullo"
        , "dreadnaut"
        , "Ryoma"
        , "Shoegazing Leo"
        , "Cas"
        , "1337haxoryt"
        , "KaoS"  -- Rule #3
        , "Stan 286XT"  -- Rule #3
        ]

c241 = NoDraws
        [ "Alan Rotoi"
        , "Duplode"
        , "KyLiE"
        , "CTG"
        , "Zapper"
        , "Heretic"
        , "dreadnaut"
        , "Overdrijf"
        , "afullo"
        , "Cas"
        , "Shoegazing Leo"
        , "KaoS"  -- Rule #3
        , "Stan 286XT"  -- Rule #3
        ]

c242 = NoDraws
        [ "Alan Rotoi"
        , "Duplode"
        , "Overdrijf"
        , "KyLiE"
        , "Zapper"
        , "afullo"
        , "Heretic"
        , "dreadnaut"
        , "Cas"
        , "Shoegazing Leo"  -- Rule #3
        , "KaoS"  -- Rule #2
        , "Stan 286XT"  -- Rule #3
        ]

c243 = NoDraws
        [ "Duplode"
        , "Alan Rotoi"
        , "dreadnaut"
        , "KyLiE"
        , "Zapper"
        , "Ryoma"
        , "Overdrijf"
        , "Frieshansen"
        , "afullo"
        , "Cas"
        , "Heretic"
        , "Shoegazing Leo"
        , "Marco"  -- Rule #2
        , "Igor"
        , "Daniel3D"
        , "Stan 286XT"  -- Rule #3
        , "GTAMan15"  -- Rule #3
        ]
