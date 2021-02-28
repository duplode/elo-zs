{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralisedNewtypeDeriving #-}
-- | Module Simulation
--
-- Simulate races using the performance model.
module Simulation
    ( SimPip(..)
    , SimEntry(..)
    , toSimPips
    , simulateSingleRace
    , runExperimentFull
    , runExperimentPip
    , runExperimentProbe
    , simModelStrength
    , simAveragePositions
    , testSeed1
    ) where

import Types
import Orbital
import Util.Lone (surroundL)

import System.Random.MWC
import Statistics.Distribution

import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List
import Data.Ord
import Pipes
import qualified Pipes.Prelude as P
import Data.Default.Class
import Control.Monad.State.Strict
import qualified Data.Vector as V (fromList)

-- | Racer identification. The type is augmented by testing probes.
data SimPip = Probe !Int | SimPip !PipId
    deriving (Eq, Ord, Show)

-- | Racer result with rank generated by the simulation.
data SimEntry = SimEntry
    { simPip :: !SimPip
    , laptimeDelta :: !Double
    , pipRank :: !Int
    }
    deriving (Eq, Ord, Show)

-- | Prepare ratings from the engine to feed the simulation.
toSimPips
    :: Int                              -- ^ Gamma shape parameter.
    -> [(Int, Double)]                  -- ^ Identifications and ratings of the probes.
    -> Ratings                          -- ^ Ratings map from the engine.
    -> [(SimPip, OrbitalDistribution)]  -- ^ Identifications and performance models.
toSimPips gsh probes rtgs = map (second (orbitalDistr gsh . kFromRating))
    $ map (first Probe) probes
    ++ map (bimap SimPip rating) (Map.assocs rtgs)

-- | Infinite stream of simulated race results. If the number of runs that
-- will be used is known in advance, it is slightly less efficient than
-- 'simulatorN', as it has to save the generator state after each run.
simulator
    :: [(SimPip, OrbitalDistribution)]   -- ^ Identifications and performance models.
    -> Producer [SimEntry] SimM ()       -- ^ Identifications and laptime deltas.
simulator pips = do
    seed <- get
    g <- liftIO $ restore seed
    P.repeatM $ do
        results <- liftIO $ traverse (surroundL (flip genContVar g)) pips
        newSeed <- liftIO $ save g
        put newSeed
        return $ zipWith (uncurry SimEntry) (sortBy (comparing snd) results) [1..]

-- | Stream of a finite number of simulated results. The updated generator
-- seed is used as the return value to make it harder to discard it
-- accidentally by interrupting the stream. For interrupting the stream, use
-- 'void' to discard the return value, or switch to 'simulator' if it is
-- important to update the generator state.
simulatorN
    :: Int                               -- ^ Number of simulations
    -> [(SimPip, OrbitalDistribution)]   -- ^ Identifications and performance models.
    -> Producer [SimEntry] SimM Seed     -- ^ Identifications and laptime deltas.
simulatorN nRuns pips = do
    seed <- get
    g <- liftIO $ restore seed
    P.replicateM nRuns $ do
        results <- liftIO $ traverse (surroundL (flip genContVar g)) pips
        return $ zipWith (uncurry SimEntry) (sortBy (comparing snd) results) [1..]
    newSeed <- liftIO $ save g
    put newSeed
    return newSeed

-- | Generates results of a single race.
simulateSingleRace
    :: [(SimPip, OrbitalDistribution)]  -- ^ Identifications and performance models.
    -> SimM [SimEntry]                  -- ^ Identifications and laptime deltas.
simulateSingleRace pips = do
    P.head (void $ simulatorN 1 pips) >>= \case
        Just res -> return res
        Nothing -> error "Analysis.Simulation: simulator exhausted"

-- | Generates results of many races and tallies how often racers reached each
-- position.
runExperimentFull
    :: Int                              -- ^ Number of runs.
    -> [(SimPip, OrbitalDistribution)]  -- ^ Identifications and performance models.
    -> SimM (Map (SimPip, Int) Int)     -- ^ Count of racer-rank pairs.
runExperimentFull nRuns pips = fst <$> P.fold' updateCount counters id entries
    where
    counters = Map.empty
    entries = simulatorN nRuns pips >-> P.mapFoldable id
    updateCount ctrs se =
        Map.alter (maybe (Just 1) (Just . (+ 1))) (simPip se, pipRank se) ctrs

-- | Generates results of many races and tallies how often a specific racer
-- reached each position.
runExperimentPip
    :: Int                               -- ^ Number of runs.
    -> SimPip                            -- ^ Selected racer.
    -> [(SimPip, OrbitalDistribution)]   -- ^ Identifications and performance models.
    -> SimM (Map Int Int)                -- ^ Count of racer-rank pairs.
runExperimentPip nRuns selPip pips =
    fst <$> P.fold' updateCount counters id entries
    where
    counters = Map.empty
    entries = simulatorN nRuns pips >-> P.mapFoldable id
        >-> P.filter ((selPip ==) . simPip)
    updateCount ctrs se =
        Map.alter (maybe (Just 1) (Just . (+ 1))) (pipRank se) ctrs
    -- Note that this doesn't seem to be noticeably faster than
    -- runExperimentFull.

-- | Generates results of many races with an added probe and tallies how often
-- the probe reached each position.
runExperimentProbe
    :: EloOptions                        -- ^ Number of runs, seed, and probe.
    -> [(SimPip, OrbitalDistribution)]   -- ^ Identifications and performance models.
    -> SimM (Map Int Int)                -- ^ Count of racer-rank pairs.
runExperimentProbe eopts pips =
    runExperimentPip nRuns probeId ((probeId, probeModel) : pips)
    where
    nRuns = simRuns eopts
    probeModel = orbitalDistr (eloGammaShape eopts) (kFromRating (simProbeRating eopts))
    probeId = Probe 0

simModelStrength :: EloOptions -> Ratings -> SimM Double
simModelStrength eopts = fmap ((fromIntegral nRuns /) . fromIntegral
        . Map.foldl' (+) 0 . Map.filterWithKey (\key _ -> isTargetReached key))
    . runExperimentProbe eopts . toSimPips (eloGammaShape eopts) []
    where
    nRuns = simRuns eopts
    isTargetReached = (<= simTarget eopts)

-- | Average positions attained by racers in the simulations.
simAveragePositions :: EloOptions -> Ratings -> SimM (Map SimPip Double)
simAveragePositions eopts = fmap (fmap (/ fromIntegral nRuns)
        . Map.mapKeysWith (+) fst
        . Map.mapWithKey (\(p, i) m -> fromIntegral i * fromIntegral m))
    . runExperimentFull nRuns . toSimPips (eloGammaShape eopts) []
    where
    nRuns = simRuns eopts


example = bimap SimPip (orbitalDistr 3)
    <$> [("HAM", 500), ("BOT", 286), ("VER", 430), ("VET", 380), ("STR", 240)]


(testSeed1, testSeed2) =
    ( (toSeed . V.fromList)
    [2983729257,1613295996,1536938243,3382368331,999233052,3403717367,1505805409
    ,457896502,2924789643,126275855,1602675974,2774079579,2608979963
    ,1285505154,2190748158,2008670062,2323612213,1474206092,1096337431
    ,288815539,2167832047,89478764,3770564965,1831684145,4254467873
    ,4134720386,890644500,541497865,2034198580,1508666107,283455713
    ,3816390539,1145530897,2308506764,842610843,3446610465,2194641952
    ,1640187121,2991202145,2011144362,3678556400,2037948716,1876884772
    ,2860215653,1643680172,2482193874,3509845458,3561221336,4142605894
    ,2783016144,3695356134,2149339665,3027240920,3779754543,1091699906
    ,1296315652,4201840705,466463827,2771261985,3609434098,3174518634
    ,1883778791,1338358979,2103001057,1010315523,1850466367,1704534333
    ,1127116119,494022984,4131810412,1989435256,2189717985,1195790672
    ,546149896,3956437595,3430065006,996894469,652151680,2444801896
    ,3277167510,3898937104,1816281959,3887115177,756915671,1965221836
    ,3217297445,2524193384,1617990254,17611352,3883903320,654722342
    ,3035693226,2959956776,2480087566,1416574980,2332603833,1114892984
    ,3972168584,3190304332,2279966641,3984143279,506682112,708954088
    ,782831114,3580552292,1328043039,1445323362,545935803,3511911499
    ,3532637593,3092002176,1293437661,866180493,470170908,2213099343
    ,2610774100,770498881,1522960121,362223202,368376677,2696502499
    ,3735383147,959672244,3597809455,1480371635,1938962987,3574654602
    ,3123103398,3899686612,111986794,555794392,433322464,3512122633
    ,888439676,4013356910,1537488858,2998004672,1503517187,1676041891
    ,4284517682,4187913039,526411011,3844452821,2843102260,1863455660
    ,2745546381,3458778584,986126662,623225133,484127313,564056677
    ,3412008153,2637822297,2049738317,801004491,3845660219,598766095
    ,280702759,3220956212,2559119027,1387002130,1860669793,3878600592
    ,1646571175,2650709733,287304878,2708941441,3725941329,1923856846
    ,1348165256,360727946,734818281,3696343351,2757132999,3994319050
    ,821963309,4053106446,3956385121,3738530775,934656047,3471283180
    ,2977709497,1234997742,1733246858,1772131303,3541096545,1687511831
    ,628693315,3546632502,2171179846,2127187761,2008195960,537150990
    ,2347439733,368798333,1489117236,1030208402,1793664579,3357536366
    ,1379818798,1445263925,4136224347,2107945916,3629803435,2568860630
    ,4168840089,2151673581,2471786285,2480203426,3333035688,705116558
    ,3745612939,1227314979,1292798725,612448134,3396651774,975841030
    ,624512980,4190767123,1233459964,3008972087,601532817,3383745831
    ,773671035,1983882269,146329547,2740348494,1651745877,728960915
    ,3716272563,1763505775,3322684912,3890606420,213721570,223357770
    ,2416381159,1421939056,189957726,776510638,3327921427,3756638545
    ,3518106256,1713504596,1695176588,2195699141,1866690002,2307523501
    ,3633434759,832509879,2504451685,2104731942,2078007247,608050460
    ,1686619141,3763034973,1595031540,255,362436]
    , (toSeed . V.fromList)
    [3514361481,3842361471,795014533,1784838522,526088448,646126946,2812735612
    ,922018427,3378711992,782018649,1464785869,2983406718,2419633236
    ,1853276290,309755073,1617742846,4109644868,2445006221,1147110543
    ,2791681101,708108117,1877561500,86076843,3979878655,2149817768
    ,927290670,2660510741,976560117,3170810766,926610827,3509847976
    ,1108252766,601807489,4115751470,3120488071,3072205774,3290517234
    ,1820511216,3050864435,571828010,272780034,1808615920,2680969109
    ,196720231,2217492074,3498903999,978244655,2904543907,462407936
    ,2767797777,1805729825,471152103,2285843480,2591886531,1835500868
    ,2217326776,3533974798,1542713276,3796979153,924020557,1821142792
    ,2849580282,3686348589,1449495310,2253563057,2165272312,2064015893
    ,3905773535,2412868076,450944229,3801900089,2996821856,3902541163
    ,1622237188,1727880413,938683952,3672638018,1520966709,856986815
    ,1262553468,4209303792,1423083963,1481088361,4220947014,980218971
    ,2211614421,2023491475,873192432,258917836,2590338598,2264586590
    ,2935412575,3510677396,2083579827,1902702209,1248330234,763375492
    ,255297030,2164301963,2217557972,3368312704,1113206599,3368064395
    ,3721694346,4269798761,4101962784,1234564983,1936596601,1270230244
    ,1248522168,3808964907,3440012765,2846369544,1495023958,2839177494
    ,2358176524,1639191498,106020455,2934873951,3137110660,426604379
    ,4081972914,1956316804,427508886,710455103,3967527362,3510393578
    ,791432587,2989263762,4159249582,3959193725,82625198,1773816659
    ,1187237560,177169928,4088291379,1342962495,375846631,34078710
    ,4259092681,2238989091,2401696247,3212319020,2081634154,644007076
    ,1450607183,25503484,3904269573,839898787,1938851346,296165563
    ,472281688,3885688307,1744171857,107901069,3687678329,3225343202
    ,4274663955,2691973102,2944086281,560196343,417575873,746385354
    ,2817008926,2007973603,2800956071,4171484727,3441556823,788429435
    ,773478323,2946141746,4117575557,1835102299,1247443170,3285737716
    ,80965588,528551827,928476088,652810025,1728639523,3172597791
    ,3195498619,1668001640,1090898405,3635992671,724319477,3085777054
    ,3197077886,1892186618,3464705939,3263299682,2045281371,4056683611
    ,3422245534,281426931,2906982745,2362266837,2311470939,4024086179
    ,296904488,3549811521,3590869187,30877516,2190023718,1129803354
    ,2329929231,3208052139,3152957852,1071654726,2578550933,2962388832
    ,3992806217,3387397289,3784382569,3386603360,1782091423,133382899
    ,3644533203,3133883996,3443143560,2901080115,218653140,2133128666
    ,2660840034,3437312141,992717558,1663297758,2357872362,2910300196
    ,899348458,3304500258,612813003,3888003996,736806268,3876075907
    ,2968262906,2686602214,1787165222,1401421921,4129502730,464353724
    ,2707726113,4072910765,3141835839,2016856979,1313588609,3290206241
    ,3656476319,2831158121,4267011120,925306905,282140482,3869026817
    ,1821837849,538991518,915864998,221,539427143])

-- >$> :set -XOverloadedStrings
-- $>
-- >$> :set +s
-- $>
-- >$> evalSimM Nothing $ simulateSingleRace Nothing example
--
-- >$> evalSimM Nothing $ runExperimentFull 10000 example
