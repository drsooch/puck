module Parsing
    ( parsingTests
    ) where

import           Data.Aeson                     ( decode )
import           Data.Aeson.Types               ( Value
                                                , parseEither
                                                , parseMaybe
                                                )
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                    ( fromRight
                                                , isRight
                                                )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Puck.Parser                    ( pGameFinal
                                                , pPlayerInfo
                                                , pTeamInfo
                                                , pTeamSeasonStats
                                                )
import           Puck.Types
import           Test.HUnit


parsingTests :: Test
parsingTests = TestList
    [ TestLabel "Team Info"         testParseTeamInfo
    , TestLabel "Team Season Stats" testTeamSeasonStats
    , TestLabel "Player"            testParsePlayerInfo
    ]

type AssertionD = IO Value

assertDecode :: String -> AssertionD
assertDecode file = do
    result <- decode <$> BL.readFile file
    case result of
        (Just x) -> return x
        Nothing  -> assertFailure $ "Failed to decode: " <> file


{---------------- TEAM PARSING -----------------}
testParseTeamInfo :: Test
testParseTeamInfo = TestCase $ do
    json   <- assertDecode "./test/json/TeamNYR.json"
    result <- case parseEither pTeamInfo json of
        (Left  err) -> assertFailure err
        (Right res) -> return res
    let expected = TeamInfo { teamID     = TeamID 3
                            , name       = "New York Rangers"
                            , abbrev     = "NYR"
                            , division   = Just Metropolitan
                            , conference = Just Eastern
                            , active     = True
                            , franID     = Just $ FranchiseID 10
                            }
    assertEqual "Testing equality in parsed vs. real value" expected result


testTeamSeasonStats :: Test
testTeamSeasonStats = TestCase $ do
    json   <- assertDecode "./test/json/SeasonStats.json"
    result <- case parseEither pTeamSeasonStats json of
        (Left  err) -> assertFailure err
        (Right res) -> return res
    let expected = TeamSeasonsStats { gamesPlayed      = 70
                                    , wins             = 37
                                    , losses           = 28
                                    , otl              = 5
                                    , points           = 79
                                    , pointPct         = 56.4
                                    , goalsForPG       = 3.329
                                    , goalsAgainstPG   = 3.143
                                    , evggaRatio       = 1.049
                                    , ppPct            = 22.9
                                    , ppGF             = 52
                                    , ppGA             = 52
                                    , ppOpp            = 227
                                    , pkPct            = 77.4
                                    , shotsForPG       = 31.0714
                                    , shotsAgainstPG   = 34.0143
                                    , winScoreFirst    = 0.625
                                    , winOppScoreFirst = 0.4
                                    , winLeadFirstPer  = 0.741
                                    , winLeadSecondPer = 0.889
                                    , winOutshootOpp   = 0.444
                                    , winOutshotByOpp  = 0.585
                                    , faceOffsTaken    = 3862
                                    , faceOffsWon      = 1799
                                    , faceOffsLost     = 2063
                                    , faceOffWinPct    = 46.6
                                    , shootingPct      = 10.7
                                    , savePct          = 0.908
                                    }
    assertEqual "Testing equality in parsed vs. real value" expected result

{---------------- PLAYER PARSING -----------------}
testParsePlayerInfo :: Test
testParsePlayerInfo = TestCase $ do
    json   <- assertDecode "./test/json/Player.json"
    result <- case parseEither pPlayerInfo json of
        (Left  err) -> assertFailure err
        (Right res) -> return res
    let expected = PlayerInfo { playerID = PlayerID 8470600
                              , teamID   = TeamID 30
                              , name     = PName "Ryan" "Suter" "Ryan Suter"
                              , number   = Just 20
                              , position = Defenseman
                              , hand     = LeftH
                              , rookie   = False
                              , age      = 35
                              }
    assertEqual "Testing equality in parsed vs. real value" expected result

{---------------- GAME PARSING -----------------}
testParseGameFinal :: Test
testParseGameFinal = TestCase $ do
    json   <- assertDecode "./test/json/GameFinal.json"
    result <- case parseEither pGameFinal json of
        (Left  err) -> assertFailure err
        (Right res) -> return res
    let expected = GameFinal{}
    assertEqual "Testing equality in parsed vs. real value" expected result
