module Puck.Parser
    --(
    ---- Utility Functions
      --toObject
    --, toValue
    ---- Parsing Teams
    --, pTeamID
    --, pFranchiseId
    --, pDivision
    --, pConference
    --, pTeamInfo
    --, pTeamSeasonStats
    --, pTeamGameStats
    ---- Parsing Players
    --, pPlayerID
    --, pPName
    --, pPlayerInfo
    --, pHand
    --, pPosition
    ---- Parsing Games
    --, pGameID
    --, pGameDate
    --, pGameTime
    --, pGameLive
    --, pGameFinal
    --, pGamePeriods
    --, pPeriod
    --, pPeriodStats
    --)
                   where

import           Data.Time                      ( UTCTime
                                                , parseTimeOrError
                                                , defaultTimeLocale
                                                )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as M
import qualified Data.HashMap.Strict           as HM
import           Data.Vector                    ( (!)
                                                , toList
                                                , Vector
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.=)
                                                , (.!=)
                                                , withObject
                                                , withArray
                                                , FromJSON(parseJSON)
                                                , Value(Object)
                                                , Value(Array)
                                                )
import           Data.Aeson.Types
import           Data.Foldable                  ( asum )
import           Data.Traversable               ( for )
import           Puck.Types
import           Puck.Database.Types
import           Puck.Utils

-- pseudo-hack, most results from JSON parsing are objects
-- Go straight to fail if x isn't an object. This means the
-- underlying JSON has changed.
toObject :: ToJSON a => a -> Object
toObject x = case toJSON x of
    Object o -> o
    x        -> error $ "Failed toObject: " <> show x

toValue :: Object -> Value
toValue = Object

-- because certain values in JSON are STRINGS rather than
-- actual numbers, we have to use this
toDouble :: Value -> Double
toDouble (String xs) = read $ T.unpack xs
toDouble x           = error $ "Failed toDouble: " <> show x

toInt :: Value -> Int
toInt (String xs) = read $ T.unpack xs
toInt x           = error $ "Failed toDouble: " <> show x

fromArray :: Int -> Value -> Value
fromArray ix (Array xs) = xs ! ix
fromArray _  x          = error $ "Failed fromArray: " <> show x

{----------------------- Parsing Team -------------------------}
pTeamID :: Value -> Parser TeamID
pTeamID = withObject "TeamID" $ \o -> do
    num <- asum
        [ (o .: "currentTeam") >>= (.: "id") -- Player Endpoint
        , o .: "id" -- Team endpoint
        ]
    return $ TeamID num

pFranchiseId :: Value -> Parser (Maybe FranchiseID)
pFranchiseId = withObject "FranchiseID" $ \o -> do
    fmap FranchiseID <$> o .:? "franchiseId"

pDivision :: Value -> Parser (Maybe DivisionID)
pDivision = withObject "Division" $ \o ->
    (o .:? "division" .!= mempty) >>= fmap (fmap DivisionID) . (.:? "id")

pConference :: Value -> Parser (Maybe ConferenceID)
pConference =
    withObject "Conference"
        $ \o ->
              (o .:? "conference" .!= mempty)
                  >>= fmap (fmap ConferenceID)
                  .   (.:? "id")

-- Don't want to use Vectors
-- parses entire /teams endpoint 
pAllNHLTeams :: Value -> Parser [TeamInfoDB]
pAllNHLTeams = fmap toList . pAllNHLTeams'

pAllNHLTeams' :: Value -> Parser (Vector TeamInfoDB)
pAllNHLTeams' = withObject "pAllNHLTeams" $ \o -> do
    (Array teams) <- o .: "teams"
    for teams $ \val -> do
        team <- pTeamInfoNHL val
        return team

-- NHL Teams ONLY
pTeamInfoNHL :: Value -> Parser TeamInfoDB
pTeamInfoNHL = withObject "TeamInfoNHL" $ \o -> do
    teamID     <- pTeamID $ toValue o
    name       <- o .: "name"
    abbrev     <- o .: "abbreviation"
    division   <- pDivision $ toValue o
    conference <- pConference $ toValue o
    active     <- o .:? "active" .!= False
    franID     <- pFranchiseId $ toValue o
    let leagueID = Just $ LeagueID 133   -- NHL League Number
    return TeamInfoDB { .. }


pTeamInfoExtra :: Value -> Parser TeamInfoDB
pTeamInfoExtra = withObject "TeamInfo" $ \o -> do
    teamID <- pTeamID $ toValue o
    name   <- o .: "name"
    abbrev <- o .: "abbreviation"
    let division   = Nothing
    let conference = Nothing
    active <- o .:? "active" .!= False
    let franID   = Nothing
    let leagueID = Nothing
    return TeamInfoDB { .. }

-- parses /teams/{ID}?expand=team.stats&season={SEASON}
pTeamSeasonStats :: Value -> Parser TeamSeasonsStatsDB
pTeamSeasonStats o = baseTeamSeasonStats o >>= pTeamSeasonStats'

-- NOTE: certain data is uninitialized as the actual data is on a 
-- separate endpoint, this data will be merged afterwards
pTeamSeasonStats' :: Value -> Parser TeamSeasonsStatsDB
pTeamSeasonStats' = withObject "TeamSeasonsStats" $ \o -> do
    teamID         <- pTeamID $ toValue o
    gamesPlayed    <- o .: "gamesPlayed"
    wins           <- o .: "wins"
    losses         <- o .: "losses"
    otl            <- o .: "ot"
    points         <- o .: "pts"
    pointPct       <- toDouble <$> o .: "ptPctg"
    goalsForPG     <- o .: "goalsPerGame"
    goalsAgainstPG <- o .: "goalsAgainstPerGame"
    ppPct          <- toDouble <$> o .: "powerPlayPercentage"
    ppGF           <- o .: "powerPlayGoals"
    ppGA           <- o .: "powerPlayGoalsAgainst"
    ppOpp          <- o .: "powerPlayOpportunities"
    pkPct          <- toDouble <$> o .: "penaltyKillPercentage"
    shotsForPG     <- o .: "shotsPerGame"
    shotsAgainstPG <- o .: "shotsAllowed"
    faceOffWinPct  <- toDouble <$> o .: "faceOffWinPercentage"
    shootingPct    <- o .: "shootingPctg"
    savePct        <- o .: "savePctg"
    return TeamSeasonsStatsDB { .. }

-- "root" of JSON Object for TeamSeasonsStats
baseTeamSeasonStats :: Value -> Parser Value
baseTeamSeasonStats o =
    (toObject o .: "teams")
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "teamStats")
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "splits")
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "stat")

-- parses a teams overall record (home, away, shootout, last ten)
-- /standings?expand=standings.record
pTeamRecord :: Value -> Parser OverallRecord
pTeamRecord o = baseOverallRecord o >>= pTeamRecord'

-- could've folded over the array instead of hardcoding,
-- but this works for now
pTeamRecord' :: Value -> Parser OverallRecord
pTeamRecord' = withArray "OverallRecord" $ \a -> do
    homeRecord     <- pSingleRecord $ a ! 0
    awayRecord     <- pSingleRecord $ a ! 1
    shootoutRecord <- pSingleRecord $ a ! 2
    lastTen        <- pSingleRecord $ a ! 3
    return OverallRecord { .. }

pSingleRecord :: Value -> Parser Record
pSingleRecord = withObject "Record" $ \o -> do
    wins   <- o .: "wins"
    losses <- o .: "losses"
    otl    <- o .:? "ot" .!= 0
    return Record { .. }

baseOverallRecord :: Value -> Parser Value
baseOverallRecord o = toObject o .: "records" >>= (.: "overallRecords")

-- /game/{GAMEID}/feed/live packages up both teams stats for a game
pTeamGameStats :: Value -> Parser (TeamStats, TeamStats)
pTeamGameStats = withObject "TeamGameStats" $ \o -> do
    home  <- baseTeamGameStats "home" o
    away  <- baseTeamGameStats "away" o
    home' <- pTeamGameStats' home
    away' <- pTeamGameStats' away
    return (HGStats home', AGStats away')

pTeamGameStats' :: Value -> Parser TeamGameStats
pTeamGameStats' = withObject "TeamGameStats" $ \o -> do
    goals     <- o .: "goals"
    pims      <- o .: "pims"
    shots     <- o .: "shots"
    ppPct     <- o .: "powerPlayPercentage"
    ppGoals   <- o .: "powerPlayGoals"
    ppOpp     <- o .: "powerPlayOpportunities"
    foPct     <- o .: "faceOffWinPercentage"
    blocked   <- o .: "blocked"
    takeaways <- o .: "takeaways"
    giveaways <- o .: "giveaways"
    hits      <- o .: "hits"
    return TeamGameStats { .. }

-- "root" JSON Object for TeamGameStats, takes either "Home" or "Away"
-- TODO: Maybe make a Type for Home and Away to pass through to parsers?
baseTeamGameStats :: T.Text -> Object -> Parser Value
baseTeamGameStats team o =
    (o .: "liveData")
        >>= (.: "boxscore")
        >>= (.: "teams")
        >>= (.: team)
        >>= (.: "teamStats")
        >>= (.: "teamSkaterStats")

{--------------------------- Parsing Player -------------------------}
pPlayerID :: Value -> Parser PlayerID
pPlayerID = withObject "PlayerID" $ \o -> PlayerID <$> o .: "id"

pPName :: Value -> Parser PName
pPName = withObject "PName" $ \o -> do
    firstName <- o .: "firstName"
    lastName  <- o .: "lastName"
    fullName  <- o .: "fullName"
    return PName { .. }

pPosition :: Value -> Parser Position
pPosition = withObject "Position"
    $ \o -> (o .: "primaryPosition") >>= fmap strToPos . (.: "code")

pHand :: Value -> Parser Handedness
pHand = withObject "Handedness" $ \o -> strToHand <$> o .: "shootsCatches"

pPlayerInfo :: Value -> Parser PlayerInfo
pPlayerInfo o = basePlayerInfo o >>= pPlayerInfo'

pPlayerInfo' :: Value -> Parser PlayerInfo
pPlayerInfo' = withObject "PlayerInfo" $ \o -> do
    playerID <- pPlayerID $ toValue o
    teamID   <- pTeamID $ toValue o
    name     <- pPName $ toValue o
    number   <- fmap toInt <$> o .:? "primaryNumber"
    position <- pPosition $ toValue o
    hand     <- pHand $ toValue o
    rookie   <- o .: "rookie"
    age      <- o .: "currentAge"
    return PlayerInfo { .. }

basePlayerInfo :: Value -> Parser Value
basePlayerInfo o = toObject o .: "people" >>= return . fromArray 0

{------------------------ Parsing  Schedule -----------------------}

--instance FromJSON GamesList where

--parseSchedule :: Value -> Parser [(Day, Game)]
--parseSchedule = withObject "GamesList" $ \o -> do
    --dates       <- HM.toList . toObject <$> o .: "dates"
    --parsedGames <- for dates $ \(_, days) -> do
        --let days' = toObject days
        --date  <- parseTime <$> days' .: "date"
        --games <- days' .: "games"
        --for (HM.toList games) $ \(_, game) -> do
            --game' <- parseJSON game
            --return (date, game')
    --return $ concat parsedGames

--parseTime :: T.Text -> Day
--parseTime = parseTimeOrError True defaultTimeLocale "%F" . T.unpack

{------------------------ Parsing  Games -----------------------}
pGameID :: Value -> Parser GameID
pGameID = withObject "GameID" $ \o -> GameID <$> o .: "gamePk"

pGameDate :: Value -> Parser GameDate
pGameDate = withObject "GameDate" $ \o ->
    (o .: "gameData")
        >>= (.: "datetime")
        >>= fmap (GameDate . pUTCTime)
        <$> (.: "datetime")

pGameTime :: Value -> Parser GameTime
pGameTime =
    withObject "GameTime" $ \o -> GameTime <$> o .: "currentPeriodTimeRemaining"

pUTCTime :: T.Text -> UTCTime
pUTCTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%XZ" . T.unpack

-- when parsing currentPeriod we will only see "currentPeriod"
-- same with parsing GamePeriods, only can see the "num"
pPeriod :: Value -> Parser Period
pPeriod = withObject "Period"
    $ \o -> strToPer <$> asum [o .: "currentPeriod", o .: "num"]

pCurrentPeriod :: Value -> Parser CurrentPeriod
pCurrentPeriod = withObject "CurrentPeriod" $ \o -> do
    baseCP <- baseCurrentPeriod $ toValue o
    CurrentPeriod <$> pPeriod baseCP <*> pGameTime baseCP

baseCurrentPeriod :: Value -> Parser Value
baseCurrentPeriod o = (toObject o .: "liveData") >>= (.: "linescore")

pTeamPeriodStats :: Value -> Parser TeamPeriodStats
pTeamPeriodStats = withObject "TeamPeriodStats" $ \o -> do
    goals <- o .: "goals"
    shots <- o .: "shotsOnGoal"
    return TeamPeriodStats { .. }

pPeriodStats :: Value -> Parser PeriodStats
pPeriodStats = withObject "PeriodStats" $ \o -> do
    homeBase <- o .: "home"
    awayBase <- o .: "away"
    home     <- pTeamPeriodStats homeBase
    away     <- pTeamPeriodStats awayBase
    return PeriodStats { .. }

-- /game/{GAMEID}/feed/live parses liveData.linescore.periods section
pGamePeriods :: Value -> Parser GamePeriods
pGamePeriods o = baseGamePeriods o >>= fmap M.fromList . pGamePeriods'

pGamePeriods' :: Value -> Parser [(Period, PeriodStats)]
pGamePeriods' = withObject "ParseGamePeriods" $ \o -> do
    for (HM.toList o) $ \(_, val) -> do
        period      <- pPeriod val
        periodStats <- pPeriodStats val
        return (period, periodStats)

baseGamePeriods :: Value -> Parser Value
baseGamePeriods o =
    (toObject o .: "liveData") >>= (.: "linescore") >>= (.: "periods")

pGameLive :: Value -> Parser GameLive
pGameLive = withObject "GameLive" $ \o -> do
    gameID        <- pGameID $ toValue o
    (home, away)  <- pTeamGameStats $ toValue o
    currentPeriod <- pCurrentPeriod $ toValue o
    periodStats   <- pGamePeriods $ toValue o
    gameDate      <- pGameDate $ toValue o
    return GameLive { .. }

pGameFinal :: Value -> Parser GameFinal
pGameFinal = withObject "GameFinal" $ \o -> do
    gameID       <- pGameID $ toValue o
    (home, away) <- pTeamGameStats $ toValue o
    periodStats  <- pGamePeriods $ toValue o
    gameDate     <- pGameDate $ toValue o
    return GameFinal { .. }
