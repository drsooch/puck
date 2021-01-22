module Puck.Parser
    (
    -- main parser
      parseMaybe
    -- team parsers
    , pAllNHLTeams
    , pTeamInfo
    , pTeamInfoNHL
    , pTeamInfoExtra
    , pTeamSeasonStats
    , pTeamRecord
    , pTeamGameStats
    , pTeamRoster
    -- player parsers
    , pPlayerInfo
    , pPlayerSeasonStats
    , pSkaterSeasonStats
    , pGoalieSeasonStats
    , pCareerStats
    -- game parsers
    , pGameLive
    , pGameFinal
    , pGamePreview
    -- misc parsers
    , pCurrentSeason
    ) where

import           Control.Applicative            ( (<|>) )
import           Control.Exception              ( throw )
import           Data.Maybe                     ( fromMaybe )
import           System.IO                      ( IOMode(..)
                                                , hClose
                                                , hPutStrLn
                                                , openFile
                                                , stderr
                                                )

import           Data.Aeson                     ( (.!=)
                                                , (.:)
                                                , (.=)
                                                , FromJSON(parseJSON)
                                                , Value(Array, Object)
                                                , decode
                                                , withArray
                                                , withObject
                                                )

import           Data.Aeson.Types        hiding ( parseEither
                                                , parseMaybe
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Foldable                  ( asum )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeOrError
                                                )
import           Data.Traversable               ( for )
import           Data.Vector                    ( Vector )

import qualified Data.Aeson.Types              as DAT
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Vector                   as V

import           Puck.Database.Types
import           Puck.Types
import           Puck.Types.Error
import           Puck.Utils

-- replaces DAT.parseEither
parseMaybe
    :: (Value -> Parser b)         -- parser
    -> Maybe ByteString            -- response if valid json
    -> IO (Maybe b)
parseMaybe p Nothing    = return Nothing
parseMaybe p (Just txt) = do
    case DAT.parseEither p (decodeJson txt) of
        Left err ->
            do
                f <- openFile "/home/sooch/.puck/log.txt" AppendMode
                hPutStrLn f ("Warning: " <> err)
                hPutStrLn f (show txt)
                hPutStrLn f ""
                hClose f
            >> return Nothing
        Right parsed -> return $ Just parsed

-- takes the place of Data.Aeson.decode so we can directly pass into parseEither
decodeJson :: ByteString -> Value
decodeJson txt =
    let valid (Just val) = val
        valid Nothing    = throw $ ParseError "" "Unable to decode into json"
    in  valid $ decode txt

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

toToi :: Value -> Double
toToi (String xs) = toiToDouble xs
toToi x           = error $ "Failed toToi: " <> show x

toInt :: Value -> Int
toInt (String xs) = read $ T.unpack xs
toInt x           = error $ "Failed toInt: " <> show x

fromArray :: Int -> Value -> Value
fromArray ix (Array xs) = xs V.! ix
fromArray _  x          = error $ "Failed fromArray: " <> show x

{----------------------- Parsing Team -------------------------}
pTeamID :: Value -> Parser TeamID
pTeamID = withObject "TeamID" $ \o -> do
    num <- asum
        [ (o .: "currentTeam") >>= (.: "id") -- Player Endpoint
        , o .: "id" -- Team endpoint
        ]
    return $ TeamID num

pFranchiseId :: Value -> Parser FranchiseID
pFranchiseId =
    withObject "FranchiseID" $ \o -> FranchiseID <$> o .: "franchiseId"

pDivision :: Value -> Parser DivisionID
pDivision = withObject "Division"
    $ \o -> (o .: "division") >>= fmap DivisionID . (.: "id")

pConference :: Value -> Parser ConferenceID
pConference = withObject "Conference"
    $ \o -> (o .: "conference") >>= fmap ConferenceID . (.: "id")

-- Don't want to use Vectors
-- parses entire /teams endpoint 
pAllNHLTeams :: Value -> Parser [TeamInfoDB]
pAllNHLTeams = fmap V.toList . pAllNHLTeams'

pAllNHLTeams' :: Value -> Parser (Vector TeamInfoDB)
pAllNHLTeams' = withObject "pAllNHLTeams" $ \o -> do
    (Array teams) <- o .: "teams"
    for teams pTeamInfoNHL

pTeamInfo :: Value -> Parser TeamInfoDB
pTeamInfo v = do
    base <- baseTeamInfo v
    asum [pTeamInfoNHL base, pTeamInfoExtra base]

baseTeamInfo :: Value -> Parser Value
baseTeamInfo o = fromArray 0 <$> toObject o .: "teams"

-- NHL teams will always have the Just elements
-- We explicitly expect Division, Conference, and Franchise
-- so we can fail and try the TeamInfoExtra parser
pTeamInfoNHL :: Value -> Parser TeamInfoDB
pTeamInfoNHL = withObject "TeamInfoNHL" $ \o -> do
    teamID     <- pTeamID $ toValue o
    name       <- o .: "name"
    abbrev     <- o .: "abbreviation"
    division   <- Just <$> pDivision (toValue o)
    conference <- Just <$> pConference (toValue o)
    active     <- o .: "active" .!= False
    franID     <- Just <$> pFranchiseId (toValue o)
    let leagueID = LeagueID 133   -- NHL League Number
    return TeamInfoDB { .. }


pTeamInfoExtra :: Value -> Parser TeamInfoDB
pTeamInfoExtra = withObject "TeamInfo" $ \o -> do
    teamID <- pTeamID $ toValue o
    name   <- asum
        [ o .: "name"
        , do
            ln <- o .: "locationName"
            tn <- o .: "teamName"
            return $ ln <> " " <> tn
        ]
    abbrev <- o .:? "abbreviation" .!= Nothing
    let division   = Nothing
    let conference = Nothing
    active <- o .:? "active" .!= False
    let franID   = Nothing
    let leagueID = LeagueID (-1) -- I know...
    return TeamInfoDB { .. }

-- parses /teams/{ID}?expand=team.stats&season={SEASON}
pTeamSeasonStats :: Value -> Parser TeamSeasonsStatsDB
pTeamSeasonStats o = baseTeamSeasonStats o >>= pTeamSeasonStats'

-- NOTE: certain data is uninitialized as the actual data is on a 
-- separate endpoint, this data will be merged afterwards
pTeamSeasonStats' :: Value -> Parser TeamSeasonsStatsDB
pTeamSeasonStats' = withObject "TeamSeasonsStats" $ \o -> do
    teamID      <- pTeamID $ toValue o
    gamesPlayed <- o .: "gamesPlayed"
    -- get parsed into a Record
    wins        <- o .: "wins"
    losses      <- o .: "losses"
    otl         <- o .: "ot"
    let record = Record { .. }
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
pTeamRecord :: Value -> Parser RecordSplits
pTeamRecord o = baseOverallRecord o >>= pTeamRecord'

-- could've folded over the array instead of hardcoding,
-- but this works for now
pTeamRecord' :: Value -> Parser RecordSplits
pTeamRecord' = withArray "RecordSplits" $ \a -> do
    homeRecord     <- pSingleRecord $ a V.! 0
    awayRecord     <- pSingleRecord $ a V.! 1
    shootoutRecord <- pSingleRecord $ a V.! 2
    lastTen        <- pSingleRecord $ a V.! 3
    return RecordSplits { .. }

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

pTeamRoster :: Value -> Parser [PlayerID]
pTeamRoster = fmap V.toList . pTeamRoster'

pTeamRoster' :: Value -> Parser (Vector PlayerID)
pTeamRoster' v = do
    (Array roster) <- baseTeamRoster v
    for roster $ \player -> do
        pid <- toObject player .: "person" >>= (.: "id")
        return $ PlayerID pid


baseTeamRoster :: Value -> Parser Value
baseTeamRoster o =
    toObject o
        .:  "teams"
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "roster")
        >>= (.: "roster")

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

pPlayerInfo :: Value -> Parser PlayerInfoDB
pPlayerInfo o = basePlayerInfo o >>= pPlayerInfo'

pPlayerInfo' :: Value -> Parser PlayerInfoDB
pPlayerInfo' = withObject "PlayerInfo" $ \o -> do
    playerID <- pPlayerID $ toValue o
    teamID   <- pTeamID $ toValue o
    name     <- pPName $ toValue o
    number   <- fmap toInt <$> o .:? "primaryNumber"
    position <- pPosition $ toValue o
    hand     <- pHand $ toValue o
    rookie   <- o .: "rookie"
    age      <- o .: "currentAge"
    return PlayerInfoDB { .. }

basePlayerInfo :: Value -> Parser Value
basePlayerInfo o = toObject o .: "people" >>= return . fromArray 0

pPlayerSeasonStats
    :: PlayerID -> SeasonID -> TeamID -> Value -> Parser PlayerStatsDB
pPlayerSeasonStats pid sid tid v = asum
    [ pEmptyStatPage v
    , SSeasonStats <$> pSkaterSeasonStats pid sid tid v
    , GSeasonStats <$> pGoalieSeasonStats pid sid tid v
    ]

-- this is for Career Stats, it ignores the base
pPlayerSeasonStats'
    :: PlayerID -> SeasonID -> TeamID -> Value -> Parser PlayerStatsDB
pPlayerSeasonStats' pid sid tid v = asum
    [ SSeasonStats <$> pSkaterSeasonStats' pid sid tid v
    , GSeasonStats <$> pGoalieSeasonStats' pid sid tid v
    ]

pEmptyStatPage :: Value -> Parser PlayerStatsDB
pEmptyStatPage o =
    toObject o
        .:  "stats"
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "splits")
        >>= \case
                (Array vec) ->
                    if null vec then return EmptyStatPage else fail "Not Empty"
                _ -> fail "Not Empty"

pSkaterSeasonStats
    :: PlayerID -> SeasonID -> TeamID -> Value -> Parser SkaterSeasonDB
pSkaterSeasonStats pid sid tid o =
    baseSkaterSeasonStats o >>= pSkaterSeasonStats' pid sid tid

-- does not parse seasonID, teamID, nor playerID as they aren't present
-- in JSON
pSkaterSeasonStats'
    :: PlayerID -> SeasonID -> TeamID -> Value -> Parser SkaterSeasonDB
pSkaterSeasonStats' pid sid tid = withObject "SkaterSeasonDB" $ \o -> do
    let playerID = pid
    let season   = sid
    let teamID   = tid
    games    <- o .:? "games" .!= 0
    toi      <- toToi <$> o .:? "timeOnIce" .!= "00:00"
    goals    <- o .:? "goals" .!= 0
    assists  <- o .: "assists" .!= 0
    points   <- o .:? "points" .!= 0
    ppGoals  <- o .:? "powerPlayGoals" .!= 0
    ppPoints <- o .:? "powerPlayPoints" .!= 0
    let ppAssists = ppPoints - ppGoals
    ppToi    <- toToi <$> o .:? "powerPlayTimeOnIce" .!= "00:00"
    shGoals  <- o .:? "shortHandedGoals" .!= 0
    shPoints <- o .:? "shortHandedPoints" .!= 0
    let shAssists = shPoints - shGoals
    shToi <- toToi <$> o .:? "shortHandedTimeOnIce" .!= "00:00"
    let evGoals   = goals - (ppGoals + shGoals)
    let evPoints  = points - (ppPoints + shPoints)
    let evAssists = assists - (ppAssists + shAssists)
    evToi       <- toToi <$> o .:? "evenTimeOnIce" .!= "00:00"
    gwg         <- o .:? "gameWinningGoals" .!= 0
    shots       <- o .:? "shots" .!= 0
    hits        <- o .:? "hits" .!= 0
    pims        <- o .:? "pim" .!= 0
    blocked     <- o .:? "blocked" .!= 0
    plusMinus   <- o .:? "plusMinus" .!= 0
    faceOffPct  <- o .:? "faceOffPct" .!= 0
    shootingPct <- o .:? "shotPct" .!= 0
    shifts      <- o .:? "shifts" .!= 0
    return SkaterSeasonDB { .. }

baseSkaterSeasonStats :: Value -> Parser Value
baseSkaterSeasonStats o =
    toObject o
        .:  "stats"
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "splits")
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "stat")

pGoalieSeasonStats
    :: PlayerID -> SeasonID -> TeamID -> Value -> Parser GoalieSeasonDB
pGoalieSeasonStats pid sid tid o =
    baseSkaterSeasonStats o >>= pGoalieSeasonStats' pid sid tid

pGoalieSeasonStats'
    :: PlayerID -> SeasonID -> TeamID -> Value -> Parser GoalieSeasonDB
pGoalieSeasonStats' pid sid tid = withObject "GoalieSeasonDB" $ \o -> do
    let playerID = pid
    let season   = sid
    let teamID   = tid
    games        <- o .:? "games" .!= 0
    gamesStarted <- o .:? "gamesStarted" .!= 0
    wins         <- o .:? "wins" .!= 0
    otl          <- o .:? "ot" .!= 0
    losses       <- o .:? "losses" .!= 0
    let record = Record { .. }
    toi          <- toToi <$> o .:? "timeOnIce" .!= "00:00"
    shutouts     <- o .:? "shutouts" .!= 0
    saves        <- o .:? "saves" .!= 0
    savePct      <- o .:? "savePercentage" .!= 0
    gaa          <- o .:? "goalAgainstAverage" .!= 0
    shotsAgainst <- o .:? "shotsAgainst" .!= 0
    goalsAgainst <- o .:? "goalsAgainst" .!= 0
    ppSaves      <- o .:? "powerPlaySaves" .!= 0
    ppShots      <- o .:? "powerPlayShots" .!= 0
    ppSavePct    <- (\sp -> sp / 100) <$> o .:? "powerPlaySavePercentage" .!= 0
    shSaves      <- o .:? "shortHandedSaves" .!= 0
    shShots      <- o .:? "shortHandedShots" .!= 0
    shSavePct <- (\sp -> sp / 100) <$> o .:? "shortHandedSavePercentage" .!= 0
    evShots      <- o .:? "evenShots" .!= 0
    evSaves      <- o .:? "evenSaves" .!= 0
    evSavePct <- (\sp -> sp / 100) <$> o .:? "evenStrengthSavePercentage" .!= 0
    return GoalieSeasonDB { .. }

pcs :: PlayerID -> Value -> Parser Value
pcs pid = withObject "CareerStats" $ \o -> baseCareerStats (toValue o)

pCareerStats :: PlayerID -> Value -> Parser CareerStatsDB
pCareerStats pid = withObject "CareerStats" $ \o ->
    baseCareerStats (toValue o) >>= pCareerStats' pid >>= return . V.toList

pCareerStats' :: PlayerID -> Value -> Parser (Vector SingleSeasonCS)
pCareerStats' pid =
    withArray "CareerStats" $ \seasons -> for seasons (pCareerStats'' pid)

pCareerStats'' :: PlayerID -> Value -> Parser SingleSeasonCS
pCareerStats'' pid = withObject "CareerStatsSingleSeason" $ \o -> do
    season <- SeasonID . toInteger . toInt <$> o .: "season" :: Parser SeasonID
    baseLeague <- o .: "league" :: Parser Value
    baseTeam   <- o .: "team" :: Parser Value
    leagueName <- toObject baseLeague .: "name"
    mLeagueID  <- toObject baseLeague .:? "id" .!= Nothing :: Parser (Maybe Int)
    mTeamID    <- toObject baseTeam .:? "id" .!= Nothing :: Parser (Maybe Int)
    let teamID   = TeamID <$> mTeamID
    let leagueID = LeagueID <$> mLeagueID
    teamName <- toObject baseTeam .: "name"
    psDB     <- o .: "stat" >>= pPlayerSeasonStats'
        pid
        season
        (fromMaybe (TeamID 0) teamID)
    return (psDB, (teamID, teamName), (leagueID, leagueName))

baseCareerStats :: Value -> Parser Value
baseCareerStats o =
    toObject o .: "stats" >>= return . toObject . fromArray 0 >>= (.: "splits")

{------------------------ Parsing  Schedule -----------------------}

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

pGamePreview :: Value -> Parser GamePreview
pGamePreview = withObject "GamePreview" $ \o -> do
    gameID   <- pGameID $ toValue o
    gameDate <- pGameDate $ toValue o
    return GamePreview { .. }

-- gets the seasons ID, used for a multitude of purposes
pCurrentSeason :: Value -> Parser SeasonID
pCurrentSeason o =
    (toObject o .: "seasons")
        >>= return
        .   toObject
        .   fromArray 0
        >>= (.: "seasonId")
        >>= return
        .   SeasonID
        .   toInteger  -- I know this is gross, but the season ID may overflow
        .   toInt


