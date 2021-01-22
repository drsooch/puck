module Puck.Database.Core
    ( connectDB
    , executeMany
    , queryMaybe
    , insertTeamInfo
    , insertTeamInfoNoID
    , insertLeague
    , insertLeagueNoID
    , insertPlayerInfo
    , insertPlayerSeasonStats
    , insertSkaterSeasonStats
    -- selection
    , selectPosition
    , selectLeagueByName
    , selectLeagueByID
    , selectTeamInfoByID
    , selectTeamInfoByName
    -- defaults
    , mkDefaultPlayerSeasonDB
    ) where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Data.Text                      ( Text )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Database.SQLite.Simple         ( Connection
                                                , FromRow
                                                , Only(..)
                                                , Query
                                                , SQLError
                                                , ToRow
                                                , execute
                                                , open
                                                , query
                                                , toRow
                                                , withTransaction
                                                )

import           Puck.Database.Types
import           Puck.Types
import           Puck.Types.Error

connectDB :: FilePath -> IO Connection
connectDB = open

{-------------------------- Wrapped Queries --------------------------}

executeMany
    :: ToRow q => Connection -> (q -> Connection -> IO ()) -> [q] -> IO ()
executeMany conn f dat = withTransaction conn (mapM_ (flip f conn) dat)

-- wrapper around query to catch SQL Failures
queryMaybe
    :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO (Maybe [r])
queryMaybe conn queryStr dat = catch
    (Just <$> query conn queryStr dat)
    (\e -> do
        let err = show (e :: SomeException)
        hPutStrLn stderr ("Warning: " <> err)
        hPutStrLn stderr ("With: " <> show queryStr)
        return Nothing
    )

singleRow :: Maybe [r] -> Maybe r
singleRow Nothing         = Nothing
singleRow (Just []      ) = Nothing
singleRow (Just (r : rs)) = Just r

{-------------------------- Database Insertion --------------------------}

insertLeague :: LeagueDB -> Connection -> IO ()
insertLeague dat conn = execute
    conn
    "INSERT INTO league (league_id, league_name) VALUES (?,?) ON CONFLICT DO NOTHING"
    dat

insertLeagueNoID :: Text -> Connection -> IO ()
insertLeagueNoID dat conn =
    execute conn "INSERT INTO league (league_name) VALUES (?)" (Only dat)

insertPlayerInfo :: PlayerInfoDB -> Connection -> IO ()
insertPlayerInfo dat conn = execute
    conn
    "INSERT INTO player VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"
    dat

insertTeamInfo :: TeamInfoDB -> Connection -> IO ()
insertTeamInfo dat conn =
    execute conn "INSERT INTO team VALUES (?, ?, ?, ?, ?, ?, ?)" dat

insertTeamInfoNoID :: TeamInfoDB -> Connection -> IO ()
insertTeamInfoNoID dat conn = execute
    conn
    "INSERT INTO team(name, abbreviation, division, conference, franchise_id, league_id)VALUES (?, ?, ?, ?, ?, ?)"
    (toRowTINoID dat)

-- overall records are stored in separate tables
insertTeamSeasonStats :: TeamSeasonsStatsDB -> Connection -> IO ()
insertTeamSeasonStats t@TeamSeasonsStatsDB {..} conn =
    insertHomeRecord teamID season (homeRecord splits) conn
        >> insertAwayRecord teamID season (awayRecord splits) conn
        >> insertShootoutRecord teamID season (shootoutRecord splits) conn
        >> insertLastTenRecord teamID season (lastTen splits) conn
        >> insertTeamSeasonStats' t conn

insertTeamSeasonStats' :: TeamSeasonsStatsDB -> Connection -> IO ()
insertTeamSeasonStats' dat conn = execute
    conn
    "INSERT INTO team_season_stats VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"
    dat

insertHomeRecord :: TeamID -> SeasonID -> Record -> Connection -> IO ()
insertHomeRecord tid sid Record {..} conn = execute
    conn
    "INSERT INTO home_record VALUES (?, ?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses, otl)

insertAwayRecord :: TeamID -> SeasonID -> Record -> Connection -> IO ()
insertAwayRecord tid sid Record {..} conn = execute
    conn
    "INSERT INTO away_record VALUES (?, ?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses, otl)

-- shootout doesnt have an otl
insertShootoutRecord :: TeamID -> SeasonID -> Record -> Connection -> IO ()
insertShootoutRecord tid sid Record {..} conn = execute
    conn
    "INSERT INTO shootout_record VALUES (?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses)

insertLastTenRecord :: TeamID -> SeasonID -> Record -> Connection -> IO ()
insertLastTenRecord tid sid Record {..} conn = execute
    conn
    "INSERT INTO last_ten VALUES (?, ?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses, otl)

insertPlayerSeasonStats :: PlayerStatsDB -> Connection -> IO ()
insertPlayerSeasonStats (SSeasonStats stats) conn =
    insertSkaterSeasonStats stats conn
insertPlayerSeasonStats (GSeasonStats stats) conn =
    insertGoalieSeasonStats stats conn
insertPlayerSeasonStats x _ = error $ "Can't do anything here" <> show x

insertSkaterSeasonStats :: SkaterSeasonDB -> Connection -> IO ()
insertSkaterSeasonStats dat conn = execute
    conn
    "insert into skater_season_stats values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now')) ON CONFLICT DO NOTHING"
    dat

insertGoalieSeasonStats :: GoalieSeasonDB -> Connection -> IO ()
insertGoalieSeasonStats dat conn = execute
    conn
    "insert into goalie_season_stats values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now')) ON CONFLICT DO NOTHING"
    dat


insertBaseStandings :: BaseStandingsDB -> Connection -> IO ()
insertBaseStandings dat conn =
    execute conn "INSERT INTO base_standings VALUES (?, ?, ?)" dat

{-------------------------- Database Selection --------------------------}

selectTeamInfoByID :: TeamID -> Connection -> IO (Maybe TeamInfo)
selectTeamInfoByID tid conn = singleRow
    <$> queryMaybe conn "SELECT * FROM team WHERE team_id = ?" (Only tid)

selectTeamInfoByName :: Text -> LeagueID -> Connection -> IO (Maybe TeamInfo)
selectTeamInfoByName tid lid conn = singleRow <$> queryMaybe
    conn
    "SELECT * FROM team WHERE name = ? AND league_id = ?"
    (tid, lid)

selectPlayerInfo :: Int -> Connection -> IO (Maybe PlayerInfo)
selectPlayerInfo pid conn = singleRow <$> queryMaybe
    conn
    "SELECT * FROM player WHERE player_id = ?"
    (Only pid)

selectPosition :: PlayerID -> Connection -> IO (Maybe Position)
selectPosition pid conn = singleRow <$> queryMaybe
    conn
    "SELECT position FROM player WHERE player_id = ?"
    (Only pid)

selectLeagueByID :: Text -> Connection -> IO (Maybe LeagueDB)
selectLeagueByID lid conn = singleRow <$> queryMaybe
    conn
    "SELECT * from league WHERE league_id = ?"
    (Only lid)

selectLeagueByName :: Text -> Connection -> IO (Maybe LeagueDB)
selectLeagueByName lname conn = singleRow <$> queryMaybe
    conn
    "SELECT * from league WHERE league_name = ?"
    (Only lname)

{----------------------- Defaults that rely on Database  -----------------------}
mkDefaultPlayerSeasonDB
    :: PlayerID -> SeasonID -> TeamID -> Connection -> IO PlayerStatsDB
mkDefaultPlayerSeasonDB pid sid tid conn = do
    print "Entered default"
    pos <- selectPosition pid conn
    case pos of
        Just Goalie ->
            return $ GSeasonStats $ mkDefaultGoalieSeasonDB pid sid tid
        Just f -> return $ SSeasonStats $ mkDefaultSkaterSeasonDB pid sid tid
        _      -> error "No valid position returned"
