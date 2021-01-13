module Puck.Database.Core
    ( connectDB
    )
where

import           Data.Text                      ( breakOn )
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Control.Exception
import           Puck.Utils
import           Puck.Types
import           Puck.Database.Types

type ExecuteResult = IO (Either SQLError ())
type QueryResult r = IO (Either SQLError [r])

connectDB :: IO (Either SQLError Connection)
connectDB = try (open "./doesntexist/testing.json")

setupDB :: Connection -> ExecuteResult
setupDB = undefined

-- wrapper around execute to catch SQL Failures
executeMaybe :: ToRow q => Connection -> Query -> q -> ExecuteResult
executeMaybe conn queryStr dat = try $ execute conn queryStr dat

-- wrapper around query to catch SQL Failures
queryMaybe :: (ToRow q, FromRow r) => Connection -> Query -> q -> QueryResult r
queryMaybe conn queryStr dat = try $ query conn queryStr dat

{-------------------------- Database Insertion --------------------------}

insertLeague :: Connection -> LeagueDB -> ExecuteResult
insertLeague conn =
    executeMaybe conn "INSERT INTO league (league_id, league_name) VALUES (?,?)"

-- certain leagues don't come with an ID
insertLeagueNoID :: Connection -> LeagueDB -> ExecuteResult
insertLeagueNoID conn LeagueDB {..} = executeMaybe
    conn
    "INSERT INTO league (league_name) VALUES (?)"
    (Only leagueName)

insertPlayerInfo :: Connection -> PlayerInfoDB -> ExecuteResult
insertPlayerInfo conn = executeMaybe
    conn
    "INSERT INTO player VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"

-- FIXME: LeagueID is not part of TeamInfo
insertTeamInfo :: Connection -> TeamInfoDB -> ExecuteResult
insertTeamInfo conn =
    executeMaybe conn "INSERT INTO team VALUES (?, ?, ?, ?, ?, ?, ?, ?)"

-- overall records are stored in separate tables
insertTeamSeasonStats :: Connection -> TeamSeasonsStatsDB -> ExecuteResult
insertTeamSeasonStats conn t@TeamSeasonsStatsDB {..} =
    insertHomeRecord conn teamID season (homeRecord splits)
        >> insertAwayRecord conn teamID season (awayRecord splits)
        >> insertShootoutRecord conn teamID season (shootoutRecord splits)
        >> insertLastTenRecord conn teamID season (lastTen splits)
        >> insertTeamSeasonStats conn t

insertTeamSeasonStats' :: Connection -> TeamSeasonsStatsDB -> ExecuteResult
insertTeamSeasonStats' conn = executeMaybe
    conn
    "INSERT INTO team_season_stats VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"

insertHomeRecord :: Connection -> TeamID -> SeasonID -> Record -> ExecuteResult
insertHomeRecord conn tid sid Record {..} = executeMaybe
    conn
    "INSERT INTO home_record VALUES (?, ?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses, otl)

insertAwayRecord :: Connection -> TeamID -> SeasonID -> Record -> ExecuteResult
insertAwayRecord conn tid sid Record {..} = executeMaybe
    conn
    "INSERT INTO away_record VALUES (?, ?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses, otl)

-- shootout doesnt have an otl
insertShootoutRecord
    :: Connection -> TeamID -> SeasonID -> Record -> ExecuteResult
insertShootoutRecord conn tid sid Record {..} = executeMaybe
    conn
    "INSERT INTO shootout_record VALUES (?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses)

insertLastTenRecord
    :: Connection -> TeamID -> SeasonID -> Record -> ExecuteResult
insertLastTenRecord conn tid sid Record {..} = executeMaybe
    conn
    "INSERT INTO last_ten VALUES (?, ?, ?, ?, ?)"
    (getTeamID tid, getSeasonID sid, wins, losses, otl)

insertSkaterSeasonStats :: Connection -> SkaterSeasonDB -> ExecuteResult
insertSkaterSeasonStats conn = executeMaybe
    conn
    "insert into skater_season_stats values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertGoalieSeasonStats :: Connection -> GoalieSeasonDB -> ExecuteResult
insertGoalieSeasonStats conn = executeMaybe
    conn
    "insert into goalie_season_stats values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertBaseStandings :: Connection -> BaseStandingsDB -> ExecuteResult
insertBaseStandings conn =
    executeMaybe conn "INSERT INTO base_standings VALUES (?, ?, ?)"

{-------------------------- Database Selection --------------------------}

selectTeamInfo :: Connection -> Int -> QueryResult TeamInfo
selectTeamInfo conn tid =
    queryMaybe conn "SELECT * FROM team WHERE team_id = ?" (Only tid)

selectPlayerInfo :: Connection -> Int -> QueryResult PlayerInfo
selectPlayerInfo conn pid =
    queryMaybe conn "SELECT * FROM player WHERE player_id = ?" (Only pid)

