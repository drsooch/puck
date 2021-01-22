module Puck.Database.Schema
    ( setupDB
    ) where

import           Control.Exception              ( try )
import           Data.Bifunctor                 ( first )
import           Data.Either                    ( lefts )

import           Database.SQLite.Simple         ( Connection
                                                , FromRow
                                                , Only(..)
                                                , Query
                                                , SQLError
                                                , ToRow
                                                , execute
                                                , open
                                                , query
                                                , withConnection
                                                )

import           Puck.Database.Core
import           Puck.Database.Types
import           Puck.Types
import           Puck.Types.Error


{-------------------------- Create Database --------------------------}

-- creates all necessary tables, filters out all the Rights
-- If any table creations fail fail we return them all
setupDB :: FilePath -> IO (Either PuckError ())
setupDB fp =
    fmap (first $ DBError "SetupDB") $ try $ withConnection fp setupDB'

setupDB' :: Connection -> IO ()
setupDB' conn = sequence_
    [ createLeagueTable conn
    , createTeamTable conn
    , createPlayerTable conn
    , createSkaterSeasonTable conn
    , createGoalieSeasonTable conn
    , createHomeTable conn
    , createAwayTable conn
    , createShootoutTable conn
    , createLastTenTable conn
    , createBaseStandings conn
    , createStandingsView conn
    , createAggSkaterView conn
    , createAggGoalieView conn
    ]


{-------------------------- Database Creation --------------------------}

createLeagueTable :: Connection -> IO ()
createLeagueTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS league (\n \
\    league_id INTEGER PRIMARY KEY, \n \
\    league_name TEXT \n \
\);"
    ()

createTeamTable :: Connection -> IO ()
createTeamTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS team (\n \
\    team_id INTEGER PRIMARY KEY,\n \
\    name TEXT NOT NULL,\n \
\    abbreviation TEXT,\n \
\    division INTEGER,\n \
\    conference INTEGER,\n \
\    franchise_id INTEGER,\n \
\    league_id INTEGER,\n \
\    FOREIGN KEY (league_id)\n \
\        REFERENCES league (league_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createPlayerTable :: Connection -> IO ()
createPlayerTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS player (\n \
    \    player_id INTEGER PRIMARY KEY,\n \
    \    team_id INTEGER,\n \
    \    name TEXT NOT NULL,\n \
    \    jersey INTEGER,\n \
    \    position TEXT,\n \
    \    handedness TEXT,\n \
    \    age INTEGER,\n \
    \    rookie BOOLEAN CHECK (rookie IN (0, 1)),\n \
    \    last_update TEXT NOT NULL,\n \
    \    FOREIGN KEY (team_id)\n \
    \        REFERENCES team (team_id)\n \
    \        ON DELETE CASCADE\n \
    \        ON UPDATE NO ACTION\n \
    \);"
    ()

createSkaterSeasonTable :: Connection -> IO ()
createSkaterSeasonTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS skater_season_stats (\n \
\    player_id INTEGER,\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    games INTEGER,\n \
\    toi REAL,\n \
\    goals INTEGER,\n \
\    assists INTEGER,\n \
\    points INTEGER,\n \
\    pp_goals INTEGER,\n \
\    pp_assists INTEGER,\n \
\    pp_points INTEGER,\n \
\    pp_toi REAL,\n \
\    sh_goals INTEGER,\n \
\    sh_assists INTEGER,\n \
\    sh_points INTEGER,\n \
\    sh_toi REAL,\n \
\    ev_goals INTEGER,\n \
\    ev_assists INTEGER,\n \
\    ev_points INTEGER,\n \
\    ev_toi REAL,\n \
\    gwg INTEGER,\n \
\    shots INTEGER,\n \
\    hits INTEGER,\n \
\    pims INTEGER,\n \
\    blocked INTEGER,\n \
\    plus_minus INTEGER,\n \
\    faceoff_pct REAL,\n \
\    shooting_pct REAL,\n \
\    shifts INTEGER,\n \
\    last_update TEXT NOT NULL,\n \
\    PRIMARY KEY (player_id, team_id, season_id),\n \
\    FOREIGN KEY (player_id)\n \
\        REFERENCES player (player_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION,\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createGoalieSeasonTable :: Connection -> IO ()
createGoalieSeasonTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS goalie_season_stats (\n \
\    player_id INTEGER,\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    games INTEGER,\n \
\    games_started INTEGER,\n \
\    toi REAL,\n \
\    wins INTEGER,\n \
\    losses INTEGER,\n \
\    ot_losses INTEGER,\n \
\    shutouts INTEGER,\n \
\    saves INTEGER,\n \
\    save_pct REAL,\n \
\    gaa REAL,\n \
\    shots_against INTEGER,\n \
\    goals_against INTEGER,\n \
\    pp_saves INTEGER,\n \
\    pp_shots INTEGER,\n \
\    pp_save_pct REAL,\n \
\    sh_saves INTEGER,\n \
\    sh_shots INTEGER,\n \
\    sh_save_pct REAL,\n \
\    ev_saves INTEGER,\n \
\    ev_shots INTEGER,\n \
\    ev_save_pct REAL,\n \
\    last_update TEXT NOT NULL,\n \
\    PRIMARY KEY (player_id, team_id, season_id),\n \
\    FOREIGN KEY (player_id)\n \
\        REFERENCES player (player_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION,\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createHomeTable :: Connection -> IO ()
createHomeTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS home_record (\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    wins INTEGER,\n \
\    losses INTEGER,\n \
\    otl INTEGER,\n \
\    PRIMARY KEY (team_id, season_id),\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createAwayTable :: Connection -> IO ()
createAwayTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS away_record (\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    wins INTEGER,\n \
\    losses INTEGER,\n \
\    otl INTEGER,\n \
\    PRIMARY KEY (team_id, season_id),\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createShootoutTable :: Connection -> IO ()
createShootoutTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS shootout_record (\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    wins INTEGER,\n \
\    losses INTEGER,\n \
\    otl INTEGER,\n \
\    PRIMARY KEY (team_id, season_id),\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createLastTenTable :: Connection -> IO ()
createLastTenTable conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS last_ten (\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    wins INTEGER,\n \
\    losses INTEGER,\n \
\    otl INTEGER,\n \
\    PRIMARY KEY (team_id, season_id),\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()


createBaseStandings :: Connection -> IO ()
createBaseStandings conn = execute
    conn
    "CREATE TABLE IF NOT EXISTS base_standings (\n \
\    team_id INTEGER,\n \
\    season_id INTEGER,\n \
\    reg_ot_wins INTEGER,\n \
\    FOREIGN KEY (team_id)\n \
\        REFERENCES team (team_id)\n \
\        ON DELETE CASCADE\n \
\        ON UPDATE NO ACTION\n \
\);"
    ()

createStandingsView :: Connection -> IO ()
createStandingsView conn = execute
    conn
    "CREATE VIEW IF NOT EXISTS standings\n \
\AS\n \
\SELECT \n \
\    BS.team_id,\n \
\    BS.season_id,\n \
\    TSS.wins,\n \
\    TSS.losses,\n \
\    BS.reg_ot_wins,\n \
\    TSS.points,\n \
\    TSS.point_pct,\n \
\    TSS.goals_for_game,\n \
\    TSS.goals_against_game,\n \
\    TSS.pp_pct,\n \
\    TSS.pp_goals_for,\n \
\    TSS.pp_goals_against,\n \
\    TSS.pp_opp,\n \
\    TSS.pk_pct,\n \
\    TSS.shots_for_game,\n \
\    TSS.shots_against_game,\n \
\    TSS.faceoff_pct,\n \
\    TSS.shooting_pct,\n \
\    TSS.save_pct,\n \
\    HR.wins AS home_wins,\n \
\    HR.losses AS home_losses,\n \
\    HR.otl AS home_otl,\n \
\    AR.wins AS away_wins,\n \
\    AR.losses AS away_losses,\n \
\    AR.otl AS away_otl,\n \
\    SR.wins AS shootout_wins,\n \
\    SR.losses AS shootout_losses,\n \
\    LT.wins AS last_ten_wins,\n \
\    LT.losses AS last_ten_losses,\n \
\    LT.otl AS last_ten_otl\n \
\FROM base_standings BS, team_season_stats TSS, home_record HR,\n \
\    away_record AR, shootout_record SR, last_ten LT\n \
\WHERE\n \
\    TSS.team_id = BS.team_id AND TSS.season_id = BS.season_id AND\n \
\    HR.team_id = BS.team_id AND HR.season_id = BS.season_id AND\n \
\    AR.team_id = BS.team_id AND AR.season_id = BS.season_id AND\n \
\    SR.team_id = BS.team_id AND SR.season_id = BS.season_id AND\n \
\    LT.team_id = BS.team_id AND LT.season_id = BS.season_id;"
    ()

createAggSkaterView :: Connection -> IO ()
createAggSkaterView conn = execute
    conn
    "CREATE VIEW IF NOT EXISTS agg_skater_stats\n \
\AS\n \
\SELECT\n \
\    GROUP_CONCAT(T.abbreviation, '/'),\n \
\    SUM(games) AS games,\n \
\    SUM(toi) AS toi,\n \
\    SUM(goals) AS goals,\n \
\    SUM(assists) AS assists,\n \
\    SUM(points) AS points,\n \
\    SUM(pp_goals) AS pp_goals,\n \
\    SUM(pp_assists) AS pp_assists,\n \
\    SUM(pp_points) AS pp_points,\n \
\    SUM(pp_toi) AS pp_toi,\n \
\    SUM(sh_goals) AS sh_goals,\n \
\    SUM(sh_assists) AS sh_assists,\n \
\    SUM(sh_points) AS sh_points,\n \
\    SUM(sh_toi) AS sh_toi,\n \
\    SUM(ev_goals) AS ev_goals,\n \
\    SUM(ev_assists) AS ev_assists,\n \
\    SUM(ev_points) AS ev_points,\n \
\    SUM(ev_toi) AS ev_toi,\n \
\    SUM(gwg) AS gwg,\n \
\    SUM(shots) AS shots,\n \
\    SUM(hits) AS hits,\n \
\    SUM(pims) AS pims,\n \
\    SUM(blocked) AS blocked,\n \
\    SUM(plus_minus) AS plus_minus,\n \
\    SUM(faceoff_pct) / 2 AS faceoff_pct,      \n \
\    SUM(goals) / SUM(shots) AS shooting_pct,\n \
\    SUM(shifts) AS shifts\n \
\FROM skater_season_stats SSS, team T\n \
\WHERE T.team_id = SSS.team_id\n \
\GROUP BY player_id, season_id;"
    ()


createAggGoalieView :: Connection -> IO ()
createAggGoalieView conn = execute
    conn
    "CREATE VIEW IF NOT EXISTS agg_goalie_stats\n \
\AS\n \
\SELECT\n \
\    GROUP_CONCAT(T.abbreviation, '/'),\n \
\    SUM(games) AS games,\n \
\    SUM(games_started) AS games_started,\n \
\    SUM(wins) AS wins,\n \
\    SUM(losses) AS losses,\n \
\    SUM(ot_losses) AS ot_losses,\n \
\    SUM(ties) AS ties,\n \
\    SUM(shutouts) AS shutouts,\n \
\    SUM(saves) AS saves,\n \
\    SUM(saves) / SUM(shots_against) AS save_pct,\n \
\    (SUM(goals_against) * 60) / SUM(toi) AS gaa,\n \
\    SUM(shots_against) AS shots_against,\n \
\    SUM(goals_against) AS goals_against,\n \
\    SUM(pp_saves) AS pp_saves,\n \
\    SUM(pp_shots) AS pp_shots,\n \
\    SUM(pp_saves) / SUM(pp_shots) AS pp_save_pct,\n \
\    SUM(sh_saves) AS sh_saves,\n \
\    SUM(sh_shots) AS sh_shots,\n \
\    SUM(sh_saves) / SUM(sh_shots) AS sh_save_pct,\n \
\    SUM(ev_saves) AS ev_saves,\n \
\    SUM(ev_shots) AS ev_shots,\n \
\    SUM(ev_saves) / SUM(ev_shots) AS ev_save_pct\n \
\FROM goalie_season_stats GSS, team T\n \
\WHERE T.team_id = GSS.team_id\n \
\GROUP BY player_id, season_id;"
    ()
