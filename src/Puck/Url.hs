module Puck.Url where

import           Puck.Types

allTeamsURL :: URL
allTeamsURL = URL "https://statsapi.web.nhl.com/api/v1/teams"

teamURL :: TeamID -> URL
teamURL (TeamID tid) =
    URL $ "https://statsapi.web.nhl.com/api/v1/teams/" <> show tid

teamRosterURL :: TeamID -> URL
teamRosterURL (TeamID tid) =
    URL $ "https://statsapi.web.nhl.com/api/v1/teams/" <> show tid <> "/roster"

scheduleURL :: URL
scheduleURL = URL "https://statsapi.web.nhl.com/api/v1/schedule"

gameURL :: GameID -> URL
gameURL (GameID gid) =
    URL
        $  "https://statsapi.web.nhl.com/api/v1/game/"
        <> show gid
        <> "/feed/live"

standingsURL :: URL
standingsURL = URL "https://statsapi.web.nhl.com/api/v1/standings"

playerURL :: PlayerID -> URL
playerURL (PlayerID pid) =
    URL $ "https://statsapi.web.nhl.com/api/v1/people/" <> show pid

playerStatsURL :: PlayerID -> SeasonID -> URL
playerStatsURL (PlayerID pid) (SeasonID sid) =
    URL
        $  "https://statsapi.web.nhl.com/api/v1/people/"
        <> show pid
        <> "/stats?stats=statsSingleSeason&season="
        <> show sid

playerStatsAllURL :: PlayerID -> URL
playerStatsAllURL (PlayerID pid) =
    URL
        $  "https://statsapi.web.nhl.com/api/v1/people/"
        <> show pid
        <> "/stats?stats=yearByYear"

currentSeasonURL :: URL
currentSeasonURL = URL "https://statsapi.web.nhl.com/api/v1/season/current"

