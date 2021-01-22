module Puck.Requests
    ( requestAllTeams
    , requestTeamRoster
    , requestPlayerInfo
    , requestPlayerStats
    , requestSkaterStats
    , requestGoalieStats
    , requestCareerStats
    , requestCurrentSeason
    , requestTeamInfo
    -- Base Functions
    , requestURL
    ) where


import           Control.Exception              ( throw )
import           Data.Aeson.Types               ( Parser
                                                , Value
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Network.HTTP.Client            ( Manager
                                                , httpLbs
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Simple     hiding ( httpLbs )
import           Network.HTTP.Types.Status      ( status200
                                                , statusCode
                                                )

import           Puck.Database.Types
import           Puck.Parser
import           Puck.Types
import           Puck.Types.Error
import           Puck.Url



requestParseURL :: (Value -> Parser b) -> URL -> Manager -> IO (Maybe b)
requestParseURL p (URL url) manager =
    (parseRequest url >>= flip httpLbs manager) >>= parseMaybe p . checkStatus

requestURL :: URL -> Manager -> IO (Maybe ByteString)
requestURL (URL url) manager =
    checkStatus <$> (parseRequest url >>= flip httpLbs manager)

 -- FIXME: only response we accept is 200
checkStatus :: Response a -> Maybe a
checkStatus resp
    | getResponseStatus resp == status200 = Just $ getResponseBody resp
    | otherwise                           = Nothing

requestAllTeams :: Manager -> IO (Maybe [TeamInfoDB])
requestAllTeams = requestParseURL pAllNHLTeams allTeamsURL

requestTeamRoster :: TeamID -> Manager -> IO (Maybe [PlayerID])
requestTeamRoster tid = requestParseURL pTeamRoster (teamRosterURL tid)

requestTeamInfo :: TeamID -> Manager -> IO (Maybe TeamInfoDB)
requestTeamInfo tid = requestParseURL pTeamInfo (teamURL tid)

requestTeamSeasonStats :: TeamID -> Manager -> IO (Maybe TeamSeasonsStatsDB)
requestTeamSeasonStats tid = requestParseURL pTeamSeasonStats (teamURL tid)

requestPlayerInfo :: PlayerID -> Manager -> IO (Maybe PlayerInfoDB)
requestPlayerInfo pid = requestParseURL pPlayerInfo (playerURL pid)

{-requestSkaterStats-}
    {-:: PlayerID -> SeasonID -> TeamID -> Manager -> IO (Maybe SkaterSeasonDB) -}
{-requestSkaterStats pid sid tid = requestParseURL-}
    {-(\v -> pSkaterSeasonStats v pid sid tid)   -- we have to do this...-}
    {-(playerStatsURL pid sid)-}
requestPlayerStats
    :: PlayerID -> SeasonID -> TeamID -> Manager -> IO (Maybe PlayerStatsDB)
requestPlayerStats pid sid tid =
    requestParseURL (pPlayerSeasonStats pid sid tid) (playerStatsURL pid sid)

requestSkaterStats
    :: PlayerID -> SeasonID -> TeamID -> Manager -> IO (Maybe SkaterSeasonDB)
requestSkaterStats pid sid tid =
    requestParseURL (pSkaterSeasonStats pid sid tid) (playerStatsURL pid sid)

requestGoalieStats
    :: PlayerID -> SeasonID -> TeamID -> Manager -> IO (Maybe GoalieSeasonDB)
requestGoalieStats pid sid tid =
    requestParseURL (pGoalieSeasonStats pid sid tid) (playerStatsURL pid sid)

requestCareerStats :: PlayerID -> Manager -> IO (Maybe CareerStatsDB)
requestCareerStats pid =
    requestParseURL (pCareerStats pid) (playerStatsAllURL pid)

requestCurrentSeason :: Manager -> IO (Maybe SeasonID)
requestCurrentSeason = requestParseURL pCurrentSeason currentSeasonURL

requestGameLive :: GameID -> Manager -> IO (Maybe GameLive)
requestGameLive gid = requestParseURL pGameLive (gameURL gid)

requestGameFinal :: GameID -> Manager -> IO (Maybe GameFinal)
requestGameFinal gid = requestParseURL pGameFinal (gameURL gid)


