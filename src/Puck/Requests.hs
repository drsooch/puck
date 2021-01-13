module Puck.Requests
    () where


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
import           Puck.Types.Exception
import           Puck.Url


type RequestResult a = IO (Either PuckException a)

requestParseURL :: (Value -> Parser b) -> URL -> Manager -> RequestResult b
requestParseURL p (URL url) manager =
    parseEither p . checkStatus <$> (parseRequest url >>= flip httpLbs manager)

requestURL :: URL -> Manager -> RequestResult ByteString
requestURL (URL url) manager =
    checkStatus <$> (parseRequest url >>= flip httpLbs manager)

 -- FIXME: only response we accept is 200
checkStatus :: Response a -> Either PuckException a
checkStatus resp
    | getResponseStatus resp == status200 = Right $ getResponseBody resp
    | otherwise = Left $ ConnError $ getResponseStatus resp

requestAllTeams :: Manager -> RequestResult [TeamInfoDB]
requestAllTeams = requestParseURL pAllNHLTeams allTeamsURL

requestTeamSeasonStats :: TeamID -> Manager -> RequestResult TeamSeasonsStatsDB
requestTeamSeasonStats tid = requestParseURL pTeamSeasonStats (teamURL tid)

requestPlayerInfo :: PlayerID -> Manager -> RequestResult PlayerInfo
requestPlayerInfo pid = requestParseURL pPlayerInfo (playerURL pid)

{-requestPlayerStats :: PlayerID -> SeasonID -> Manager -> RequestResult -}

requestCurrentSeason :: Manager -> RequestResult SeasonID
requestCurrentSeason = requestParseURL pCurrentSeason currentSeasonURL

requestGameLive :: GameID -> Manager -> RequestResult GameLive
requestGameLive gid = requestParseURL pGameLive (gameURL gid)

requestGameFinal :: GameID -> Manager -> RequestResult GameFinal
requestGameFinal gid = requestParseURL pGameFinal (gameURL gid)

