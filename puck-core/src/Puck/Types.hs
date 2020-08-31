module Puck.Types () where

import           Data.Text     (Text)
import           Puck.Database
import           Puck.Game
import           Puck.Player
import           Puck.Team


-- Specific game stats
data TeamGameStats = TeamGameStats { _teamInfo :: TeamInfo
                                   , _players  :: Roster
                                   --, _teamStats :: Comes from JSON Parsing
                                   } deriving (Eq, Show)

-- Data mostly derived from API requests
data Game = Game { _gameID   :: GameID
                 , _home     :: TeamGameStats
                 , _away     :: TeamGameStats
                 , _period   :: Period
                 , _gameDate :: GameDate
                 , _gameTime :: GameTime
                 } deriving (Eq, Show)
