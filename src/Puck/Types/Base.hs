module Puck.Types.Base
    ( Config(..)
    ) where

import           Database.SQLite.Simple         ( Connection )
import           Network.HTTP.Client            ( Manager )

import           Puck.Types.Common              ( SeasonID )

{------------------------------ Main Application ------------------------------}

-- We need to feed this into a Reader mechanism
-- ReaderT? StateT? combination?
data Config = Config
    { dbConn        :: Connection
    , tlsManager    :: Manager
    , currentSeason :: SeasonID
    }





{---------------------------- Schedule ----------------------------}

--data Schedule = Schedule { startDate :: Day
                         --, endDate   :: Maybe Day
                         --, games     :: GamesList
                         --, numGames  :: Int
                         --} deriving (Generic, Eq, Show)

--newtype GamesList = GamesList { gamesList :: Map Day [Game] }
                 --deriving (Generic, Eq, Show)


