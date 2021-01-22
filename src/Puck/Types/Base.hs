module Puck.Types.Base
    ( PuckConfig(..)
    , PuckState
    , PuckLog
    , Display
    , ResourceName
    , PuckApp
    ) where

import           Control.Monad.RWS.Strict       ( RWST )

import           Brick.Main                     ( App )
import           Database.SQLite.Simple         ( Connection )
import           Network.HTTP.Client            ( Manager )

import           Puck.Types.Common              ( SeasonID )

{------------------------------ Main Application ------------------------------}

-- We need to feed this into a Reader mechanism
-- ReaderT? StateT? combination?
data PuckConfig = PuckConfig
    { dbConn        :: Connection
    , tlsManager    :: Manager
    , currentSeason :: SeasonID
    }

data PuckState = PuckState
    deriving (Eq, Show)

-- writer portion of PuckApp, needs to be replaced by proper Logger
type PuckLog = [String]

-- Display is the Brick App plus the state of the application
-- ResourceName names widgets and may end up changing
type Display e = App PuckState e ResourceName
type ResourceName = String

-- the actual application, runs on top of the IO monad so we can contact
-- database and the NHL api
type PuckApp e r = RWST PuckConfig PuckLog (Display e) IO r

