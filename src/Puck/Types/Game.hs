module Puck.Types.Game
    (
    -- Game Types
      GameID(..)
    , GameTime(..)
    , GameDate(..)
    , Period(..)
    , CurrentPeriod(..)
    , PeriodStats(..)
    , TeamPeriodStats(..)
    , GamePeriods
    , Game(..)
    , GamePreview(..)
    , GameLive(..)
    , GameFinal(..)
    ) where


import           Data.Generics.Labels
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           GHC.Generics

import           Puck.Types.Team                ( TeamID
                                                , TeamSeasonsStats
                                                , TeamStats
                                                )

newtype GameID = GameID {getGameID :: Integer} deriving (Generic, Eq, Show)

-- Time in period
newtype GameTime = GameTime {getGameTime :: Text} deriving (Generic, Eq, Show)

newtype GameDate = GameDate {getStartTime :: UTCTime} deriving (Generic, Eq, Show)

data Game = Preview GamePreview
          | PreGame GamePreview
          | Live GameLive
          | Finished GameFinal
          deriving (Generic, Eq, Show)

data Period = First
            | Second
            | Third
            | OT (Maybe Int)  -- Number OT (playoffs)
            | Shootout
            | Final
            deriving (Generic, Eq, Ord)

instance Show Period where
    show First           = "1st"
    show Second          = "2nd"
    show Third           = "3rd"
    show (OT (Just num)) = show num <> "OT"
    show (OT Nothing   ) = "OT"
    show Shootout        = "Shootout"
    show Final           = "Final"

data CurrentPeriod = CurrentPeriod
    { period   :: Period
    , gameTime :: GameTime
    }
    deriving (Generic, Eq, Show)

data TeamPeriodStats = TeamPeriodStats
    { shots :: Int
    , goals :: Int
    }
    deriving (Generic, Eq, Show)

data PeriodStats = PeriodStats
    { away :: TeamPeriodStats
    , home :: TeamPeriodStats
    }
    deriving (Generic, Eq, Show)

type GamePeriods = Map Period PeriodStats

-- Data mostly derived from API requests
data GamePreview = GamePreview
    { gameID   :: GameID
    , home     :: TeamSeasonsStats
    , away     :: TeamSeasonsStats
    , gameDate :: GameDate
    }
    deriving (Generic, Eq, Show)

data GameLive = GameLive
    { gameID        :: GameID
    , home          :: TeamStats
    , away          :: TeamStats
    , currentPeriod :: CurrentPeriod
    , periodStats   :: GamePeriods
    , gameDate      :: GameDate
    }
    deriving (Generic, Eq, Show)

data GameFinal = GameFinal
    { gameID      :: GameID
    , home        :: TeamStats
    , away        :: TeamStats
    , periodStats :: GamePeriods
    , gameDate    :: GameDate
    }
    deriving (Generic, Eq, Show)

-- FIXME: not sure how this is handled in the api
data GamePostponed = GamePostponed
