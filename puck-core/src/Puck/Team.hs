{-# LANGUAGE DuplicateRecordFields #-}
module Puck.Team (
    -- types
    Division(..),
    Conference(..),
    TeamID(..),
    FranchiseID(..),
    Record(..),
    TeamInfo(..),
    TeamStats(..),
) where

import           Data.Text (Text)

-- NHL Division
data Division = Atlantic
              | Metro
              | Central
              | Pacific
              deriving (Eq, Show)

-- NHL Conference
data Conference = Eastern
                | Western
                deriving (Eq, Show)

-- May not need to be newtyped, but for now
newtype TeamID = TeamID Int deriving (Eq, Show)
newtype FranchiseID = FranchiseID Int deriving (Eq, Show)

-- Basic TeamInfo, when dealing with leagues outside of NHL certain fields are
-- not applicable
data TeamInfo = TeamInfo { _teamID     :: TeamID
                         , _name       :: Text
                         , _abbrev     :: Maybe Text
                         , _division   :: Maybe Division
                         , _conference :: Maybe Conference
                         , _active     :: Maybe Bool
                         , _franID     :: Maybe FranchiseID
                         , _leagueID   :: LeagueID
                         } deriving (Eq, Show)

-- Team Stats
data TeamStats = TeamStats { _teamInfo :: TeamInfo
                           , _teamData :: TeamSeasonsData
                           } deriving (Eq, Show)

data TeamSeasonsData -- This will come from the Database, approximately 15 columns of data

