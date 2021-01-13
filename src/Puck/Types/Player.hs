module Puck.Types.Player
    (
    -- Player Types
      PlayerID(..)
    , PName(..)
    , Position(..)
    , ForwardType(..)
    , Handedness(..)
    , PlayerInfo(..)
    , Player(..)
    , Roster(..)
    , SkaterSeason(..)
    , GoalieSeason(..)
    )
where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Generics.Labels
import           GHC.Generics

import           Puck.Types.Team                ( TeamID )

-- Probably could just be an alias
newtype PlayerID = PlayerID { getPlayerID :: Int } deriving (Generic, Eq, Show)

-- player name
data PName = PName { firstName :: Text
                   , lastName  :: Text
                   , fullName  :: Text
                   } deriving (Generic, Eq)

-- subject to change
instance Show PName where
    show = show . fullName

data Position = Forward ForwardType
              | Defenseman
              | Goalie
              deriving (Generic, Eq)

-- subject to change
instance Show Position where
    show (Forward ft) = show ft
    show Defenseman   = "Defenseman"
    show Goalie       = "Goalie"

data ForwardType = C
                 | LW
                 | RW
                 deriving (Generic, Eq)

instance Show ForwardType where
    show C  = "Center"
    show LW = "Left Wing"
    show RW = "Right Wing"

-- may need better constructor names
data Handedness = LeftH
                | RightH
                deriving (Generic, Eq)

instance Show Handedness where
    show LeftH  = "Left"
    show RightH = "Right"

-- Generic Player Info
data PlayerInfo = PlayerInfo { playerID :: PlayerID
                             , teamID   :: TeamID
                             , name     :: PName
                             , number   :: Maybe Int
                             , position :: Position
                             , hand     :: Handedness
                             , rookie   :: Bool
                             , age      :: Int
                             } deriving (Generic, Eq, Show)

-- Possibly alter to encode what type of stats the Skater carries
data Player = Player { playerInfo :: PlayerInfo
                     , stats      :: PlayerStats
                     } deriving (Generic, Eq, Show)

-- These will be expanded to whatever the DB query returns
data PlayerStats = SSeasonStats -- Skater Season Stats
                 | SCareerStats -- Skater Career Stats
                 | SGameStats   -- Skater Game Stats
                 | GSeasonStats -- Goalie Season Stats
                 | GCareerStats -- Goalie Career Stats
                 | GGameStats   -- Goalie Game Stats
                 deriving (Generic, Eq, Show)

data SkaterSeason = SkaterSeason { team        :: Text -- this will hold a string delineating all the Teams the skater has played for.
                                 , games       :: Int
                                 , toi         :: Double
                                 , goals       :: Int
                                 , assists     :: Int
                                 , point       :: Int
                                 , ppGoals     :: Int
                                 , ppAssists   :: Int
                                 , ppPoints    :: Int
                                 , ppToi       :: Double
                                 , shGoals     :: Int
                                 , shAssists   :: Int
                                 , shPoints    :: Int
                                 , shToi       :: Int
                                 , evGoals     :: Int
                                 , evAssists   :: Int
                                 , evPoints    :: Int
                                 , evToi       :: Int
                                 , gwg         :: Int
                                 , shots       :: Int
                                 , hits        :: Int
                                 , pims        :: Int
                                 , blocked     :: Int
                                 , plusMinus   :: Int
                                 , faceOffPct  :: Double
                                 , shootingPct :: Double
                                 , shifts      :: Int
                                 } deriving (Generic, Eq, Show)

data GoalieSeason = GoalieSeason { team         :: Text
                                 , games        :: Int
                                 , gamesStarted :: Int
                                 , toi          :: Double
                                 , wins         :: Int
                                 , losses       :: Int
                                 , ties         :: Int
                                 , shutouts     :: Int
                                 , saves        :: Int
                                 , savePct      :: Double
                                 , gaa          :: Double
                                 , shotsAgainst :: Int
                                 , goalsAgainst :: Int
                                 , ppSaves      :: Int
                                 , ppShots      :: Int
                                 , ppSavePct    :: Double
                                 , shSaves      :: Int
                                 , shShots      :: Int
                                 , shSavePct    :: Double
                                 , evShots      :: Int
                                 , evSaves      :: Int
                                 , evSavePct    :: Double
                                 } deriving (Generic, Eq, Show)

-- Should be paired with a Team to encapsulate a roster
newtype Roster = Roster { players :: [Player]} deriving (Generic, Eq, Show)

