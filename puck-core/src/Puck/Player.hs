{-# LANGUAGE DuplicateRecordFields #-}
module Puck.Player (
    -- typeclasses
    IsPlayer,
    -- type
    PlayerID(..),
    Position(..),
    Handedness(..),
    PlayerInfo(..),
    Skater(..),
    SkaterStats(..),
    Goalie(..),
    GoalieStats(..),
    Roster(..),
) where

import           Data.Text (Text)

-- FIXME: TypeClass encompassing Skater and Goalie
class Updateable a where
    update :: a -> a

-- Probably could just be an alias
newtype PlayerID = PlayerID Int deriving (Eq, Show)

-- player name
data PName = PName { _first :: Text
                   , _last  :: Text
                   , _full  :: Text
                   } deriving (Eq)

-- subject to change
instance Show PName where
    show = show . full

data Position = C
              | LW
              | RW
              | D
              | G
              deriving (Eq)

-- subject to change
instance Show Position where
    show C  = "Center"
    show LW = "Left Wing"
    show RW = "Right Wing"
    show D  = "Defenseman"
    show G  = "Goalie"

-- may need better constructor names
data Handedness = L
                | R
                deriving (Eq)

instance Show Handedness where
    show L = "Left"
    show R = "Right"

-- Generic Player Info
data PlayerInfo = PlayerInfo { _playerID :: PlayerID
                             , _teamID   :: Int     -- Circular import
                             , _name     :: Name    -- Does not match database directly
                             , _number   :: Int
                             , _position :: Position
                             , _hand     :: Handedness
                             , _rookie   :: Bool
                             , _age      :: Int
                             } deriving (Eq, Show)


-- Possibly alter to encode what type of stats the Skater carries
data Skater = Skater { _playerInfo :: PlayerInfo
                     , _stats      :: SkaterStats
                     } deriving (Eq, Show)

-- These will be expanded to whatever the DB query returns
data SkaterStats = SSStats -- Skater Season Stats
                 | SCStats -- Skater Career Stats  (essentially just a list of season stats)
                 | SGStats -- Skater Game Stats
                 deriving (Eq, Show)

-- see Skater
data Goalie = Goalie { _playerInfo :: PlayerInfo
                     , _stats      :: GoalieStats
                     } deriving (Eq, Show)

-- See skater
data GoalieStats = GSStats -- Goalie Season Stats
                 | GCStats -- Goalie Career Stats  (essentially just a list of season stats)
                 | GGStats -- Goalie Game Stats
                 deriving (Eq, Show)

-- Should be paired with a Team to encapsulate a roster
data Roster = Roster { _skaters :: [Skater]
                     , _goalies :: [Goalie]
                     } deriving (Eq, Show)
