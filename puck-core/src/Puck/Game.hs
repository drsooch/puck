module Puck.Game (
    -- types and constructors
    GameID,
    GameDate,
    GameTime,
    Period
) where

import           Data.Text (Text)

-- May make this a type alias
newtype GameID = GameID Int deriving (Eq, Show)

-- May be moved
data GameDate = GameDate { _startTime :: Text
                         , _date      :: Text
                         } deriving (Eq, Show)

-- Time in period
newtype GameTime = GameTime Text deriving (Eq, Show)

data GameStatus = Preview
                | PreGame
                | Live
                | Finished
                deriving (Eq, Show)

data Period = First
            | Second
            | Third
            | OT
            | Shootout
            | Final
            deriving (Eq)

instance Show Period where
    show First    = "1st"
    show Second   = "2nd"
    show Third    = "3rd"
    show OT       = "OT"
    show Shootout = "Shootout"
    show Final    = "Final"


