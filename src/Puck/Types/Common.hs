module Puck.Types.Common
    ( URL(..)
    , SeasonID(..)
    , LeagueID(..)
    ) where

import           Data.Generics.Labels
import           GHC.Generics

newtype URL = URL { getURL :: String } deriving (Generic, Eq, Show)

newtype SeasonID = SeasonID { getSeasonID :: Integer } deriving (Generic, Eq, Show)

newtype LeagueID = LeagueID { getLeagueID :: Int } deriving (Generic, Eq, Show)
