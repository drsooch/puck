module Puck.Types.Team
    ( TeamID(..)
    , FranchiseID(..)
    , DivisionID(..)
    , ConferenceID(..)
    , TeamInfo(..)
    , TeamStats(..)
    , TeamGameStats(..)
    , Record(..)
    , RecordSplits(..)
    , TeamSeasonsStats(..)
    )
where

import           Data.Generics.Labels
import           Data.Text                      ( Text )
import           GHC.Generics

import           Puck.Types.Common              ( SeasonID )

newtype TeamID = TeamID { getTeamID :: Int } deriving (Generic, Eq, Show)
newtype FranchiseID = FranchiseID { getFranchiseID :: Int } deriving (Generic, Eq, Show)

-- FIXME: Covid Rules have us using new divisions and Conferences
-- NHL Division
newtype DivisionID = DivisionID { getDivisionID :: Int } deriving (Generic, Eq)

instance Show DivisionID where
    show DivisionID {..} = case getDivisionID of
        25 -> "East"
        26 -> "Central"
        27 -> "West"
        28 -> "North"
        x  -> error $ "No division with id: " <> show x

-- NHL Conference
newtype ConferenceID = ConferenceID { getConferenceID :: Int } deriving (Generic, Eq)

instance Show ConferenceID where
    show ConferenceID {..} = case getConferenceID of
        0 -> "Unknown"  -- Covid season has an unamed Conference
        5 -> "Western"
        6 -> "Eastern"
        x -> error $ "No conference with id: " <> show x

-- Basic TeamInfo, when dealing with leagues outside of NHL certain fields are
-- not applicable
data TeamInfo = TeamInfo { teamID     :: TeamID
                         , name       :: Text
                         , abbrev     :: Text
                         , division   :: Maybe DivisionID
                         , conference :: Maybe ConferenceID
                         , active     :: Bool
                         , franID     :: Maybe FranchiseID
                         } deriving (Generic, Eq, Show)

data TeamStats = HGStats TeamGameStats -- Home
               | AGStats TeamGameStats -- Away
               | TSStats               -- Season
               deriving (Generic, Eq, Show)

data TeamGameStats = TeamGameStats { --players :: Roster
                                   {-,-} goals :: Int
                                   , shots     :: Int
                                   , pims      :: Int
                                   , ppPct     :: Double
                                   , ppGoals   :: Int
                                   , ppOpp     :: Int
                                   , foPct     :: Double
                                   , blocked   :: Int
                                   , takeaways :: Int
                                   , giveaways :: Int
                                   , hits      :: Int
                                   } deriving (Generic, Eq, Show)

data Record = Record { wins   :: Int
                     , losses :: Int
                     , otl    :: Int
                     } deriving (Generic, Eq, Show)

data RecordSplits = RecordSplits { homeRecord     :: Record
                                 , awayRecord     :: Record
                                 , shootoutRecord :: Record
                                 , lastTen        :: Record
                                 } deriving (Generic, Eq, Show)

-- TODO: Maybe scrub some stats we don't really need?
-- add roster for the season?
data TeamSeasonsStats = TeamSeasonsStats { season         :: SeasonID
                                         , gamesPlayed    :: Int
                                         , splits         :: RecordSplits
                                         , record         :: Record
                                         , points         :: Int
                                         , pointPct       :: Double
                                         , goalsForPG     :: Double
                                         , goalsAgainstPG :: Double
                                         , ppPct          :: Double
                                         , ppGF           :: Int
                                         , ppGA           :: Int
                                         , ppOpp          :: Int
                                         , pkPct          :: Double
                                         , shotsForPG     :: Double
                                         , shotsAgainstPG :: Double
                                         , faceOffWinPct  :: Double
                                         , shootingPct    :: Double
                                         , savePct        :: Double
                                         } deriving (Generic, Eq, Show)
