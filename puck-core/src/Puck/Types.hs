module Puck.Types
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
    -- Team Types
    , TeamID(..)
    , FranchiseID(..)
    , DivisionID(..)
    , ConferenceID(..)
    , TeamInfo(..)
    , TeamStats(..)
    , TeamGameStats(..)
    , Record(..)
    , OverallRecord(..)
    , TeamSeasonsStats(..)
    -- Player Types
    , PlayerID(..)
    , PName(..)
    , Position(..)
    , ForwardType(..)
    , Handedness(..)
    , PlayerInfo(..)
    , Player(..)
    , Roster(..)
    , SkaterSeason(..)
    , GoalieSeason(..)
    -- Other Types
    , Schedule(..)
    , GamesList(..)
    )
where

import           Data.Time                      ( UTCTime
                                                , Day
                                                )
import           Data.Generics.Labels
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           GHC.Generics

{------------------------------ Game ------------------------------}
newtype GameID = GameID {getGameID :: Int} deriving (Generic, Eq, Show)

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

data CurrentPeriod = CurrentPeriod {period :: Period
                                   , gameTime :: GameTime
                                   } deriving (Generic, Eq, Show)

data TeamPeriodStats = TeamPeriodStats { shots :: Int
                                       , goals :: Int
                                       } deriving (Generic, Eq, Show)

data PeriodStats = PeriodStats { away :: TeamPeriodStats
                               , home :: TeamPeriodStats
                               } deriving (Generic, Eq, Show)

type GamePeriods = Map Period PeriodStats

-- Data mostly derived from API requests
data GamePreview = GamePreview { gameID   :: GameID
                               , home     :: TeamSeasonsStats
                               , away     :: TeamSeasonsStats
                               , gameDate :: GameDate
                               } deriving (Generic, Eq, Show)

data GameLive = GameLive { gameID          :: GameID
                         , home            :: TeamStats
                         , away            :: TeamStats
                         , currentPeriod   :: CurrentPeriod
                         , periodStats     :: GamePeriods
                         , gameDate        :: GameDate
                         } deriving (Generic, Eq, Show)

data GameFinal = GameFinal { gameID      :: GameID
                           , home        :: TeamStats
                           , away        :: TeamStats
                           , periodStats :: GamePeriods
                           , gameDate    :: GameDate
                           } deriving (Generic, Eq, Show)

-- FIXME: not sure how this is handled in the api
data GamePostponed = GamePostponed


{------------------------------ Team ------------------------------}
newtype TeamID = TeamID { getTeamID :: Int } deriving (Generic, Eq, Show)
newtype FranchiseID = FranchiseID { getFranchiseID :: Int } deriving (Generic, Eq, Show)

-- FIXME: Covid Rules have us using new divisions and Conferences
-- NHL Division
data DivisionID = DivisionID { getDivisionID :: Int } deriving (Generic, Eq)

-- NHL Conference
data ConferenceID = ConferenceID { getConferenceID :: Int } deriving (Generic, Eq)


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
                                   , shots :: Int
                                   , pims :: Int
                                   , ppPct :: Double
                                   , ppGoals :: Int
                                   , ppOpp :: Int
                                   , foPct :: Double
                                   , blocked :: Int
                                   , takeaways :: Int
                                   , giveaways :: Int
                                   , hits :: Int
                                   } deriving (Generic, Eq, Show)

data Record = Record { wins   :: Int
                     , losses :: Int
                     , otl    :: Int
                     } deriving (Generic, Eq, Show)

data OverallRecord = OverallRecord { homeRecord     :: Record
                                   , awayRecord     :: Record
                                   , shootoutRecord :: Record
                                   , lastTen        :: Record
                                   } deriving (Generic, Eq, Show)

-- TODO: Maybe scrub some stats we don't really need?
-- add roster for the season?
data TeamSeasonsStats = TeamSeasonsStats { gamesPlayed :: Int
                                         , overallRecord :: OverallRecord
                                         , wins :: Int
                                         , losses :: Int
                                         , otl :: Int
                                         , points :: Int
                                         , pointPct :: Double
                                         , goalsForPG :: Double
                                         , goalsAgainstPG :: Double
                                         , ppPct :: Double
                                         , ppGF :: Int
                                         , ppGA :: Int
                                         , ppOpp :: Int
                                         , pkPct :: Double
                                         , shotsForPG :: Double
                                         , shotsAgainstPG :: Double
                                         , faceOffWinPct :: Double
                                         , shootingPct :: Double
                                         , savePct :: Double
                                         } deriving (Generic, Eq, Show)

{----------------------------- Player -----------------------------}
-- FIXME: TypeClass encompassing Skater and Goalie
class Updateable a where
    update :: a -> a

-- Probably could just be an alias
newtype PlayerID = PlayerID { getPlayerID :: Int } deriving (Generic, Eq, Show)

-- player name
data PName = PName { firstName :: Text
                   , lastName  :: Text
                   , fullName  :: Text
                   } deriving (Generic, Eq)

data Position = Forward ForwardType
              | Defenseman
              | Goalie
              deriving (Generic, Eq)

data ForwardType = C
                 | LW
                 | RW
                 deriving (Generic, Eq)

-- may need better constructor names
data Handedness = LeftH
                | RightH
                deriving (Generic, Eq)

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

{---------------------------- Schedule ----------------------------}

data Schedule = Schedule { startDate :: Day
                         , endDate   :: Maybe Day
                         , games     :: GamesList
                         , numGames  :: Int
                         } deriving (Generic, Eq, Show)

newtype GamesList = GamesList { gamesList :: Map Day [Game] }
                 deriving (Generic, Eq, Show)

{-------------------------- Instance Show -------------------------}
instance Show Period where
    show First           = "1st"
    show Second          = "2nd"
    show Third           = "3rd"
    show (OT (Just num)) = show num <> "OT"
    show (OT Nothing   ) = "OT"
    show Shootout        = "Shootout"
    show Final           = "Final"

-- subject to change
instance Show PName where
    show = show . fullName

-- subject to change
instance Show Position where
    show (Forward ft) = show ft
    show Defenseman   = "Defenseman"
    show Goalie       = "Goalie"

instance Show ForwardType where
    show C  = "Center"
    show LW = "Left Wing"
    show RW = "Right Wing"

instance Show Handedness where
    show LeftH  = "Left"
    show RightH = "Right"

instance Show DivisionID where
    show DivisionID {..} = case getDivisionID of
        25 -> "East"
        26 -> "Central"
        27 -> "West"
        28 -> "North"
        x  -> error $ "No division with id: " <> show x

instance Show ConferenceID where
    show ConferenceID {..} = case getConferenceID of
        5 -> "Western"
        6 -> "Eastern"
        x -> error $ "No conference with id: " <> show x
