module Puck.Database.Types
    (
    -- Team Types
      TeamInfoDB(..)
    , TeamSeasonsStatsDB(..)
    -- Player Types
    , PlayerInfoDB(..)
    , PlayerStatsDB(..)
    , SkaterSeasonDB(..)
    , GoalieSeasonDB(..)
    , CareerStatsDB
    , SingleSeasonCS
    -- Other Types
    , LeagueDB(..)
    , BaseStandingsDB(..)
    -- defaults
    , mkDefaultGoalieSeasonDB
    , mkDefaultSkaterSeasonDB
    , mkDefaultTeamInfoDBNoID
    -- private
    , toRowTINoID
    ) where

import           Data.Text                      ( Text
                                                , breakOn
                                                )
import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField

import           Puck.Types
import           Puck.Types.Error
import           Puck.Utils

{------------------------------ Misc ------------------------------}

data LeagueDB = LeagueDB
    { leagueID   :: LeagueID
    , leagueName :: Text
    }
    deriving (Generic, Eq, Show)

{------------------------------ Team ------------------------------}

data TeamInfoDB = TeamInfoDB
    { teamID     :: TeamID
    , name       :: Text
    , abbrev     :: Maybe Text
    , division   :: Maybe DivisionID
    , conference :: Maybe ConferenceID
    , franID     :: Maybe FranchiseID
    , leagueID   :: LeagueID
    }
    deriving (Generic, Eq, Show)

-- this is a shitty way to get a teamInfo record for two items
-- this should only be used in Populate.hs when we insert Career Stats.
-- Teams outside the NHL don't get proper information, we let the
-- database take care of creating an ID
mkDefaultTeamInfoDBNoID :: LeagueID -> Text -> TeamInfoDB
mkDefaultTeamInfoDBNoID lid name = TeamInfoDB { teamID     = TeamID (-1)
                                              , name       = name
                                              , abbrev     = Nothing
                                              , division   = Nothing
                                              , conference = Nothing
                                              , franID     = Nothing
                                              , leagueID   = lid
                                              }



data TeamSeasonsStatsDB = TeamSeasonsStatsDB
    { teamID         :: TeamID
    , season         :: SeasonID
    , splits         :: RecordSplits
    , gamesPlayed    :: Int
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
    }
    deriving (Generic, Eq, Show)

{----------------------------- Player -----------------------------}
-- Generic Player Info
data PlayerInfoDB = PlayerInfoDB
    { playerID :: PlayerID
    , teamID   :: TeamID
    , name     :: PName
    , number   :: Maybe Int
    , position :: Position
    , hand     :: Handedness
    , age      :: Int
    , rookie   :: Bool
    }
    deriving (Generic, Eq, Show)


-- Possibly alter to encode what type of stats the Skater carries
--data Player = Player { playerInfo :: PlayerInfo
                     --, stats      :: PlayerStats
                     --} deriving (Generic, Eq, Show)

data PlayerStatsDB = SSeasonStats SkaterSeasonDB-- Skater Season Stats
                   | SCareerStats -- Skater Career Stats
                   | SGameStats   -- Skater Game Stats
                   | GSeasonStats GoalieSeasonDB-- Goalie Season Stats
                   | GCareerStats -- Goalie Career Stats
                   | GGameStats   -- Goalie Game Stats
                   | EmptyStatPage
                   deriving (Generic, Eq, Show)

data SkaterSeasonDB = SkaterSeasonDB
    { playerID    :: PlayerID
    , teamID      :: TeamID
    , season      :: SeasonID
    , games       :: Int
    , toi         :: Double
    , goals       :: Int
    , assists     :: Int
    , points      :: Int
    , ppGoals     :: Int
    , ppAssists   :: Int
    , ppPoints    :: Int
    , ppToi       :: Double
    , shGoals     :: Int
    , shAssists   :: Int
    , shPoints    :: Int
    , shToi       :: Double
    , evGoals     :: Int
    , evAssists   :: Int
    , evPoints    :: Int
    , evToi       :: Double
    , gwg         :: Int
    , shots       :: Int
    , hits        :: Int
    , pims        :: Int
    , blocked     :: Int
    , plusMinus   :: Int
    , faceOffPct  :: Double
    , shootingPct :: Double
    , shifts      :: Int
    }
    deriving (Generic, Eq, Show)

data GoalieSeasonDB = GoalieSeasonDB
    { playerID     :: PlayerID
    , teamID       :: TeamID
    , season       :: SeasonID
    , games        :: Int
    , gamesStarted :: Int
    , toi          :: Double
    , record       :: Record
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
    }
    deriving (Generic, Eq, Show)


mkDefaultGoalieSeasonDB :: PlayerID -> SeasonID -> TeamID -> GoalieSeasonDB
mkDefaultGoalieSeasonDB pid sid tid = GoalieSeasonDB { playerID     = pid
                                                     , teamID       = tid
                                                     , season       = sid
                                                     , games        = 0
                                                     , gamesStarted = 0
                                                     , toi          = 0
                                                     , record = Record 0 0 0
                                                     , shutouts     = 0
                                                     , saves        = 0
                                                     , savePct      = 0
                                                     , gaa          = 0
                                                     , shotsAgainst = 0
                                                     , goalsAgainst = 0
                                                     , ppSaves      = 0
                                                     , ppShots      = 0
                                                     , ppSavePct    = 0
                                                     , shSaves      = 0
                                                     , shShots      = 0
                                                     , shSavePct    = 0
                                                     , evShots      = 0
                                                     , evSaves      = 0
                                                     , evSavePct    = 0
                                                     }

mkDefaultSkaterSeasonDB :: PlayerID -> SeasonID -> TeamID -> SkaterSeasonDB
mkDefaultSkaterSeasonDB pid sid tid = SkaterSeasonDB { playerID    = pid
                                                     , teamID      = tid
                                                     , season      = sid
                                                     , games       = 0
                                                     , toi         = 0
                                                     , goals       = 0
                                                     , assists     = 0
                                                     , points      = 0
                                                     , ppGoals     = 0
                                                     , ppAssists   = 0
                                                     , ppPoints    = 0
                                                     , ppToi       = 0
                                                     , shGoals     = 0
                                                     , shAssists   = 0
                                                     , shPoints    = 0
                                                     , shToi       = 0
                                                     , evGoals     = 0
                                                     , evAssists   = 0
                                                     , evPoints    = 0
                                                     , evToi       = 0
                                                     , gwg         = 0
                                                     , shots       = 0
                                                     , hits        = 0
                                                     , pims        = 0
                                                     , blocked     = 0
                                                     , plusMinus   = 0
                                                     , faceOffPct  = 0
                                                     , shootingPct = 0
                                                     , shifts      = 0
                                                     }

type CareerStatsDB = [SingleSeasonCS]
type SingleSeasonCS
    = (PlayerStatsDB, (Maybe TeamID, Text), (Maybe LeagueID, Text))

{------------------------------ Standings ------------------------------}

data BaseStandingsDB = BaseStandingsDB
    { teamID :: TeamID
    , season :: SeasonID
    , row    :: Int
    }
    deriving (Generic, Eq, Show)

{-------------------------- FromRow Instances --------------------------}

instance FromRow TeamInfo where
    fromRow =
        TeamInfo
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance FromRow PlayerInfo where
    fromRow =
        PlayerInfo
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

instance FromRow Position where
    fromRow = field

instance FromRow LeagueDB where
    fromRow = LeagueDB <$> field <*> field

{-------------------------- ToRow Instances --------------------------}

instance ToRow LeagueDB where
    toRow LeagueDB {..} = toRow (leagueID, leagueName)

instance ToRow PlayerInfoDB where
    toRow PlayerInfoDB {..} =
        toRow (playerID, teamID, name, number, position, hand, age, rookie)

toRowTINoID :: TeamInfoDB -> [SQLData]
toRowTINoID TeamInfoDB {..} =
    toRow (name, abbrev, division, conference, franID, leagueID)

instance ToRow TeamInfoDB where
    toRow TeamInfoDB {..} =
        toRow (teamID, name, abbrev, division, conference, franID, leagueID)

-- SQLite.Simple doesn't have an instance for this size tuple
-- can't map toField into large array either as they aren't the
-- same type
instance ToRow TeamSeasonsStatsDB where
    toRow TeamSeasonsStatsDB {..} =
        [toField teamID, toField season, toField gamesPlayed]
            <> toRow record
            <> [ toField points
               , toField pointPct
               , toField goalsForPG
               , toField goalsAgainstPG
               , toField ppPct
               , toField ppGF
               , toField ppGA
               , toField ppOpp
               , toField pkPct
               , toField shotsForPG
               , toField shotsAgainstPG
               , toField faceOffWinPct
               , toField shootingPct
               , toField savePct
               ]

instance ToRow SkaterSeasonDB where
    toRow SkaterSeasonDB {..} =
        [ toField playerID
        , toField teamID
        , toField season
        , toField games
        , toField $ doubleToToi toi
        , toField goals
        , toField assists
        , toField points
        , toField ppGoals
        , toField ppAssists
        , toField ppPoints
        , toField $ doubleToToi ppToi
        , toField shGoals
        , toField shAssists
        , toField shPoints
        , toField $ doubleToToi shToi
        , toField evGoals
        , toField evAssists
        , toField evPoints
        , toField $ doubleToToi evToi
        , toField gwg
        , toField shots
        , toField hits
        , toField pims
        , toField blocked
        , toField plusMinus
        , toField faceOffPct
        , toField shootingPct
        , toField shifts
        ]

instance ToRow GoalieSeasonDB where
    toRow GoalieSeasonDB {..} =
        [ toField playerID
            , toField teamID
            , toField season
            , toField games
            , toField gamesStarted
            , toField toi
            ]
            <> toRow record
            <> [ toField shutouts
               , toField saves
               , toField savePct
               , toField gaa
               , toField shotsAgainst
               , toField goalsAgainst
               , toField ppSaves
               , toField ppShots
               , toField ppSavePct
               , toField shSaves
               , toField shShots
               , toField shSavePct
               , toField evShots
               , toField evSaves
               , toField evSavePct
               ]

instance ToRow BaseStandingsDB where
    toRow BaseStandingsDB {..} =
        toRow (getTeamID teamID, getSeasonID season, row)

instance ToRow Record where
    toRow Record {..} = [toField wins, toField losses, toField otl]

{-------------------------- FromField Instances --------------------------}

fieldToID :: Typeable a => (Int -> a) -> String -> Field -> Ok a
fieldToID cons err sql = case fieldData sql of
    (SQLInteger n) -> Ok $ cons $ fromInteger $ toInteger n
    _ -> returnError ConversionFailed sql $ "Need a number for " <> err


instance FromField LeagueID where
    fromField = fieldToID LeagueID "LeagueID"

instance FromField FranchiseID where
    fromField = fieldToID FranchiseID "FranchiseID"

instance FromField ConferenceID where
    fromField = fieldToID ConferenceID "ConferenceID"

instance FromField DivisionID where
    fromField = fieldToID DivisionID "DivisionID"

instance FromField TeamID where
    fromField = fieldToID TeamID "TeamID"

instance FromField PlayerID where
    fromField = fieldToID PlayerID "PlayerID"

instance FromField PName where
    fromField sql = case fieldData sql of
        (SQLText t) -> Ok $ PName first last t
            where (first, last) = breakOn " " t
        _ -> returnError ConversionFailed sql "Did not receive Text for PName"

instance FromField Position where
    fromField sql = case fieldData sql of
        (SQLText "Center"    ) -> Ok $ Forward C
        (SQLText "Left Wing" ) -> Ok $ Forward LW
        (SQLText "Right Wing") -> Ok $ Forward RW
        (SQLText "Defenseman") -> Ok Defenseman
        (SQLText "Goalie"    ) -> Ok Goalie
        (SQLText "Unknown"   ) -> Ok UnknownPos
        (SQLText "N/A"       ) -> Ok UnknownPos
        x ->
            returnError ConversionFailed sql
                $ "Position - Expected one of: 'Center', 'Left Wing', 'Right Wing', 'Defenseman', 'Goalie', or 'Unknown'.\nReceived: "
                <> show x


instance FromField Handedness where
    fromField sql = case fieldData sql of
        (SQLText "Left" ) -> Ok LeftH
        (SQLText "Right") -> Ok RightH
        (SQLText x) ->
            returnError ConversionFailed sql
                $  "Hand - Expected one of: 'Left', 'Right'.\nReceived: "
                <> show x
{-------------------------- ToField Instances --------------------------}

instance ToField LeagueID where
    toField = toField . getLeagueID

instance ToField SeasonID where
    toField = toField . getSeasonID

instance ToField FranchiseID where
    toField = toField . getFranchiseID

instance ToField PlayerID where
    toField = toField . getPlayerID

instance ToField TeamID where
    toField = toField . getTeamID

instance ToField PName where
    toField = toField . fullName

instance ToField Position where
    toField = toField . show

instance ToField Handedness where
    toField = toField . show

instance ToField DivisionID where
    toField = toField . getDivisionID

instance ToField ConferenceID where
    toField = toField . getConferenceID
