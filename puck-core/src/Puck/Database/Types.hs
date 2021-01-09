module Puck.Database.Types
    (
    -- Team Types
      LeagueID(..)
    , TeamInfoDB(..)
    , TeamSeasonsStatsDB(..)
    -- Player Types
    , PlayerInfoDB(..)
    , SkaterSeasonDB(..)
    , GoalieSeasonDB(..)
    -- Other Types
    , LeagueDB(..)
    , SeasonID(..)
    , BaseStandingsDB(..)
    )
where

import           Data.Time                      ( UTCTime
                                                , Day
                                                )
import           Data.Generics.Labels
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text
                                                , breakOn
                                                )
import           GHC.Generics
import           Puck.Types
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok

{------------------------------ Misc ------------------------------}
newtype SeasonID = SeasonID { getSeasonID :: Int } deriving (Generic, Eq, Show)
newtype LeagueID = LeagueID { getLeagueID :: Int } deriving (Generic, Eq, Show)

data LeagueDB = LeagueDB { leagueID :: LeagueID
                         , leagueName :: Text
                         } deriving (Generic, Eq, Show)

{------------------------------ Team ------------------------------}

data TeamInfoDB = TeamInfoDB { teamID     :: TeamID
                             , name       :: Text
                             , abbrev     :: Text
                             , division   :: Maybe DivisionID
                             , conference :: Maybe ConferenceID
                             , active     :: Bool
                             , franID     :: Maybe FranchiseID
                             , leagueID   :: Maybe LeagueID
                             } deriving (Generic, Eq, Show)



data TeamSeasonsStatsDB = TeamSeasonsStatsDB { teamID :: TeamID
                                             , season :: SeasonID
                                             , overallRecord :: OverallRecord
                                             , gamesPlayed :: Int
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
-- Generic Player Info
data PlayerInfoDB = PlayerInfoDB { playerID :: PlayerID
                                 , teamID   :: TeamID
                                 , name     :: PName
                                 , number   :: Maybe Int
                                 , position :: Position
                                 , hand     :: Handedness
                                 , age      :: Int
                                 , rookie   :: Bool
                                 } deriving (Generic, Eq, Show)


-- Possibly alter to encode what type of stats the Skater carries
--data Player = Player { playerInfo :: PlayerInfo
                     --, stats      :: PlayerStats
                     --} deriving (Generic, Eq, Show)

{-data PlayerStats = SSeasonStats -- Skater Season Stats-}
                 {-| SCareerStats -- Skater Career Stats-}
                 {-| SGameStats   -- Skater Game Stats-}
                 {-| GSeasonStats -- Goalie Season Stats-}
                 {-| GCareerStats -- Goalie Career Stats-}
                 {-| GGameStats   -- Goalie Game Stats-}
                 {-deriving (Generic, Eq, Show)-}

data SkaterSeasonDB = SkaterSeasonDB { playerID    :: PlayerID
                                     , teamID      :: TeamID
                                     , season      :: SeasonID
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
                                     } deriving (Generic, Eq, Show)

data GoalieSeasonDB = GoalieSeasonDB { playerID     :: PlayerID
                                     , teamID       :: TeamID
                                     , season       :: SeasonID
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

{------------------------------ Standings ------------------------------}

data BaseStandingsDB = BaseStandingsDB { teamID :: TeamID
                                       , season :: SeasonID
                                       , row    :: Int
                                       } deriving (Generic, Eq, Show)

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

{-------------------------- ToRow Instances --------------------------}

instance ToRow LeagueDB where
    toRow LeagueDB {..} = toRow (leagueID, leagueName)

instance ToRow PlayerInfoDB where
    toRow PlayerInfoDB {..} =
        toRow (playerID, teamID, name, number, position, hand, rookie, age)

instance ToRow TeamInfoDB where
    toRow TeamInfoDB {..} =
        toRow
            ( teamID
            , name
            , abbrev
            , division
            , conference
            , active
            , franID
            , leagueID
            )

-- SQLite.Simple doesn't have an instance for this size tuple
-- can't map toField into large array either as they aren't the
-- same type
instance ToRow TeamSeasonsStatsDB where
    toRow TeamSeasonsStatsDB {..} =
        [ toField teamID
        , toField season
        , toField gamesPlayed
        , toField wins
        , toField losses
        , toField otl
        , toField points
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
        , toField toi
        , toField goals
        , toField assists
        , toField point
        , toField ppGoals
        , toField ppAssists
        , toField ppPoints
        , toField ppToi
        , toField shGoals
        , toField shAssists
        , toField shPoints
        , toField shToi
        , toField evGoals
        , toField evAssists
        , toField evPoints
        , toField evToi
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
        , toField wins
        , toField losses
        , toField ties
        , toField shutouts
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

{-------------------------- FromField Instances --------------------------}

instance FromField FranchiseID where
    fromField sql = case fieldData sql of
        (SQLInteger n) -> Ok $ FranchiseID $ fromInteger $ toInteger n
        _ -> returnError ConversionFailed sql "Need a number for FranchiseID"

instance FromField ConferenceID where
    fromField sql = case fieldData sql of
        (SQLInteger n) -> Ok $ ConferenceID $ fromInteger $ toInteger n
        _ -> returnError ConversionFailed sql "Not a number for ConferenceID"

instance FromField DivisionID where
    fromField sql = case fieldData sql of
        (SQLInteger n) -> Ok $ DivisionID $ fromInteger $ toInteger n
        _ -> returnError ConversionFailed sql "Not a number for DivisionID"

instance FromField TeamID where
    fromField sql = case fieldData sql of
        (SQLInteger n) -> Ok $ TeamID $ fromInteger $ toInteger n
        _ -> returnError ConversionFailed sql "Not a number for TeamID"

instance FromField PlayerID where
    fromField sql = case fieldData sql of
        (SQLInteger n) -> Ok $ PlayerID $ fromInteger $ toInteger n
        _ -> returnError ConversionFailed sql "Not a number for PlayerID"

instance FromField PName where
    fromField sql = case fieldData sql of
        (SQLText t) -> Ok $ PName first last t
            where (first, last) = breakOn " " t
        _ -> returnError ConversionFailed sql "Not Text for PName"

instance FromField Position where
    fromField sql = case fieldData sql of
        (SQLText "C" ) -> Ok $ Forward C
        (SQLText "LW") -> Ok $ Forward LW
        (SQLText "RW") -> Ok $ Forward RW
        (SQLText "D" ) -> Ok Defenseman
        (SQLText "G" ) -> Ok Goalie
        _              -> returnError
            ConversionFailed
            sql
            "Position - Expected one of: 'C', 'LW', 'RW', 'D', 'G'"

instance FromField Handedness where
    fromField sql = case fieldData sql of
        (SQLText "L") -> Ok LeftH
        (SQLText "R") -> Ok RightH
        _ ->
            returnError ConversionFailed sql "Hand - Expected one of: 'L', 'R'"
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
