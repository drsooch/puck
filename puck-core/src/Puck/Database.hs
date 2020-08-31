module Puck.Database () where

import           Opaleye

-- SEE Puck.Player.PlayerInfo
-- This is direct from database
data PlayerInfoD a b c d e f g h i j k l m n o p q = PlayerInfoD
    { _playerID     :: a
    , _teamID       :: b
    , _firstName    :: c
    , _lastName     :: d
    , _number       :: e
    , _position     :: f
    , _hand         :: g
    , _rookie       :: h
    , _age          :: i
    , _birthDate    :: j
    , _birthCity    :: k
    , _birthState   :: l
    , _birthCountry :: m
    , _height       :: n
    , _weight       :: o
    , _active       :: p
    , _lastUpdate   :: q
    } deriving (Eq, Show)

type PlayerInfoW = PlayerInfoD
type PlayerInfoR = PlayerInfoD
type PlayerInfo  = PlayerInfoD -- I don't really want to carry all the data above, I want whats in the Player Module


data TeamInfoD a b c d e f g h = TeamInfoD
    { _teamID     :: a
    , _name       :: b
    , _abbrev     :: c
    , _division   :: d
    , _conference :: e
    , _active     :: f
    , _franID     :: g
    , _leagueID   :: h
    } deriving (Eq, Show)

type TeamInfoW = TeamInfoD -- Write
type TeamInfoR = TeamInfoD -- Read
type TeamInfo  = TeamInfoD -- This type will match Puck.Team.TeamInfo
