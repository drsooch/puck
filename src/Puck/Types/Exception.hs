module Puck.Types.Exception
    ( PuckException(..)
    ) where

import           Control.Exception

import           Database.SQLite.Simple
import           Network.HTTP.Types             ( Status(..) )

data PuckException = ParseError String
                   | DBError SQLError
                   | ConnError Status
                   deriving (Show)

instance Exception PuckException
