module Puck.Types.Error
    ( PuckError(..)
    , showError
    ) where

import           Control.Exception
import           Data.List                      ( intercalate )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Database.SQLite.Simple
import           Network.HTTP.Types             ( Status(..) )

type Details = String

data PuckError = ParseError Details String
               | DBError Details SQLError
               | ConnError Details Status
               | ConfigError Details String
               | ErrorList [PuckError]

instance Exception PuckError

instance Show PuckError where
    show (ParseError det err   ) = "JSON Parse Error in: " <> det <> "\n" <> err
    show (DBError    det sqlerr) = "SQLError in: " <> det <> "\n" <> show sqlerr
    show (ConnError det status) =
        "Request Error in: "
            <> det
            <> "\n"
            <> show (statusCode status)
            <> " "
            <> show (statusMessage status)
    show (ConfigError det msg) =
        "Configuration Error in: " <> det <> "\n" <> msg
    show (ErrorList errs) = intercalate "\n" $ map show errs


showError :: String -> IO (Maybe a) -> IO (Maybe a)
showError str action = catch
    action
    (\e -> do
        let err = show (e :: SomeException)
        hPutStrLn stderr $ "Warning: " <> err
        hPutStrLn stderr $ "Identifier: " <> str
        return Nothing
    )

