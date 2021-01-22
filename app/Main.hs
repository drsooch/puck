module Main where

import           Control.Exception
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.Char                      ( isSpace )
import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                , getAppUserDataDirectory
                                                )
import           System.Exit
import           System.FilePath.Posix          ( (</>) )
import           System.IO                      ( Handle
                                                , IOMode(WriteMode)
                                                , getChar
                                                , openFile
                                                )

import           Database.SQLite.Simple         ( Connection
                                                , withConnection
                                                )


import           Puck.Database.Core             ( connectDB )
import           Puck.Database.Populate
import           Puck.Database.Schema           ( setupDB )
import           Puck.Types
import           Puck.Types.Error

main :: IO ()
main = do
    dbFp <- getDatabaseFP
    populateDB dbFp
    return ()

-- get the database filepath, if it doesn't exist, we create it
getDatabaseFP :: IO FilePath
getDatabaseFP =
    getAppUserDataDirectory "puck" >>= checkAppDir >>= checkDatabaseFile

checkAppDir :: FilePath -> IO FilePath
checkAppDir fp = do
    dirExist <- doesDirectoryExist fp
    if dirExist then return (fp </> "puck.sqlite3") else setupAppDir fp

-- create the .puck directory and return the path to the (uncreated) database
setupAppDir :: FilePath -> IO FilePath
setupAppDir fp = createDirectory fp >> return (fp </> "puck.sqlite3")

-- check the database file exists and return its file path
checkDatabaseFile :: FilePath -> IO FilePath
checkDatabaseFile fp = do
    dbExist <- doesFileExist fp
    unless dbExist (promptUserDB fp)
    return fp

promptUserDB :: FilePath -> IO ()
promptUserDB fp = do
    mapM_
        putStrLn
        [ "Puck Database has not been found, initial setup will begin."
        , "This could take a while, there is a dump of the database at:"
        , "https://github.com/drsooch/puck"
        , "Place the dump at: " <> fp
        , "Press Enter to continue or kill the process:"
        ]
    _ <- getChar  -- this is just so the user has an opportunity to cancel
    putStrLn "Setting up Database..."
    setupDB fp
    return ()



