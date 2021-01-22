module Puck.Database.Populate
    ( populateDB
    ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar

import           Control.Exception              ( bracket )
import           Control.Monad                  ( forM_ )
import           Data.Text                      ( Text )
import           System.IO                      ( IOMode(AppendMode)
                                                , hPutStrLn
                                                , stderr
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )


import           Database.SQLite.Simple         ( Connection
                                                , withConnection
                                                )
import           Network.HTTP.Client            ( Manager
                                                , newManager
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )

import           Puck.Requests                  ( requestAllTeams
                                                , requestCareerStats
                                                , requestCurrentSeason
                                                , requestPlayerInfo
                                                , requestPlayerStats
                                                , requestSkaterStats
                                                , requestTeamInfo
                                                , requestTeamRoster
                                                )

import           Puck.Database.Core
import           Puck.Database.Types
import           Puck.Types
import           Puck.Types.Error

--------------------------------------------------------------------------------------
-- | This module is purely script-esque. It's only job is to setup a database and pull
-- | in required information for every team and player.
-- | It uses multiple threads to allow different threads to do different task.
-- | It's setup in a pipeline so that when a team or player is finished it gets passed
-- | on to the next worker.
-- | In its current state, it has very little error handling, most of it is just
-- | unwrapping maybe values and killing the thread on Nothing Values.
-- |
-- | There is also the issue of Database Connection Concurrency. SQLite3 is not particularly
-- | robust in this sense, well the Database.SQLite.Simple doesn't really offer us a way to
-- | concurrently connect. As far as I can tell there are no cursors like Postgres.
-- | This means that when we get to PlayerStatsFullW, we are literally SOL.
-- | There are so many threads trying to access the SINGLE connection that it's hard to get
-- | any work done.
--------------------------------------------------------------------------------------





type TeamRosterQ = Chan (Maybe TeamID)
type PlayerIDQ = Chan (Maybe PlayerID)


-- 4 seems to be the best number so far, it's possible
-- we get a rate limit or something:
-- Timings for TeamInfo and PlayerInfo (Currently PlayerStats takes forever so we measure the first two):
--       2 workers -> Approximately 1 minute
--       3 workers -> Approximately 40 seconds
--       4 workers -> Approximately 15 seconds
--       5 workers -> Approximately 2.5 minutes 
maxWorkers :: Int
maxWorkers = 4

workerThreads :: MVar [MVar ()]
workerThreads = unsafePerformIO (newMVar [])
{-# NOINLINE workerThreads #-}

mainThread :: IO ThreadId
mainThread = myThreadId

waitForWorkers :: IO ()
waitForWorkers = do
    cs <- takeMVar workerThreads
    case cs of
        []     -> return ()
        m : ms -> do
            putMVar workerThreads ms
            takeMVar m
            waitForWorkers

forkWorker :: IO () -> IO ThreadId
forkWorker io = do
    mvar    <- newEmptyMVar
    workers <- takeMVar workerThreads
    putMVar workerThreads (mvar : workers)
    forkFinally io (\_ -> putMVar mvar ())

populateDB :: FilePath -> IO ()
populateDB fp = withConnection fp $ \conn -> do
    manager          <- newManager tlsManagerSettings -- per the Docs managers close automatically
    connLock         <- newMVar conn                  -- flag for blocking database writes/reads
    teamIDQueue      <- newChan :: IO TeamRosterQ     -- queue for TeamIDs to get roster
    playerInfoQueue  <- newChan :: IO PlayerIDQ       -- queue for parsing and requsting player information
    playerStatsQueue <- newChan :: IO PlayerIDQ       -- queue for parsing and requesting player stats

    -- start off by inserting NHL into the database, its a primary key for other tables
    insertNHL conn

    -- create the NHL team info rows, and return the IDs of all NHL teams
    teamIDs <- getTeamIDs <$> populateTeams conn manager

    -- send the list of teamids into the queue
    writeList2Chan teamIDQueue (map Just teamIDs)
    -- sentinel to shutdown worker threads
    writeChan teamIDQueue Nothing
    -- spawn threads 
    forM_ [0 .. maxWorkers] $ \_ -> do
        forkWorker (rosterW manager teamIDQueue playerInfoQueue)
        -- we fork these twice as players vastly outnumber teams (obviously)
        forkWorker
            (playerInfoW connLock manager playerInfoQueue playerStatsQueue)
        forkWorker
            (playerInfoW connLock manager playerInfoQueue playerStatsQueue)
        forkWorker (playerStatsFullW connLock manager playerStatsQueue)
        forkWorker (playerStatsFullW connLock manager playerStatsQueue)

    waitForWorkers

-- get all NHL Teams parse and insert into Database
-- return all the TeamIDs
populateTeams :: Connection -> Manager -> IO [TeamInfoDB]
populateTeams conn manager = do
    requestAllTeams manager >>= \case
        Just x -> executeMany conn insertTeamInfo x >> return x
        Nothing ->
            hPutStrLn stderr "Warning: Failed requestAllTeams" >> return []

-- get list of playerids that make up a teams roster
-- pass along the player list to playerInfoW
rosterW :: Manager -> TeamRosterQ -> PlayerIDQ -> IO ()
rosterW manager from to = do
    -- wait for a team ID to act on
    mTid <- readChan from
    case mTid of
        Nothing ->
            writeChan to Nothing           -- signal playerWorkers we have no more info
                >>  writeChan from Nothing -- allow other rosterWorkers a chance to exit
                >>  myThreadId
                >>= \t -> putStrLn $ "Exiting RosterW: " <> show t
        Just tid -> do
            -- get the roster for team tid
            pids <- async (requestTeamRoster tid manager) >>= wait >>= \case
                Nothing ->
                    hPutStrLn stderr
                              ("Failed Request Roster Worker" <> show tid)
                        >> error "RW"
                Just x -> return x   -- unwrap from the Just
            writeList2Chan to (map Just pids) -- this writes every player Id to the queue
            rosterW manager from to

playerInfoW :: MVar Connection -> Manager -> PlayerIDQ -> PlayerIDQ -> IO ()
playerInfoW conn manager from to = do
    -- wait for a player id to act on
    mPid <- readChan from
    case mPid of
        Nothing ->
            writeChan to Nothing    -- allow playerInfoWorkers to see Sentinel
                >>  writeChan from Nothing  -- Propogate Sentinel
                >>  myThreadId
                >>= \t -> putStrLn $ "Exiting PlayerInfoW: " <> show t
        Just pid -> do
            -- get the playerInfo
            pi <- async (requestPlayerInfo pid manager) >>= wait >>= \case
                Nothing ->
                    hPutStrLn
                            stderr
                            ("Failed Request Player Info Worker" <> show pid)
                        >> error "PIW"
                Just x -> return x -- unwrap from Just
            -- take the database connection to insert the playerInfo
            takeConnection conn (insertPlayerInfo pi)
            -- propogate player Id to the stats queue
            writeChan to (Just $ pi ^. #playerID)
            playerInfoW conn manager from to


playerStatsFullW :: MVar Connection -> Manager -> PlayerIDQ -> IO ()
playerStatsFullW conn manager from = do
    -- wait for player info
    mPid <- readChan from
    case mPid of
        Nothing -> writeChan from Nothing >> myThreadId >>= \t ->
            putStrLn $ "Exiting PlayerStatsFullW: " <> show t
        Just pid -> do
            -- wait for career stats
            careerStats <-
                async (requestCareerStats pid manager) >>= wait >>= \case
                    Nothing ->
                        hPutStrLn
                                stderr
                                (  "Failed Request Player Stats Worker"
                                <> show pid
                                )
                            >> error "PSW"
                    Just x -> return x
            forM_ careerStats $ \cs -> handleSeason conn manager cs
            playerStatsFullW conn manager from

-- handles an entire season stats:
-- inserts a new league into DB, and gets the LeagueID if there is none
-- inserts a new team into DB, and gets the team ID if there is none
-- finally, inserts the season stats
handleSeason :: MVar Connection -> Manager -> SingleSeasonCS -> IO ()
handleSeason conn manager (ps, team, league) = do
    let pid = case ps of
            GSeasonStats stats -> stats ^. #playerID
            SSeasonStats stats -> stats ^. #playerID
    (Just lid) <-
        showError (show league <> show pid)
        $   Just
        <$> handleLeague conn manager league
    (Just tid) <-
        showError (show team <> show pid)
        $   Just
        <$> handleTeam conn manager team lid
    let ps' = replaceTeamID tid ps

    showError (show team <> show league <> show pid) $ Just <$> takeConnection
        conn
        (insertPlayerSeasonStats $ replaceTeamID tid ps)
    return ()
    {-handleLeague conn manager league-}
        {->>= handleTeam conn manager team-}
        {->>= \tid -> takeConnection-}
                {-conn-}
                {-(insertPlayerSeasonStats $ replaceTeamID tid ps)-}

-- handles league Tuple of SingleSeasonCS
handleLeague
    :: MVar Connection -> Manager -> (Maybe LeagueID, Text) -> IO LeagueID
-- insertLeague uses an UPSERT so we can ignore duplicates safely
handleLeague conn _ (Just leagueID, leagueName) =
    takeConnection conn (insertLeague LeagueDB { .. }) >> return leagueID
handleLeague conn manager (Nothing, leagueName) = do
    -- if the league name exists, return the ID
    -- otherwise insert the league in by just name and rerun a selection to
    -- get the DB defined ID
    -- FIXME: Inserting into DB may cause an ID to be given to a team
    -- even if the ID exists for another League we havent seen yet...
    takeConnection conn (selectLeagueByName leagueName) >>= \case
        Just LeagueDB {..} -> return leagueID
        Nothing ->
            takeConnection conn (insertLeagueNoID leagueName)
                >>  takeConnection conn (selectLeagueByName leagueName)
                >>= \case
                        Nothing -> hPutStrLn stderr "Failed Insert of League"
                            >> error "IL"
                        Just LeagueDB {..} -> return leagueID


-- handles the team tuple of SingleSeasonCS
handleTeam
    :: MVar Connection
    -> Manager
    -> (Maybe TeamID, Text)
    -> LeagueID
    -> IO TeamID
-- if we get an actual ID, we make sure it exists in the database
-- otherwise we construct a new teaminfo record
handleTeam conn manager (Just teamID, teamName) lid = do
    takeConnection conn (selectTeamInfoByID teamID) >>= \case
        Just _  -> return teamID
        Nothing -> async (requestTeamInfo teamID manager) >>= wait >>= \case
            {-Nothing -> error "HandleTeam" -- This would be connection failure-}
            Nothing ->
                hPutStrLn stderr ("Failed Request of Team" <> show teamID)
                    >> error "RTID"
            Just ti -> -- here we just insert it and return ID
                -- also note that teams that are vague won't have the proper
                -- league Id so we HAVE to update it here
                takeConnection conn (insertTeamInfo (ti & #leagueID .~ lid))
                    >> return teamID
-- when we don't have an id we have to be clever 
handleTeam conn manager (Nothing, teamName) lid = do
    -- teamName and league id form a pseudo-primary key of team table
    -- we check to see if we have a match and if we do just return the ID
    takeConnection conn (selectTeamInfoByName teamName lid) >>= \case
        Just ti -> return (ti ^. #teamID)
        Nothing -> -- here we don't have a match so we let the database create the ID
            takeConnection
                    conn
                    (insertTeamInfoNoID $ mkDefaultTeamInfoDBNoID lid teamName)
                >>  takeConnection conn (selectTeamInfoByName teamName lid)
                >>= \case
                        Nothing -> hPutStrLn stderr "Failed Select of Team ID"
                            >> error "STID"
                        {-Nothing -> error "HandleTeam" -- really shouldn't happen-}
                        Just ti -> return (ti ^. #teamID)

takeConnection :: MVar Connection -> (Connection -> IO a) -> IO a
takeConnection conn = bracket (takeMVar conn) (putMVar conn)

replaceTeamID :: TeamID -> PlayerStatsDB -> PlayerStatsDB
replaceTeamID tid (GSeasonStats stats) = GSeasonStats $ stats { teamID = tid }
replaceTeamID tid (SSeasonStats stats) = SSeasonStats $ stats { teamID = tid }

-- probably don't need this explicitly
getTeamIDs :: [TeamInfoDB] -> [TeamID]
getTeamIDs teams = teams ^.. traversed . #teamID

-- we have to hardcode the NHL into the DB
insertNHL :: Connection -> IO ()
insertNHL = insertLeague LeagueDB { leagueID   = LeagueID 133
                                  , leagueName = "National Hockey League"
                                  }
