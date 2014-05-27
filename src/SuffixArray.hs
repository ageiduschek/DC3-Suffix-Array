module SuffixArray  where

foobarbaz :: String
foobarbaz = "foobarbaz"

--import Control.Applicative
--import System.Environment
--import System.Exit
--import System.Process
--import System.IO

--import qualified Data.Map as Map
--import qualified Data.Set as Set
--import System.Directory
--import Codec.Digest.SHA
--import qualified Data.ByteString.Lazy as L
--import System.Random
--import Data.Maybe
--import Control.Monad

--type ReplicaID = Int
--type VersionNum = Int
--data WriteStamp = WriteStamp{ replica :: ReplicaID  
--                     , version :: VersionNum  
--                     , fileHash :: String   
--                     } deriving (Read, Show) 
--data DBFile = DBFile { directoryReplicaID :: ReplicaID  
--                     , vV :: Map.Map ReplicaID VersionNum  
--                     , writeStamps :: Map.Map FilePath WriteStamp   
--                     } deriving (Read, Show) 


---- | Command for executing trahs on a remote system.  The '@' will be
---- replaced by the hostname, and the directory will be appended.
--trassh :: String
--trassh = "ssh -CTaxq @ ./trahs --server"

---- | @server r w dir@ runs the code to serve the contents of @dir@,
---- reading input from @r@ and writing it to @w@.
--server :: Handle -> Handle -> FilePath -> IO ()
--server r w dir = do
--  remoteDB <- getUpdatedDB dir
--  hPutStrLn w $ show remoteDB -- send updated db to host
  
--  saveDB remoteDB dir
--  shouldTurn <- serverWaitForFileRequests r w dir
  
--  if shouldTurn 
--    then client False r w dir
--    else return ()
--  return ()

--serverWaitForFileRequests :: Handle -> Handle -> String -> IO Bool
--serverWaitForFileRequests r w dir = do 
--    flag <- hGetLine r
--    case flag of 
--        "turn" -> return True
--        ('g':'e':'t':' ':filepath) -> do sendFile w dir filepath; serverWaitForFileRequests r w dir
--        _ -> return False

--sendFile :: Handle -> String -> String -> IO ()
--sendFile w dir filename = do 
--    contents <- L.readFile (concatDirToFile dir filename)
--    hPutStrLn w $ show $ L.length contents -- send length to  
--    L.hPut w contents
  
---- | @client turn r w dir@ runs the client to update @dir@ based on
---- the remote contents.  Commands for the remote server are written to
---- @w@, while replies are read from @r@.  If @turn@, then when done
---- the client should attempt to swap roles and run the protocol in the
---- other direction (uploading any changes to the other side).
---- Otherwise, if @turn@ is false, @client@ should simply return when
---- done.
--client :: Bool -> Handle -> Handle -> FilePath -> IO ()
--client turn r w dir = do
--  localDB <- getUpdatedDB dir
--  remoteDBStr <- hGetLine r
--  let remoteDB = (read remoteDBStr :: DBFile)

--  localDB' <- clientSyncFiles r w dir localDB remoteDB

--  let localDB'' = syncVersionVectors localDB' remoteDB
--  saveDB localDB'' dir

--  if turn
--    then do 
--        hPutStrLn stderr "=== switching from client to server ==="
--        hPutStrLn w "turn" 
--        server r w dir
--    else hPutStrLn w "stop"

--syncVersionVectors :: DBFile -> DBFile -> DBFile
--syncVersionVectors localDB remoteDB = 
--    localDB{vV = Map.unionWith max (vV localDB) (vV remoteDB)}

--clientSyncFiles :: Handle -> Handle -> FilePath -> DBFile -> DBFile -> IO DBFile
--clientSyncFiles r w dir localDB remoteDB = do 
--    let (localFiles, remoteFiles) = (getFileNames localDB, getFileNames remoteDB)
--    let (filesOnClient, filesOnBoth, filesOnServer) = mapTuple Set.toList (Set.difference localFiles remoteFiles ,Set.intersection localFiles remoteFiles, Set.difference remoteFiles localFiles)
    
--    let (lvv, rvv)= (vV localDB, vV remoteDB)
--    maybeUpdatedWriteStamps <- forM filesOnBoth $ \filename -> do
--        let (lws, rws) = (getFileWriteStamp filename localDB, getFileWriteStamp filename remoteDB)
--        if writeStampCompare lws rws 
--            then return $ [Just(filename, lws)] -- keep old write stamp
--            else if (version rws) <= lvv!replica(rws)
--                then return $ [Just (filename, lws)] -- keep old write stamp
--                else if version(lws) <= rvv!replica(lws)
--                    then do copyFileFromServer r w dir filename; return ([Just (filename, rws)]);
--                    else flagConflict r w dir localDB filename lws rws --return $ [Just (filename, lws)] --TODO: CHANGE--flagConflict filename
    
--    maybeOldWriteStamps <- forM filesOnClient $ \filename -> do
--        let lws = getFileWriteStamp filename localDB
--        if (version lws) <= (rvv ! (replica lws))
--            then do deleteFile dir filename; return Nothing;
--            else return $ Just(filename, lws)

--    maybeNewWriteStamps <- forM filesOnServer $ \filename -> do
--        let rws = getFileWriteStamp filename remoteDB 
--        if (version rws) > (lvv ! (replica rws))
--                then do copyFileFromServer r w dir filename; return (Just (filename, rws));
--                else return Nothing

--    let updatedWritestampsList = catMaybes $ (concat maybeUpdatedWriteStamps) ++ maybeOldWriteStamps ++ maybeNewWriteStamps

--    return $ localDB {writeStamps=(Map.fromList updatedWritestampsList)} -- this returns the modified DB

--flagConflict :: Handle -> Handle -> String -> DBFile -> [Char] -> WriteStamp -> WriteStamp -> IO [Maybe (FilePath, WriteStamp)]
--flagConflict r w dir dirDB filename lws rws = do
--    hPutStrLn stderr $ "conflicting \"" ++ filename ++ "\""
--    let (filename1, filename2) = (conflictName filename lws, conflictName filename rws)
--    hPutStrLn stderr $ "filename1: " ++ filename1
--    hPutStrLn stderr $ "filename2: " ++ filename2
--    hPutStrLn w $ "get "++filename
--    fileLength <- hGetLine r
--    contents <- L.hGet r (read fileLength)
--    L.writeFile (concatDirToFile dir filename2) contents
--    renameFile (concatDirToFile dir filename) $ (concatDirToFile dir filename1)
    
--    newWS1 <- filenameToWriteStamp dirDB dir filename1
--    newWS2 <- filenameToWriteStamp dirDB dir filename2
--    return [Just newWS1, Just newWS2]

--conflictName:: FilePath -> WriteStamp -> FilePath
--conflictName filename ws = filename ++ "#" ++ (show $ replica ws) ++ "." ++ (show $ version ws)

--writeStampCompare :: WriteStamp -> WriteStamp -> Bool
--writeStampCompare ws1 ws2 = (replica ws1) == (replica ws2) && (version ws1 == version ws2)

--(!) :: (Map.Map ReplicaID VersionNum) -> ReplicaID -> VersionNum
--vv ! replicaID = 
--    case Map.lookup replicaID vv of
--        Just v -> v
--        Nothing -> 0

--deleteFile :: FilePath -> FilePath -> IO ()
--deleteFile dir filename = do
--    hPutStrLn stderr $ "deleting \"" ++ filename ++ "\""
--    removeFile (concatDirToFile dir filename)

--copyFileFromServer :: Handle -> Handle -> String -> [Char] -> IO ()
--copyFileFromServer r w dir filename = do
--    hPutStrLn stderr ("fetching \"" ++ filename ++ "\"")
--    hPutStrLn w $ "get "++filename
--    fileLength <- hGetLine r
--    contents <- L.hGet r (read fileLength)
--    L.writeFile (concatDirToFile dir filename) contents

--mapTuple :: (a -> b) -> (a, a, a) -> (b, b, b)
--mapTuple f (a1, a2, a3) = (f a1, f a2, f a3)

--getFileNames :: DBFile -> Set.Set FilePath
--getFileNames db = Map.keysSet (writeStamps db)

--filterIgnoredFiles :: [FilePath] -> [FilePath]
--filterIgnoredFiles files = filter (`notElem` ignoredFiles) files

--ignoredFiles :: [FilePath]
--ignoredFiles = [".trahs.db", ".trahs.db~", ".", ".."]

--hostCmd :: String -> FilePath -> IO String
--hostCmd host dir = do
--  tmpl <- maybe trassh id <$> lookupEnv "TRASSH"
--  case break (== '@') tmpl of
--    (b, '@':e) -> return $ b ++ host ++ e ++ ' ':dir
--    _          -> return $ tmpl ++ ' ':dir

--spawnRemote :: String -> FilePath -> IO (Handle, Handle)
--spawnRemote host dir = do
--  cmd <- hostCmd host dir
--  hPutStrLn stderr $ "running " ++ show cmd
--  (Just w, Just r, _, _) <- createProcess (shell cmd) {
--        std_in = CreatePipe
--      , std_out = CreatePipe
--    }
--  hSetBuffering w LineBuffering
--  return (r, w)

--connect :: String -> FilePath -> FilePath -> IO ()
--connect host rdir ldir = do
--  (r, w) <- spawnRemote host rdir
--  client True r w ldir

--trahs :: IO ()
--trahs = do
--  args <- getArgs
--  case args of
--    ["--server", l] -> do hSetBuffering stdout LineBuffering
--                          server stdin stdout l
--    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
--    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
--            exitFailure

--getUpdatedDB :: FilePath -> IO DBFile
--getUpdatedDB dir = do 
--    dbExists <- doesFileExist $ dbFile dir
--    dirDB <- if not dbExists then createDBMap else readDB dir
--    dbUpdate dirDB dir --returns an updated dirDB'

--createDBMap :: IO DBFile
--createDBMap = do
--    rgen <- newStdGen
--    return $ DBFile {directoryReplicaID=(fst $ random rgen), vV=Map.empty, writeStamps=Map.empty} 

--readDB :: FilePath -> IO DBFile
--readDB dir = do 
--    contents <- readFile $ dbFile dir
--    return $ ((read contents) :: DBFile)

--bumpedVersionNumber :: DBFile -> VersionNum
--bumpedVersionNumber dirDB = 
--    let maybeVersionNum = Map.lookup (directoryReplicaID dirDB) (vV dirDB) in 
--        case maybeVersionNum of 
--                Nothing -> 1
--                Just v -> v + 1

--bumpVersionNumber :: DBFile -> DBFile
--bumpVersionNumber dirDB =
--    DBFile {directoryReplicaID=localReplicaID, vV=updatedVV, writeStamps=(writeStamps dirDB)}
--    where 
--        localReplicaID = (directoryReplicaID dirDB)
--        updatedVV = Map.insert localReplicaID (bumpedVersionNumber dirDB) (vV dirDB)

--dbUpdate :: DBFile -> FilePath -> IO DBFile
--dbUpdate dirDB dir = do
--    let dirDB' = bumpVersionNumber dirDB
--    dirDB'' <- do 
--        contents <- getDirectoryContents dir
--        modifyDBToReflectNewContents dirDB' dir $ filterIgnoredFiles contents --filterAndFormatDirectoryContents dir contents
--    writeDBToFile dirDB'' $ tempDBFile dir
--    return dirDB''

--concatDirToFile :: String -> String -> String
--concatDirToFile dir filename = if last dir == '/' then dir ++ filename else dir ++ "/" ++ filename

--modifyDBToReflectNewContents :: DBFile -> FilePath -> [FilePath] -> IO DBFile
--modifyDBToReflectNewContents dirDB dir contents = do
--    fileWriteStampPairs <- mapM (filenameToWriteStamp dirDB dir) contents
--    return dirDB {writeStamps = (Map.fromList fileWriteStampPairs)}

--filenameToWriteStamp :: DBFile -> FilePath -> FilePath -> IO (FilePath, WriteStamp)
--filenameToWriteStamp dirDB dir f = do 
--    currentHash <- hashFile (concatDirToFile dir f)
--    updateNeeded <- shouldUpdateFileVersion f dirDB currentHash
--    let localReplicaID = directoryReplicaID dirDB
--    let oldWS = case (Map.lookup f (writeStamps dirDB)) of
--                    Just ws -> ws
--                    Nothing ->  WriteStamp {replica = localReplicaID, version = (localVersionNumber dirDB localReplicaID), fileHash = currentHash}
--    let newWS = if updateNeeded 
--                    then WriteStamp {replica = localReplicaID, version = (localVersionNumber dirDB localReplicaID), fileHash = currentHash}
--                    else oldWS
--    return (f, newWS)

--shouldUpdateFileVersion :: FilePath -> DBFile -> String -> IO Bool
--shouldUpdateFileVersion f dirDB currentHash = do
--    let oldHash = getFileHash f dirDB
--    case oldHash of
--        Just s -> return $ not (s == currentHash)
--        Nothing -> return True

--localVersionNumber :: DBFile -> ReplicaID -> VersionNum
--localVersionNumber dirDB localReplicaID = 
--    case (Map.lookup localReplicaID $ vV dirDB) of
--        Just versionNum -> versionNum
--        Nothing -> 0 -- To make the damn compiler happy

--getFileHash :: FilePath -> DBFile -> Maybe String
--getFileHash filename dirDB = do 
--    let wStamps = writeStamps dirDB
--    maybeWriteStamp <- Map.lookup filename wStamps
--    return $ fileHash maybeWriteStamp

--getFileWriteStamp :: FilePath -> DBFile -> WriteStamp
--getFileWriteStamp filename dirDB = 
--    fromJust $ Map.lookup filename $ writeStamps dirDB

--saveDB :: DBFile -> FilePath -> IO ()
--saveDB dirDB dir = do
--    writeDBToFile dirDB $ tempDBFile dir
--    renameFile (tempDBFile dir) $ dbFile dir


--writeDBToFile :: DBFile -> String -> IO ()
--writeDBToFile dirDB' dir = do
--    writeFile dir $ show (dirDB' :: DBFile)

--dbFile :: FilePath -> FilePath
--dbFile dir = dir ++ ".trahs.db"

--tempDBFile :: FilePath -> FilePath
--tempDBFile dir = dir ++ ".trahs.db~"

--hashFile :: FilePath -> IO String
--hashFile path = showBSasHex <$> (hash SHA256 <$> L.readFile path)
