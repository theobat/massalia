{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.HasqlConnection
-- Description : A module to enhance "Hasql.Migration" baseline.
--  The idea is to split Data Definition Language migrations in two kinds:
--    - The <<Init>> which are supposed to be executed only once.
--    - The <<Revision>> which are supposed to be executed only once.
--  If an init migration has not been executed, it's executed, if it has been and
--  the file has not changed, nothing happens. But if the file has changed and there's a
--  revision migration for this init migration we execute the revision and replace the
--  file MD5 with the new one.
module Massalia.Migration where

-- CheckScriptResult(ScriptOk, ScriptModified, ScriptNotExecuted)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Hasql.Migration
  ( MigrationCommand (MigrationInitialization, MigrationScript),
    MigrationError(ScriptChanged, NotInitialised, ScriptMissing, ChecksumMismatch),
    ScriptName,
    loadMigrationFromFile,
    runMigration,
    updateChecksum
  )
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import Hasql.URL (parseDatabaseUrl)
import Massalia.HasqlConnection as Connection
import Massalia.HasqlExec (QueryError, run)
import Massalia.Utils (pPrint, uuidV4)
import Protolude
import System.FilePath.Posix (splitFileName, (</>))
import System.FilePattern.Directory (FilePattern, getDirectoryFiles)

data GlobalMigrationError
  = StepInitDBError InitDBConnectionError
  | StepFileGatheringError FileGatheringError
  | StepFileExecutionError MigrationExecutionError
  deriving (Eq, Show)

data FileGatheringError
  = FileHasNoMigrationType FilePath
  | RevMigrationWithoutInitMigration FilePath
  deriving (Eq, Show)

data MigrationExecutionError
  = HasqlQueryError QueryError
  | HasqlMigrationError MigrationError
  | InitFileChangeWihtoutRev FilePath
  | InitChecksumUpdateError QueryError
  deriving (Eq, Show)

-- | A Massalia migration consists of two types of migration, the **init** migrations and
-- the **revision** migrations. The init migrations are the one applied and tracked using
-- "Hasql.Migration". A revision migration alone is never used.
-- The idea is to find all the init migrations and try to apply them using 'loadMigrationFromFile'
-- (based on the initMigrationPrefixValue)

-- init => applied first:
--          - if not applied, executed following Hasql.Migration concept
--          - if not applied, executed following Hasql.Migration concept
--
-- revision =>
-- Revision ==
data MigrationPattern
  = MigrationPattern
      { initMigrationPrefix :: String,
        revisionMigrationPrefix :: String,
        seedMigrationPrefix :: Maybe String,
        migrationPatternList :: [FilePattern],
        basePath :: FilePath
      }
      deriving (Eq, Show, Read)

defaultMigrationPattern :: MigrationPattern
defaultMigrationPattern = MigrationPattern {
  initMigrationPrefix = "ddli",
  revisionMigrationPrefix = "ddlr",
  seedMigrationPrefix = Just "dml",
  migrationPatternList = ["**/*.sql"],
  basePath = "./"
}

findAndRunAllMigration ::
  MigrationPattern ->
  String ->
  ExceptT [GlobalMigrationError] IO ()
findAndRunAllMigration migrationPattern databaseURL = do
  migrationRegister <- withExceptT (StepFileGatheringError <$>) gatherFileAttempt 
  connection <- withStepError StepInitDBError connectionAttempt
  finalRes <- withStepError StepFileExecutionError $ executionScheme migrationRegister connection
  liftIO $ Connection.release finalRes
  where
    gatherFileAttempt = gatherFileFailOnError migrationPattern
    connectionAttempt = ExceptT $ connectionFromURL databaseURL
    withStepError errConstructor = withExceptT (pure . errConstructor)

executionScheme ::
  MigrationRegister ->
  Connection ->
  ExceptT MigrationExecutionError IO Connection
executionScheme register dbCo = do
  dbCoWithInit <- ExceptT $ runMassaliaMigrationCommand dbCo MigrationInitialization
  tupleToRevise <- foldM (initMigrationProcess dbCoWithInit) [] (initRevMap register)
  dbCoWithRevision <- foldM revisionMigrationProcess dbCoWithInit tupleToRevise
  foldM simpleExec dbCoWithRevision (seed register) -- connection has to be passed along to ensure sequential execution
  where
    simpleExec a b = ExceptT $ runMassaliaMigrationArgs a b

type InitAndRev = (MigrationArgs, MigrationArgs)

initMigrationProcess :: Connection -> [InitAndRev] -> TupleMigration -> ExceptT MigrationExecutionError IO [InitAndRev]
initMigrationProcess connection currentRevisionMigrationList args = finalRes
  where
    finalRes = ExceptT $ executionRes <$> runMassaliaMigrationArgs connection (getInitMigration args)
    executionRes res = case res of
      Left (HasqlMigrationError (ScriptChanged filePathVal)) -> case args of
        JustInit _ -> Left $ InitFileChangeWihtoutRev filePathVal
        InitAndRev initVal revVal -> Right $ currentRevisionMigrationList <> [(initVal, revVal)]
      Left err -> Left err
      _ -> Right currentRevisionMigrationList

revisionMigrationProcess :: Connection -> InitAndRev -> ExceptT MigrationExecutionError IO Connection
revisionMigrationProcess connection (initVal, revVal) = do
  withExceptT InitChecksumUpdateError $ ExceptT $ join $ updateChecksumIfPossible <$> rawInitMigration
  revValProperlyNamed <- liftIO $ loadAndRenameRev revVal
  ExceptT $ runMassaliaMigrationCommand connection revValProperlyNamed
  where
    updateChecksumIfPossible input = case input of
      MigrationScript name content -> runTx connection $ updateChecksum name content
      -- Partial pattern match OK here because 'rawInitMigration' is built this way.
    rawInitMigration = loadMigrationArgs initVal
    loadAndRenameRev migrationArgs = do
      (MigrationScript name content) <- loadMigrationArgs migrationArgs
      uuidVal <- uuidV4
      pure $ MigrationScript (name <> "_" <> show uuidVal) content

gatherFileFailOnError :: MigrationPattern -> ExceptT [FileGatheringError] IO MigrationRegister
gatherFileFailOnError migPattern = ExceptT $ tupleToEither <$> gatherAllMigrationFiles migPattern
  where
    tupleToEither inputTuple = case inputTuple of
      ([], result) -> Right result
      (errList, _) -> Left errList

gatherAllMigrationFiles :: MigrationPattern -> IO ([FileGatheringError], MigrationRegister)
gatherAllMigrationFiles mig = do
  filePathList <- getDirectoryFiles (basePath mig) (migrationPatternList mig)
  let (migrationErrorList, migrationRegister) = buildMigrationRegister mig filePathList
  pure (migrationErrorList, migrationRegister)

-- splitFileName
data MigrationArgs
  = MigrationArgs
      { scriptName :: ScriptName,
        path :: FilePath
      }
  deriving (Eq, Show, Ord)

-- | An init mogration assembled
data TupleMigration
  = JustInit MigrationArgs
  | InitAndRev MigrationArgs MigrationArgs
  deriving (Eq, Show, Ord)
getInitMigration :: TupleMigration -> MigrationArgs
getInitMigration input = case input of
  JustInit init -> init
  InitAndRev init _ -> init

data InitOrRevMigration = Init String MigrationArgs | Rev String MigrationArgs deriving (Eq, Show, Ord)

data AllMigration = InitOrRev InitOrRevMigration | Seed MigrationArgs deriving (Eq, Show, Ord)

-- | Takes naming (prefix) rules encoded in a 'MigrationPattern' and returns
-- a classified file among the 'AllMigration' constructor.
classifyFile :: MigrationPattern -> FilePath -> Either FileGatheringError AllMigration
classifyFile migrationPattern filePath
  | isFilenamePrefix revisionPrefix = makeInitOrRev $ Rev revisionPrefix (MigrationArgs fileName fullPath)
  | isFilenamePrefix initPrefix = makeInitOrRev $ Init initPrefix (MigrationArgs fileName fullPath)
  | isSeed = Right $ Seed (MigrationArgs fileName fullPath)
  | otherwise = Left $ FileHasNoMigrationType fullPath
  where
    makeInitOrRev = Right . InitOrRev
    fullPath = (basePath migrationPattern) </> filePath
    isSeed = maybe False isFilenamePrefix (seedMigrationPrefix migrationPattern)
    revisionPrefix = revisionMigrationPrefix migrationPattern
    initPrefix = initMigrationPrefix migrationPattern
    isFilenamePrefix prefix = prefix `isPrefixOf` fileName
    (_, fileName) = splitFileName filePath

data MigrationRegister
  = MigrationRegister
      { initRevMap :: Map ScriptName TupleMigration,
        seed :: [MigrationArgs]
      }
      deriving (Eq, Show)

instance Semigroup MigrationRegister where
  (<>) a b =
    MigrationRegister
      { initRevMap = (initRevMap a <> initRevMap b),
        seed = (seed a <> seed b)
      }

instance Monoid MigrationRegister where
  mempty = MigrationRegister {initRevMap = mempty, seed = mempty}

-- | Smaller means it's processed before, greater means after.
processingOrder :: Either FileGatheringError AllMigration -> Int
processingOrder input = case input of
  Left _ -> -1
  Right (Seed _) -> 1
  Right (InitOrRev (Rev _ _)) -> 2
  Right (InitOrRev (Init _ _)) -> 3

buildMigrationRegister :: MigrationPattern -> [FilePath] -> ([FileGatheringError], MigrationRegister)
buildMigrationRegister migrationPattern listToProcess = foldr iterator ([], mempty) sortedList
  where
    classifiedList = classifyFile migrationPattern <$> listToProcess
    sortedList = sortOn processingOrder classifiedList
    iterator migration (errorList, prevRegister) = case migration of
      Right allMigration -> case allMigration of
        Seed mig -> (errorList, prevRegister {seed = pure mig <> seed prevRegister})
        InitOrRev initOrRev -> setInitRev initOrRev
        where
          setInitRev input = case handleInitRev input currentInitRevMap of
            Left err -> (errorList <> [err], prevRegister)
            Right newMap -> (errorList, prevRegister {initRevMap = newMap})
          currentInitRevMap = initRevMap prevRegister
      Left err -> (errorList <> [err], prevRegister)

handleInitRev ::
  InitOrRevMigration ->
  Map ScriptName TupleMigration ->
  Either FileGatheringError (Map ScriptName TupleMigration)
handleInitRev input mapp = case input of
  Init prefix migrationArgs -> Right $ insertJustInit (prepareMigration prefix migrationArgs) mapp
  Rev prefix migrationArgs -> case insertRev (prepareMigration prefix migrationArgs) mapp of
    (Nothing, _) -> Left $ RevMigrationWithoutInitMigration (path migrationArgs)
    (_, result) -> Right result
  where
    insertRev (key, value) = Map.updateLookupWithKey (adjustInit value) key
    adjustInit revMig key (JustInit initMig) = Just $ InitAndRev initMig revMig
    adjustInit _ _ _ = Nothing
    insertJustInit (key, value) = Map.insert key (JustInit value)
    prepareMigration prefix migrationArgs =
      ( dropPrefixFromName prefix (scriptName migrationArgs),
        migrationArgs
      )

dropPrefixFromName :: String -> ScriptName -> ScriptName
dropPrefixFromName prefix = drop (length prefix)
  
runMassaliaMigrationArgs :: 
  Connection ->
  MigrationArgs ->
  IO (Either MigrationExecutionError Connection)
runMassaliaMigrationArgs connection args = join res
  where
    res = runMassaliaMigrationCommand connection <$> migrationCommand
    migrationCommand = loadMigrationArgs args

runMassaliaMigrationCommand ::
  Connection ->
  MigrationCommand ->
  IO (Either MigrationExecutionError Connection)
runMassaliaMigrationCommand connection migrationCommand = result
  where
    result = join <$> (runTxMigration connection $ runMigrationWrappedError migrationCommand)
    runTxMigration connection transaction = first HasqlQueryError <$> runTx connection transaction
    runMigrationWrappedError migrationCommand = first HasqlMigrationError <$> (maybeToLeft connection <$> runMigration migrationCommand)

runTx :: Connection.Connection -> Tx.Transaction a -> IO (Either QueryError a)
runTx con act = run (Txs.transaction Txs.ReadCommitted Txs.Write act) con

loadMigrationArgs :: MigrationArgs -> IO MigrationCommand
loadMigrationArgs MigrationArgs{scriptName=sn, path=scriptPath} = loadMigrationFromFile sn scriptPath