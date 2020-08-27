{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

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

import qualified Data.Map as Map
import Data.String
import Hasql.Migration
  ( MigrationCommand (MigrationInitialization, MigrationScript),
    MigrationError(ScriptChanged),
    ScriptName,
    loadMigrationFromFile,
    runMigration,
    updateChecksum
  )
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as Txs
import Massalia.HasqlConnection as Connection
import Massalia.HasqlExec (QueryError, run)
import Massalia.Utils (uuidV4, intercalate)
import Protolude hiding (intercalate)
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

-- | A set of options to create/change the execution of the migration process.
data MigrationPattern
  = MigrationPattern
      { initMigrationPrefix :: !String,
        revisionMigrationPrefix :: !String,
        seedMigrationPrefix :: !(Maybe String),
        migrationPatternList :: ![FilePattern],
        basePath :: !FilePath,
        dbSchemaOption :: !(Maybe DBSchemaOption),
        migrationOrder :: Maybe (MigrationArgs -> MigrationArgs -> Ordering)
      }
-- | Options to operate the migration(s) within a given schema.
-- This is only if you want to change the overall default schema,
-- otherwise you can specify a schema in a migration file directly.
data DBSchemaOption = DBSchemaOption {
  withinSchema :: !ByteString,
  setSearchPathTo :: ![ByteString],
  commandBeforeEverything :: !(Maybe ByteString)
} deriving (Show)
defaultDBSchemaOption :: DBSchemaOption
defaultDBSchemaOption = DBSchemaOption {
  withinSchema = "public",
  setSearchPathTo = ["public"],
  commandBeforeEverything = Nothing
}

defaultMigrationPattern :: MigrationPattern
defaultMigrationPattern = MigrationPattern {
  initMigrationPrefix = "ddli",
  revisionMigrationPrefix = "ddlr",
  seedMigrationPrefix = Just "dml",
  migrationPatternList = ["**/*.sql"],
  basePath = "./",
  dbSchemaOption = Nothing,
  migrationOrder = Nothing
}

-- | A function to assemble, classify and order the migration files
-- And then execute them (and, stop in the way if any error is encountered).
findAndRunAllMigration ::
  MigrationPattern ->
  String ->
  ExceptT [GlobalMigrationError] IO ()
findAndRunAllMigration migrationPattern databaseURL = do
  orderedMigrationRegister <- withExceptT (StepFileGatheringError <$>) gatherAndOrderFile
  connection <- withStepError StepInitDBError connectionAttempt
  finalRes <- withStepError StepFileExecutionError $ executionScheme (dbSchemaOption migrationPattern) orderedMigrationRegister connection
  liftIO $ Connection.release finalRes
  where
    gatherAndOrderFile = findAndOrderAllMigration migrationPattern
    connectionAttempt = ExceptT $ connectionFromURL databaseURL
    withStepError errConstructor = withExceptT (pure . errConstructor)

-- | A function to assemble, classify and return the migration files
-- in the form of a migration register ('MigrationOrderedRegister').
findAndOrderAllMigration :: 
  MigrationPattern ->
  ExceptT [FileGatheringError] IO MigrationOrderedRegister
findAndOrderAllMigration migrationPattern =
  orderMigrationRegister migrationPattern <$> gatherFileFailOnError migrationPattern

executionScheme ::
  Maybe DBSchemaOption ->
  MigrationOrderedRegister ->
  Connection ->
  ExceptT MigrationExecutionError IO Connection
executionScheme maybeSchemaOption register dbCo = do
  let prepareInitTransaction = migrationCommandToTransaction MigrationInitialization
  initAndRevListOfTransaction <- liftIO $ (sequence $ loadInitAndRevTransaction <$> (initRevList register))
  let initAndRevTransaction = sequence initAndRevListOfTransaction
  seedTransactionList <- liftIO $ sequence <$> (sequence $ migrationArgsToTransaction <$> seedList register)
  let allTransactions = initAndRevTransaction >> seedTransactionList
  let liftedTrans = sequence <$> allTransactions
  let schemaTransaction = Right <$> (fromMaybe mempty (schemaTransactions <$> maybeSchemaOption))
  let finalTransaction = schemaTransaction >> prepareInitTransaction >> liftedTrans
  restrictedErr <- liftIO $ runTx dbCo finalTransaction
  let final = join $ (first HasqlQueryError restrictedErr)
  const dbCo <$> (ExceptT $ pure final)

schemaTransactions :: DBSchemaOption -> Tx.Transaction ()
schemaTransactions DBSchemaOption{
    withinSchema=schemaName,
    setSearchPathTo=searchPathList,
    commandBeforeEverything=commandValue
  } =
   (fromMaybe (pure ()) (Tx.sql <$> commandValue)) >>
   Tx.sql ("CREATE SCHEMA IF NOT EXISTS \""<> schemaName <> "\";") >>
   Tx.sql ("SET search_path TO " <> intercalate "," searchPathList <> ";")

type InitAndRev = (MigrationArgs, MigrationArgs)

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

orderMigrationRegister :: MigrationPattern -> MigrationRegister -> MigrationOrderedRegister
orderMigrationRegister MigrationPattern{migrationOrder=givenOrderFunction} register = case givenOrderFunction of
  Nothing -> getRes identity identity
  Just orderFunction -> getRes (sortBy tupleMigrationOrder) (sortBy orderFunction)
    where
      tupleMigrationOrder = applyMigration getInitMigration
      applyMigration f a b = orderFunction (f a) (f b)
  where
    getRes ord1 ord2 = MigrationOrderedRegister {
      initRevList = ord1 defaultInitRevList,
      seedList = ord2 defaultSeedList
    }
    defaultInitRevList = toList $ initRevMap register
    defaultSeedList = seed register

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
data MigrationOrderedRegister
  = MigrationOrderedRegister
      { initRevList :: [TupleMigration],
        seedList :: [MigrationArgs]
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
    adjustInit revMig _ (JustInit initMig) = Just $ InitAndRev initMig revMig
    adjustInit _ _ _ = Nothing
    insertJustInit (key, value) = Map.insert key (JustInit value)
    prepareMigration prefix migrationArgs =
      ( dropPrefixFromName prefix (scriptName migrationArgs),
        migrationArgs
      )

dropPrefixFromName :: String -> ScriptName -> ScriptName
dropPrefixFromName prefix = drop (length prefix)
  
tupleMigrationToTupleTransaction ::
  (TupleMigration -> MigrationArgs) ->
  TupleMigration ->
  IO (Tx.Transaction (TupleMigration, Either MigrationExecutionError ()))
tupleMigrationToTupleTransaction acc tuple = do
  migrationCommand <- loadMigrationArgs (acc tuple)
  let transaction =  migrationCommandToTransaction migrationCommand
  pure ((\trRes -> (tuple,trRes)) <$> transaction)

loadInitAndRevTransaction ::
  TupleMigration ->
  IO (Tx.Transaction (Either MigrationExecutionError ()))
loadInitAndRevTransaction tuple = case tuple of
  JustInit initMigrationArg -> do
    initMigrationCommand <- loadMigrationArgs initMigrationArg
    pure (rewritePureInitError <$> migrationCommandToTransaction (initMigrationCommand))
  InitAndRev initVal revVal -> do
    initMigrationCommand <- loadMigrationArgs initVal
    let initTransaction = migrationCommandToTransaction initMigrationCommand
    revMigrationCommand <- loadAndRenameRev revVal
    let revTransaction = migrationCommandToTransaction revMigrationCommand
    pure (do
      initRes <- initTransaction
      case initRes of
        Left (HasqlMigrationError (ScriptChanged _)) -> revTransaction >> (updateChecksumIfPossible initMigrationCommand)
        _ -> pure initRes
      )
  where
    rewritePureInitError res = case res of
      Left (HasqlMigrationError (ScriptChanged filePathVal)) -> Left $ InitFileChangeWihtoutRev filePathVal
      r -> r
    updateChecksumIfPossible migrationCom = case migrationCom of
      MigrationScript name content -> Right <$> (updateChecksum name content)
      _ -> panic "Partial pattern match OK here because 'rawInitMigration' is built this way"
    loadAndRenameRev migrationArgs = do
      (MigrationScript name content) <- loadMigrationArgs migrationArgs
      uuidVal <- uuidV4
      pure $ MigrationScript (name <> "_" <> show uuidVal) content


migrationArgsToTransaction ::
  MigrationArgs ->
  IO (Tx.Transaction (Either MigrationExecutionError ()))
migrationArgsToTransaction args = migrationCommandToTransaction <$> loadMigrationArgs args

migrationCommandToTransaction ::
  MigrationCommand ->
  Tx.Transaction (Either MigrationExecutionError ())
migrationCommandToTransaction input =
  first HasqlMigrationError <$> (maybeToLeft () <$> ingestCommand runMigration input)
  where
    ingestCommand _ !(MigrationScript _ "") = pure Nothing
    ingestCommand fn !(MigrationScript name content) = fn (MigrationScript name (" -- coming from: " <> fromString name <> fromString "\n\n" <> content))
    ingestCommand fn !a = fn a

runTx :: Connection.Connection -> Tx.Transaction a -> IO (Either QueryError a)
runTx con act = run (Txs.transaction Txs.ReadCommitted Txs.Write act) con

loadMigrationArgs :: MigrationArgs -> IO MigrationCommand
loadMigrationArgs MigrationArgs{scriptName=sn, path=scriptPath} = do
  putLText $ "Load content of " <> (show sn)
  loadMigrationFromFile sn scriptPath
