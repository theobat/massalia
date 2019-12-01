{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lib
    ( someFunc
    ) where

import           Data.Morpheus.Kind     (OBJECT, ENUM, SCALAR)
import           Data.Data
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Control.Monad.Except       (ExceptT (..))
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus                  ( interpreter
                                                )
import           Data.Morpheus.Types        (ScalarValue(String), GQLScalar(..), ResolveQ, liftEither, GQLRequest(..),
  GQLResponse, Resolver (..), IORes, GQLRootResolver (..), GQLType(..), Undefined (..))
import           Control.Monad.Identity (Identity)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Data.Int (Int64)
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( SelectionRec(..), ValidSelection, SelectionSet )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key )
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Data.Maybe (fromJust)
import           Data.Bifunctor                     ( first )
import           Control.Monad.IO.Class (liftIO)
import Data.UUID
import qualified Debug.Trace as Debug
import qualified Data.Text as T
import           Data.Text.Encoding             ( encodeUtf8 )

someFunc :: IO ()
someFunc = do
  rawConnection <- Connection.acquire connectionSettings
  connection <- case rawConnection of
    Left e -> (error $ show e)
    Right goodCo -> pure goodCo
  -- result <- Session.run (Session.statement () selectSum) connection
  -- queryTest <- Session.run (Session.statement () queryCrashTest) connection
  -- print (show <$> queryTest)
  -- print (plantId <$> queryTest)

  res <- testAPI (connection, fieldList) (customQuery fieldList)
  print res
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"
    fieldList = ["plantId", "testOK", "truckList"]

customQuery :: [Text] -> GQLRequest
customQuery fieldList = GQLRequest { operationName = Nothing
                         , query         = "query thisIsMyQueryName { plantList { " <> (T.unwords fieldList) <> " { truckId } } }"
                         , variables     = Nothing
                         }


queryCrashTest :: Statement () Plant
queryCrashTest = Statement sql encoder decoder True where
  sql = selectSQL
  selectSQL =
    "select row(plant.id, 1, 'okokok') as plant_data from plant join truck_plant ON truck_plant.plant_id=plant.id JOIN truck ON truck.id=truck_plant.truck_id group by plant.id limit 1"
  encoder    = Encoders.noParams
  decoderCompositeCol = Decoders.nonNullable Decoders.uuid
  defaultVal = Plant{}
  decoderComposite = do
    plant1 <- (\val -> defaultVal{
      plantId = val
    }) <$> Decoders.field (Decoders.nonNullable Decoders.uuid)
    plant2 <- (\val -> plant1{
      exNumber = fromIntegral val
    }) <$> Decoders.field (Decoders.nonNullable Decoders.int8)
    pure plant2
    -- (Decoders.nonNullable Decoders.uuid)
    -- <*> Decoders.field
    -- (Decoders.nonNullable (fromIntegral <$> Decoders.int2))
    -- <*> Decoders.field
    -- (Decoders.nonNullable Decoders.text)
  decoder = Decoders.singleRow
    ( Decoders.column
    $ Decoders.nonNullable
    $ Decoders.composite
    $ decoderComposite
    )


data Query m = Query
  { plantList :: () -> m [Plant]
  } deriving (Generic, GQLType)

-- data DeityArgs = DeityArgs
--   { name      :: Text -- Required Argument
--   , mythology :: Maybe Text -- Optional Argument
--   } deriving (Generic)

-- fakeDBPlant :: () -> IO (Either String Plant)
-- fakeDBPlant _ = return $ Right $ Plant "" 1 ["ok"]

resolvePlantList :: (Connection.Connection, [Text])  -> () -> IORes m [Plant]
resolvePlantList (connection, fieldList) _ = liftEither $ testInIO
  where
    -- makeList = (fmap (\p -> [p]))
    -- fakeDB = makeList <$> (fakeDBPlant ())
    -- testWithoutLeft = (\inp -> case inp of
    --   Left e -> error e
    --   Right a -> a) <$> testInIO
    testInIO =
      fmap (first show) test
    test = fmap (fmap (\p -> [p])) (Session.run (Session.statement () (selectSum fieldList)) connection)

-- getPlantList :: IORes e [Plant]

rootResolver
  :: (Connection.Connection, [Text]) -> GQLRootResolver IO () Query Undefined Undefined
rootResolver config =
  GQLRootResolver
    { queryResolver = Query {plantList = resolvePlantList config }
    , mutationResolver =  Undefined
    , subscriptionResolver = Undefined
    }

testAPI :: (Connection.Connection, [Text]) -> GQLRequest -> IO GQLResponse
testAPI config = interpreter $ rootResolver config

selectSum :: [Text] -> Statement () Plant
selectSum fieldList = Statement (encodeUtf8 sql) encoder decoder True where
  compute = decodePlant fieldList
  sql = "select row("
      <> (T.intercalate "," $ selectPart compute)
      <> ") as plant_data"
      <> " "
      <>   filtersSQL
      <>   " "
      <>   groupbySQL
  filtersSQL = "from plant join truck_plant ON truck_plant.plant_id=plant.id JOIN truck ON truck.id=truck_plant.truck_id"
  groupbySQL = "group by plant.id limit 1"
  encoder = Encoders.noParams
  compositeDecoder = Decoders.column $ Decoders.nonNullable $ Decoders.composite $ statementDecoder compute
  decoder = Decoders.singleRow compositeDecoder

-- selectPlant :: Statement () Int64
-- selectTest = Statement sql encoder decoder True where
--   sql = "select 1"
--   encoder = Encoders.noParams
--   decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

decodePlant :: [Text] -> SQLStructure () Plant
decodePlant fields = testFields
  where
    defaultSQLStruct = SQLStructure { selectPart = [], statementDecoder = pure plantDefault}
    testFields = foldr updatePlant defaultSQLStruct fields
    -- decoderCompositeCol = Decoders.nullable Decoders.text
    -- decoderComposite = Decoders.field decoderCompositeCol
    -- decoderCompositeTuple =
    --   updatePlantId <$> (fromJust <$> decoderComposite)

-- plantFieldEffect :: Text -> Plant -> Plant
-- plantFieldEffect "plantId" plant = plant { plantId = plantId }

plantDefault = Plant {}
truckDefault = Truck {}

-- updatePlant :: Text -> Plant -> Plant
updatePlant
  :: Text -> SQLStructure () Plant -> SQLStructure () Plant
updatePlant "plantId" sqlST = sqlST {
  selectPart = selectPart sqlST <> ["plant.id"],
  statementDecoder = do
    plant <- statementDecoder sqlST
    (\val -> plant { plantId = Debug.traceShowId val }) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
}
updatePlant "testOK" sqlST = sqlST {
  selectPart = selectPart sqlST <> ["'okokok'"],
  statementDecoder = do
    plant <- statementDecoder sqlST
    (\val -> plant { testOK = val }) <$> (Decoders.field (Decoders.nonNullable Decoders.text))
}
updatePlant "truckList" sqlST = sqlST {
  selectPart = selectPart sqlST <> ["array_agg(row(truck.id))"],
  statementDecoder = do
    plant <- statementDecoder sqlST
    (\val -> plant { truckList = val }) <$> (
      Decoders.field $ Decoders.nonNullable (Decoders.listArray $ Decoders.nonNullable (Decoders.composite $ statementDecoder truckUpdater)) )
}
  where truckUpdater = updateTruck "truckId" SQLStructure { selectPart = [], statementDecoder = pure truckDefault }
updatePlant anything _ = error ("field " ++ T.unpack anything ++ " is not supported yet")

updateTruck :: Text -> SQLStructure () Truck -> SQLStructure () Truck
updateTruck "truckId" sqlST = sqlST
  { statementDecoder =
    do
      truck <- statementDecoder sqlST
      (\val -> truck { truckId = Debug.traceShowId val })
        <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
  }

data Plant = Plant {
  plantId :: UUID
  , exNumber :: Int
  , testOK :: Text
  -- , productList :: [Text]
  , truckList :: [Truck]
} deriving (Generic, Typeable)

instance GQLType UUID where
  type KIND UUID = SCALAR

instance GQLScalar UUID where
  parseValue (String x) = case fromText x of
    Nothing -> Left "Not a valid UUID"
    Just uuidParsed -> Right uuidParsed
  parseValue _ = Left "Not a valid UUID input type"
  serialize = String . toText

instance GQLType Plant where
  type KIND Plant = OBJECT
  description _ = Just "A plant"

data Truck = Truck {
  truckId :: UUID
  , vehicleId :: Text
} deriving (Show, Generic, Data, Typeable)

instance GQLType Truck where
  type KIND Truck = OBJECT
  description _ = Just "A truck"

newtype TypedSelectionSet a = TypedSelectionSet ValidSelection
-- data SQLLink a = SQLLink Text -- link to the name of the column

keyToColumn key = show key

qualifySelection :: ValidSelection -> TypedSelectionSet Plant
qualifySelection = TypedSelectionSet

data SQLColumnMetaData = SQLColumnMetaData {
  sqlName :: Text
  , camelCaseName :: Text
}

data SQLStructure encoder decoder = SQLStructure {
  selectPart :: [Text],
  fromPart :: Text,
  joinList :: [Text],
  statementEncoder :: encoder,
  statementDecoder :: Decoders.Composite decoder
}

class SQLInstance a where
  getAllColumns :: TypedSelectionSet a -> [Text]
  getTable :: TypedSelectionSet a -> Text
  getRecord :: TypedSelectionSet a -> Text

instance SQLInstance Plant where
  getAllColumns _ = ["plantId", "truckList"] 
  getTable _ = "plant"
  getRecord (TypedSelectionSet _) = "plant"

selectionSetToSQL :: SelectionSet -> Text
selectionSetToSQL inputSelSet = undefined
  where test = qualifySelection . snd <$> inputSelSet 

joinSelectionSet :: (Key, ValidSelection) -> String
joinSelectionSet (keyVakue, _) = keyToColumn keyVakue


plantSelectionToSQL :: TypedSelectionSet Plant -> SQLStructure () Plant
plantSelectionToSQL (TypedSelectionSet a) = undefined

plantFieldEffect :: (Key, ValidSelection) -> SQLStructure () Plant -> SQLStructure () Plant
plantFieldEffect ("truckList", selection) struct = undefined
plantFieldEffect normalField struct = struct
