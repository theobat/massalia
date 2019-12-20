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
                                                ( SelectionRec(..), ValidSelection, SelectionSet, Selection(..), Arguments )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key, Position(Position) )
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Data.Maybe (fromJust)
import           Data.Bifunctor                     ( first )
import           Control.Monad.IO.Class (liftIO)
import Data.UUID
import qualified Debug.Trace as Debug
import qualified Data.Text as T
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified LibAPI as Lib
import LibAPI (SQLStructure, defaultSQLStruct, globalStructureToQuery)

selectionGen :: Arguments -> SelectionRec -> Selection Arguments SelectionRec
selectionGen a = Selection a (Position 0 0) Nothing

orgSelectionSet :: SelectionSet
orgSelectionSet =
  [ ("orgId"  , selectionGen [] SelectionField)
  , ("plantList", selectionGen [] $ SelectionSet plantSelTest)
  ]

plantSelTest :: SelectionSet
plantSelTest =
  [ ("plantId"  , selectionGen [] SelectionField)
  , ("truckList", selectionGen [] $ SelectionSet truckSelTest)
  ]

truckSelTest :: SelectionSet
truckSelTest =
  [ ("truckId"  , selectionGen [] SelectionField)
  ]

testSelection :: SelectionSet
testSelection = orgSelectionSet

someFunc :: IO ()
someFunc = do
  print (globalStructureToQuery (plantGlobalSelect plantSelTest))
  -- rawConnection <- Connection.acquire connectionSettings
  -- connection <- case rawConnection of
  --   Left e -> (error $ show e)
  --   Right goodCo -> pure goodCo
  -- result <- Session.run (Session.statement () selectSum) connection
  -- queryTest <- Session.run (Session.statement () queryCrashTest) connection
  -- print (show <$> queryTest)
  -- print (plantId <$> queryTest)

  -- res <- testAPI (connection, testSelection) (customQuery queryString)
  -- print res
  -- where
  --   connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"
  --   queryString = selectionSetToString "testOrg" "organizationList" testSelection

customQuery :: Text -> GQLRequest
customQuery queryString = GQLRequest { operationName = Nothing
                         , query         = queryString
                         , variables     = Nothing
                         }

selectionSetToString :: Text -> Text -> SelectionSet -> Text
selectionSetToString queryName nodeName selset =
  "query " <> queryName <> " { " <> nodeName <> resolveList selset <> " }"
  where
    resolveList partialSelset = (foldr resolved " { " partialSelset) <> " } "
    resolved (key, Selection{ selectionRec = selectionRec }) queryRes = case selectionRec of
          SelectionField -> queryRes <> key
          (SelectionSet deeperSel) -> queryRes <> resolveList deeperSel


data PlantListFilter = PlantListFilter {
  idFilter :: UUID
}


data Query m = Query
  { plantList :: () -> m [Plant]
  } deriving (Generic, GQLType)

-- data DeityArgs = DeityArgs
--   { name      :: Text -- Required Argument
--   , mythology :: Maybe Text -- Optional Argument
--   } deriving (Generic)

-- fakeDBPlant :: () -> IO (Either String Plant)
-- fakeDBPlant _ = return $ Right $ Plant "" 1 ["ok"]

resolvePlantList :: (Connection.Connection, SelectionSet)  -> () -> IORes m [Plant]
resolvePlantList (connection, fieldList) _ = undefined -- liftEither $ testInIO
  -- where
  --   testInIO =
  --     fmap (first show) test
  --   test = Session.run (Session.statement Nothing (plantToSQL 1 fieldList Nothing)) connection

-- getPlantList :: IORes e [Plant]

rootResolver
  :: (Connection.Connection, SelectionSet) -> GQLRootResolver IO () Query Undefined Undefined
rootResolver config =
  GQLRootResolver
    { queryResolver = Query {plantList = resolvePlantList config }
    , mutationResolver =  Undefined
    , subscriptionResolver = Undefined
    }

testAPI :: (Connection.Connection, SelectionSet) -> GQLRequest -> IO GQLResponse
testAPI config = interpreter $ rootResolver config

-- | This should either select array_agg if it's a list or select simple row if not
-- | for now let's ignore filters
plantToSQL :: Int -> SelectionSet -> Maybe Lib.PlantFilter -> Statement () [Plant]
plantToSQL depth selSet filter = undefined

--------------------
-- TODO --  SelectionSet -> SQLStructure () [Plant] ########### qOKOKOKOKOKOKOKOKOKOKOK
--------------------

plantGlobalSelect :: SelectionSet -> SQLStructure () Plant
plantGlobalSelect = foldr plantSelection initialValue
  where initialValue = defaultSQLStruct {
    Lib.wrapFunctionList = ["row"],
    Lib.fromPart="plant",
    Lib.statementEncoder = Encoders.noParams,
    Lib.statementDecoder = (\val -> plantDefault {plantId = val}) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
  }

plantSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure () Plant -> SQLStructure () Plant
plantSelection ("plantId", _) = Lib.scalar "plant.id" (\p -> (\val -> p { plantId = val })) Decoders.uuid
plantSelection ("truckList", selection) = Lib.compo truckGlobalSelect ("truck_plant.plant_id=plant.id", "plant.id") ((\p -> (\val -> p { truckList = val }))) selection

truckGlobalSelect :: SelectionSet -> SQLStructure () Truck
truckGlobalSelect = foldr truckSelection initialValue
  where initialValue = defaultSQLStruct {
    Lib.wrapFunctionList = ["row"],
    Lib.fromPart="truck",
    Lib.joinList=["JOIN truck_plant ON truck_plant.truck_id=truck.id"],
    Lib.statementEncoder = Encoders.noParams,
    Lib.statementDecoder = (\val -> truckDefault {truckId = val}) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
  }

truckSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure () Truck -> SQLStructure () Truck
truckSelection ("truckId", _) = Lib.scalar "truck.id" (\p -> (\val -> p { truckId = val })) Decoders.uuid

-- selectSum :: [Text] -> Statement () Plant
-- selectSum fieldList = Statement (encodeUtf8 sql) encoder decoder True where
--   compute = decodePlant fieldList
--   sql = "select row("
--       <> (T.intercalate "," $ selectPart compute)
--       <> ") as plant_data"
--       <> " "
--       <>   filtersSQL
--       <>   " "
--       <>   groupbySQL
--   filtersSQL = "from plant join truck_plant ON truck_plant.plant_id=plant.id JOIN truck ON truck.id=truck_plant.truck_id"
--   groupbySQL = "group by plant.id limit 1"
--   encoder = Encoders.noParams
--   compositeDecoder = Decoders.column $ Decoders.nonNullable $ Decoders.composite $ statementDecoder compute
--   decoder = Decoders.singleRow compositeDecoder

-- -- selectPlant :: Statement () Int64
-- -- selectTest = Statement sql encoder decoder True where
-- --   sql = "select 1"
-- --   encoder = Encoders.noParams
-- --   decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))

-- decodePlant :: [Text] -> SQLStructure () Plant
-- decodePlant fields = testFields
--   where
--     defaultSQLStruct = SQLStructure { selectPart = [], statementDecoder = pure plantDefault}
--     testFields = foldr updatePlant defaultSQLStruct fields
--     -- decoderCompositeCol = Decoders.nullable Decoders.text
    -- decoderComposite = Decoders.field decoderCompositeCol
    -- decoderCompositeTuple =
    --   updatePlantId <$> (fromJust <$> decoderComposite)

-- plantFieldEffect :: Text -> Plant -> Plant
-- plantFieldEffect "plantId" plant = plant { plantId = plantId }

plantDefault = Plant {}
truckDefault = Truck {}

-- -- updatePlant :: Text -> Plant -> Plant
-- updatePlant
--   :: Text -> SQLStructure () Plant -> SQLStructure () Plant
-- updatePlant "plantId" sqlST = sqlST {
--   selectPart = selectPart sqlST <> ["plant.id"],
--   statementDecoder = do
--     plant <- statementDecoder sqlST
--     (\val -> plant { plantId = Debug.traceShowId val }) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
-- }
-- updatePlant "testOK" sqlST = sqlST {
--   selectPart = selectPart sqlST <> ["'okokok'"],
--   statementDecoder = do
--     plant <- statementDecoder sqlST
--     (\val -> plant { testOK = val }) <$> (Decoders.field (Decoders.nonNullable Decoders.text))
-- }
-- updatePlant "truckList" sqlST = sqlST {
--   selectPart = selectPart sqlST <> ["array_agg(row(truck.id))"],
--   statementDecoder = do
--     plant <- statementDecoder sqlST
--     (\val -> plant { truckList = val }) <$> (
--       Decoders.field $ Decoders.nonNullable (Decoders.listArray $ Decoders.nonNullable (Decoders.composite $ statementDecoder truckUpdater)) )
-- }
--   where truckUpdater = updateTruck "truckId" SQLStructure { selectPart = [], statementDecoder = pure truckDefault }
-- updatePlant anything _ = error ("field " ++ T.unpack anything ++ " is not supported yet")

-- updateTruck :: Text -> SQLStructure () Truck -> SQLStructure () Truck
-- updateTruck "truckId" sqlST = sqlST
--   { statementDecoder =
--     do
--       truck <- statementDecoder sqlST
--       (\val -> truck { truckId = Debug.traceShowId val })
--         <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
--   }

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

