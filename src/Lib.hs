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
import Data.Functor.Contravariant ((>$<))

import LibAPI (
  SQLStructure, defaultSQLStruct, globalStructureToQuery,
  globalStructureToListStatement, PlantFilter)

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
  print ()
  rawConnection <- Connection.acquire connectionSettings
  connection <- case rawConnection of
    Left e -> (error $ show e)
    Right goodCo -> pure goodCo
  print (globalStructureToQuery limitedOrg)
  -- result <- Session.run (Session.statement () statement) connection
  -- print (fmap (organizationId <$>) result)
  -- queryTest <- Session.run (Session.statement () queryCrashTest) connection
  -- print (show <$> queryTest)
  -- print (plantId <$> queryTest)

  -- res <- testAPI (connection, testSelection) (customQuery queryString)
  -- print res
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "beton_direct_web"
    queryString = selectionSetToString "testOrg" "organizationList" testSelection
    org = organizationGlobalSelect (Nothing, Nothing) orgSelectionSet
    limitedOrg = org{ Lib.limit = 1 }
    statement = globalStructureToListStatement limitedOrg


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


-- no filter
type OrganizationSelectFilter = (Maybe PlantFilter, Maybe Bool)
organizationGlobalSelect :: OrganizationSelectFilter -> SelectionSet -> SQLStructure OrganizationSelectFilter Organization
organizationGlobalSelect filter = foldr organizationSelection filteredValue
  where
    paramUUID = Encoders.param (Encoders.nonNullable Encoders.uuid)
    test = Encoders.noParams <> paramUUID
    initialValue = defaultSQLStruct {
      Lib.wrapFunctionList = ["row"],
      Lib.fromPart="organization",
      Lib.statementEncoder = Lib.plantIdFilter >$< paramUUID,
      Lib.statementDecoder = (\val -> organizationDefault {organizationId = val}) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
    }
    filteredValue = case fst filter of
      Nothing -> initialValue
      Just (Lib.PlantFilter _ plantIdFilter) -> initialValue {
        Lib.joinList = [
          "JOIN plant ON plant.id=organization.plant_id AND plant.id=?"
        ]
      }

organizationSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure a Organization -> SQLStructure a Organization
organizationSelection ("orgId", _) = Lib.scalar "organization.id" (\o val -> o { organizationId = val }) Decoders.uuid
organizationSelection ("plantList", selection) = rs
  where rs = Lib.compo plantGlobalSelect ("plant.organization_id=organization.id", "organization.id") ((\p val -> p { orgPlantList = val })) selection
  

plantGlobalSelect :: SelectionSet -> SQLStructure () Plant
plantGlobalSelect = foldr plantSelection initialValue
  where initialValue = defaultSQLStruct {
    Lib.wrapFunctionList = ["row"],
    Lib.fromPart="plant",
    Lib.statementDecoder = (\val -> plantDefault {plantId = val}) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
  }

plantSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure a Plant -> SQLStructure a Plant
plantSelection ("plantId", _) = Lib.scalar "plant.id" (\p -> (\val -> p { plantId = val })) Decoders.uuid
plantSelection ("truckList", selection) = rs
  where rs = Lib.compo truckGlobalSelect ("truck_plant.plant_id=plant.id", "plant.id") (\p val -> p { truckList = val }) selection

truckGlobalSelect :: SelectionSet -> SQLStructure () Truck
truckGlobalSelect = foldr truckSelection initialValue
  where initialValue = defaultSQLStruct {
    Lib.wrapFunctionList = ["row"],
    Lib.fromPart="truck",
    Lib.joinList=["JOIN truck_plant ON truck_plant.truck_id=truck.id"],
    Lib.statementDecoder = (\val -> truckDefault {truckId = val}) <$> (Decoders.field (Decoders.nonNullable Decoders.uuid))
  }

truckSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure a Truck -> SQLStructure a Truck
truckSelection ("truckId", _) = Lib.scalar "truck.id" (\p val -> p{ truckId = val }) Decoders.uuid

organizationDefault = Organization {}
plantDefault = Plant {}
truckDefault = Truck {}

workGlobalSelect :: SelectionSet -> SQLStructure () Work
workGlobalSelect = foldr workSelection initialValue
  where initialValue = defaultSQLStruct {
    Lib.wrapFunctionList = ["row"],
    Lib.fromPart="work",
    Lib.statementEncoder = Encoders.noParams
  }

workSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure () Work -> SQLStructure () Work
workSelection ("workId", _) = Lib.scalar "work.id" (\p val -> p{ workId = val }) Decoders.uuid

workScheduleGlobalSelect :: SelectionSet -> SQLStructure () WorkSchedule
workScheduleGlobalSelect = foldr workScheduleSelection initialValue
  where initialValue = defaultSQLStruct {
    Lib.wrapFunctionList = ["row"],
    Lib.fromPart="workSchedule",
    Lib.statementEncoder = Encoders.noParams
  }

workScheduleSelection :: (Key, Selection Arguments SelectionRec) -> SQLStructure () WorkSchedule -> SQLStructure () WorkSchedule
workScheduleSelection ("pouringDate", _) = Lib.scalar "work_schedule.pouring_date" (\p val -> p{ pouringDate = val }) Decoders.text


data Organization = Organization {
  organizationId :: UUID,
  orgPlantList :: [Plant]
}

data Work = Work {
  workId :: UUID,
  workScheduleList :: [WorkSchedule]
}

data WorkSchedule = WorkSchedule {
  workScheduleId :: UUID,
  pouringDate :: Text,
  createdAt :: Text
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


data WorkFilter = WorkFilter {
  requireWorkSchedule :: Maybe Bool,
  orderByPouringDate :: Maybe Bool
}