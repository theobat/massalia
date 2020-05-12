{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Massalia.SQLClass
-- Description : A module to define SQL-related typeclasses
-- with generic deriving capabilities
module Massalia.SQLClass
  ( SQLName(..),
    SQLColumns(..),
    SQLValues(..),
    DBContext(..),
    WithQueryOption(..),
    SQLFilter(toQueryFormatFilter),
    -- SQLSelect(toSelectQuery),
  )
where

import Massalia.QueryFormat
  (
    FromText(fromText),
    SQLEncoder(sqlEncode),
    SQLDecoder(sqlDecode),
    HasqlSnippet,
    DefaultParamEncoder,
    param,
    (§),
    inParens
  )
import Massalia.SQLUtils (insertIntoWrapper, selectWrapper, rowsAssembler)
import Massalia.GenericUtils (GTypeName(gtypename), GSelectors(selectors))
import Data.String (String, IsString(fromString))
import Massalia.SQLRawSelect (RawSelectStruct)
import qualified Data.Text as Text
import GHC.Generics (
    U1,
    D,
    S,
    C,
    R,
    M1(M1),
    datatypeName,
    K1(K1)
  )
import Massalia.Utils (simpleSnakeCase, intercalate, toCSVInParens)
import Protolude hiding (intercalate)
import Massalia.Filter (
    GQLScalarFilter(GQLScalarFilter),
    filterFieldToMabeContent,
    PostgresRange
  )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Massalia.SQLSelect (SelectStruct(SelectStruct), simpleSelectGroup)

-- | This class represents all the haskell types with a corresponding SQL
-- name. It provides 2 functions: @sqlName@ and @tableName@. @sqlName@ is meant
-- to be used as a a free name for aliases (in CTE or FROM parts) whereas the 
-- @tableName@ is meant to exist as a table in the SQL schema.
-- The default instances take th type name and:
--    - turn it into snake case for @sqlName@
--    - turn it into snake case and remove any @"_input"@ occurence for @sqlName@
-- e.g.
-- @
--     sqlName $ HaskellRecord { name = " abc" } == "haskell_record"
-- @
-- @
--     sqlTable $ RecordInput { name = "some text" } == "record"
-- @
-- 
class SQLName a where
  sqlName :: (IsString queryFormat) => queryFormat
  sqlTable :: (FromText queryFormat) => queryFormat
  -- | The default SQLName is merely a snake case alternative of the 
  default sqlName :: (IsString queryFormat, Generic a, GTypeName (Rep a)) => queryFormat
  sqlName = fromString snakeTypename
    where
      snakeTypename = simpleSnakeCase typename
      typename = gtypename @(Rep a)
  -- | The default sqlTable is snake case + dropping the "input" word
  default sqlTable :: (FromText queryFormat, Generic a, GTypeName (Rep a)) => queryFormat
  sqlTable = fromText snakeTypename
    where
      snakeTypename = Text.replace "_input" "" typename
      typename = fromString $ simpleSnakeCase $ gtypename @(Rep a) :: Text

-- | This class represents all the haskell types with a corresponding list
-- of SQL column associated (mainly haskell records, representing sql tables).
-- e.g.
-- @
--     sqlColumns $ HaskellRecord { name = "some text", identifier = 1234, pID = "really ?"}
-- @
-- Would yield something like @["name", "identifier", "p_id"]@.
-- 
class SQLColumns a where
  sqlColumns :: (IsString queryFormat) => [queryFormat]
  default sqlColumns :: (IsString queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
  sqlColumns = snakeTypename
    where
      snakeTypename = (fromString . simpleSnakeCase . fst) <$> list
      list = selectors @(Rep a)

-- | This class represents all the haskell types with a corresponding 'toSQLValues'
-- function. It's a function meant to encode a haskell record as an SQL comma separated
-- list of parametrized values.
-- e.g.
-- @
--     toSQLValues $ HaskellRecord { name = "some text", identifier = 1234, pID = "really ?"}
-- @
-- Would yield something like @(?, ?, ?)@ with @["some text", 1234, "really ?"]@ parameters.
-- Or @ ("some text", 1234, "really ?") @ if the 'queryFormat' is 'Text'.
-- 
class SQLValues queryFormat a where
  toSQLValues :: a -> [queryFormat]
  default toSQLValues :: (Generic a, GValues (Rep a) queryFormat) => a -> [queryFormat]
  toSQLValues value = goToValues (from value)

class GValues f queryFormat where
  goToValues :: f a -> [queryFormat]

instance (Monoid queryFormat) => GValues U1 queryFormat where
  goToValues U1 = mempty
instance (GValues a queryFormat, GValues b queryFormat) => GValues (a :*: b) queryFormat where
  goToValues (a :*: b) = goToValues a <> goToValues b

instance (GValues a queryFormat) => GValues (M1 D c a) queryFormat where
  goToValues (M1 x) = goToValues x
instance (GValues a queryFormat) => GValues (M1 S c a) queryFormat where
  goToValues (M1 x) = goToValues x
instance (GValues a queryFormat) => GValues (M1 C c a) queryFormat where
  goToValues (M1 x) = goToValues x

instance (SQLEncoder a Text, GValues (K1 i a) Text) =>
  GValues (K1 i a) Text where
  goToValues (K1 val) = [(sqlEncode val)]
instance (SQLEncoder a HasqlSnippet, GValues (K1 i a) HasqlSnippet) =>
  GValues (K1 i a) HasqlSnippet where
  goToValues (K1 val) = [(sqlEncode val)]


----------------------------------------------------------------------------
---------------------------- SQL Filter
----------------------------------------------------------------------------

data SQLFilterOption = TableName Text

class SQLFilter queryFormat record where
  toQueryFormatFilter :: Maybe SQLFilterOption -> record -> queryFormat
  default toQueryFormatFilter :: (
      IsString queryFormat, Monoid queryFormat, Generic record,
      GSQLFilter (Rep record) queryFormat
    ) =>
    Maybe SQLFilterOption -> record -> queryFormat
  toQueryFormatFilter options value = intercalate " AND " filterGroupList
    where filterGroupList = gtoQueryFormatFilter options (from value)

class GSQLFilter f queryFormat where
  gtoQueryFormatFilter :: Maybe SQLFilterOption -> f a -> [queryFormat]

instance (Monoid queryFormat) => GSQLFilter U1 queryFormat where
  gtoQueryFormatFilter _ U1 = mempty
instance (Monoid queryFormat, GSQLFilter a queryFormat, GSQLFilter b queryFormat) =>
  GSQLFilter (a :*: b) queryFormat where
  gtoQueryFormatFilter options (a :*: b) = withStatement a <> withStatement b
    where
      withStatement a = gtoQueryFormatFilter options a

instance (GSQLFilter a queryFormat) => GSQLFilter (M1 D c a) queryFormat where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
instance (GSQLFilter a queryFormat) => GSQLFilter (M1 S c a) queryFormat where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
instance (GSQLFilter a queryFormat) => GSQLFilter (M1 C c a) queryFormat where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
  
instance (
    IsString queryFormat,
    SQLEncoder filterType queryFormat
  ) => GSQLFilter (K1 i (
    Maybe filterType)
  ) queryFormat where
  gtoQueryFormatFilter options (K1 val) = result
    where
      result = case val of
        Nothing -> []
        Just a -> [sqlEncode a]

instance (
    IsString queryFormat, KnownSymbol a,
    SQLEncoder [b] queryFormat,
    SQLEncoder b queryFormat,
    SQLEncoder c queryFormat,
    SQLEncoder d queryFormat,
    PostgresRange d
  ) => SQLEncoder (GQLScalarFilter a b c d) queryFormat where
  sqlEncode val = case filterFieldToMabeContent (Nothing @queryFormat) (Just val) of
    Nothing -> mempty
    Just a -> a
      

----------------------------------------------------------------------------
---------------------------- DBContext queries
----------------------------------------------------------------------------

-- | A type to specify which type of query should be generated in
-- 'Default' is an insert query statement with values wrapped in
--  a 
data WithQueryOption = Default | PureSelect

class DBContext queryFormat record where
  toWithQuery :: Maybe WithQueryOption -> record -> queryFormat
  default toWithQuery :: (
      IsString queryFormat, Monoid queryFormat, Generic record,
      GDBContext (Rep record) queryFormat
    ) =>
    Maybe WithQueryOption -> record -> queryFormat
  toWithQuery options value = "WITH " <> (intercalate "," genericRes)
    where genericRes = gtoWithQuery options (from value)

class GDBContext f queryFormat where
  gtoWithQuery :: Maybe WithQueryOption -> f a -> [queryFormat]

instance (Monoid queryFormat) => GDBContext U1 queryFormat where
  gtoWithQuery _ U1 = mempty
instance (Monoid queryFormat, GDBContext a queryFormat, GDBContext b queryFormat) =>
  GDBContext (a :*: b) queryFormat where
  gtoWithQuery options (a :*: b) = withStatement a <> withStatement b
    where
      withStatement a = gtoWithQuery options a

instance (GDBContext a queryFormat) => GDBContext (M1 D c a) queryFormat where
  gtoWithQuery options (M1 x) = gtoWithQuery options x
instance (GDBContext a queryFormat) => GDBContext (M1 S c a) queryFormat where
  gtoWithQuery options (M1 x) = gtoWithQuery options x
instance (GDBContext a queryFormat) => GDBContext (M1 C c a) queryFormat where
  gtoWithQuery options (M1 x) = gtoWithQuery options x

instance (
    Foldable collection,
    Functor collection,
    Eq (collection a),
    Monoid (collection a),
    Monoid queryFormat,
    FromText queryFormat,
    IsString queryFormat,
    SQLName a,
    SQLColumns a,
    SQLValues queryFormat a
  ) =>
  GDBContext (K1 i (collection a)) queryFormat where
  gtoWithQuery options (K1 val) = result
    where
      result
        | val == mempty = []
        | otherwise = pure $ sqlName @a <> " AS " <> (inParens selectInstance)
      selectInstance = case options of
        Nothing -> insertValuesQuery () val
        Just PureSelect -> selectValuesQuery Nothing val
    -- where values = 
-- instance (SQLEncoder a HasqlSnippet, GValues (K1 i a) HasqlSnippet) =>
--   GValues (K1 i a) HasqlSnippet where
--   goToValues (K1 val) = [(sqlEncode val)]


insertValuesQuery :: 
  forall collection recordType queryFormat. (
    Foldable collection,
    Functor collection,
    FromText queryFormat,
    IsString queryFormat,
    Monoid queryFormat,
    SQLValues queryFormat recordType,
    SQLColumns recordType,
    SQLName recordType
  ) =>
  () -> collection recordType -> queryFormat
insertValuesQuery _ recordCollection = insertHeader <> "\n" <> selectBody
  where
    insertHeader = insertIntoWrapper (sqlTable @recordType) columnListVal
    selectBody = selectValuesQuery (Just columnListVal) recordCollection
    columnListVal = columnList @recordType

selectValuesQuery ::
  forall collection recordType queryFormat. (
    Foldable collection,
    Functor collection,
    IsString queryFormat,
    Monoid queryFormat,
    SQLValues queryFormat recordType,
    SQLColumns recordType,
    SQLName recordType
  ) =>
  (Maybe queryFormat) -> collection recordType -> queryFormat
selectValuesQuery (maybeCols) recordCollection = result
  where
    result = selectWrapper name cols assembledRows
    name = sqlName @recordType
    assembledRows = ("VALUES " <>) $ rowsAssembler " " listOfRows
    listOfRows = (inParens . intercalate ",") <$> listOfListOfValues
    listOfListOfValues = toSQLValues @queryFormat <$> recordCollection
    cols = fromMaybe (columnList @recordType) maybeCols

columnList :: forall a queryFormat. (IsString queryFormat, SQLColumns a) => queryFormat
columnList = fromString $ toCSVInParens (sqlColumns @a)

--------------------- FilterTree














------------------- SQLRelation class

-- | This is to materialize relations between two types
-- e.g.
-- @
--    SQLRelation Plant [Truck] where
--      getRelation "truckList" (plantName, truckName) = addWhereJoinGroup ...
-- @
class SQLRelation sourceType targetType where
  getRelation :: (IsString queryFormat) =>
    Text -> RawSelectStruct queryFormat -> RawSelectStruct queryFormat


---------------------- SQLSelect test

-- | This is the filter's impact, it has no automatic instance
-- class NodeFilter filter nodeType where
--   getQueryPart :: filter -> SelectStruct nodeType queryFormat -> SelectStruct nodeType queryFormat

-- | This is the way to get a select query out of a select tree and a filter
-- class SQLSelect queryFormat nodeType where
--   toSelectQuery ::
--     (Tree selectionType) =>
--     selectionType -> filter -> SelectStruct nodeType queryFormat -> SelectStruct nodeType queryFormat
--   default toSelectQuery :: (
--       IsString queryFormat, Generic nodeType,
--       GSQLSelect queryFormat (Rep nodeType) nodeType,
--       Tree selectionType
--     ) =>
--     selectionType -> filter -> SelectStruct nodeType queryFormat -> SelectStruct nodeType queryFormat
--   toSelectQuery = gtoSelectQuery @queryFormat @(Rep nodeType) @nodeType

-- class GSQLSelect queryFormat (f :: * -> *) nodeType where
--   gtoSelectQuery :: (Tree selectionType) =>
--     selectionType -> filter -> SelectStruct nodeType queryFormat -> SelectStruct nodeType queryFormat

-- instance (GSQLSelect queryFormat a nodeType, GSQLSelect queryFormat b nodeType) =>
--   GSQLSelect queryFormat (a :*: b) nodeType where
--   gtoSelectQuery selectionMap filter input = gtoSelectQuery @queryFormat @a selectionMap filter firstExec
--     where firstExec = gtoSelectQuery @queryFormat @b selectionMap filter input

-- instance (
--     IsString queryFormat,
--     FromText queryFormat,
--     Selector s,
--     SetField nodeType t,
--     SQLDecoder t,
--     Generic (f nodeType)
--   ) => GSQLSelect queryFormat (M1 S s (K1 R (Rec0 t b))) nodeType where
--   gtoSelectQuery selection filter input = case lookupChildren keyname selection of
--     Nothing -> input
--     Just childrenNode -> res
--     where
--       keyname = fromString $ selName (undefined :: M1 S s (K1 R t) ())
--       (queryValue, decoder) = sqlDecode @t selection
--       fieldSetter = setField @nodeType @t keyname
--       res = simpleSelectGroup (queryValue "") fieldSetter decoder input

-- instance (SQLDecoder a) => GSQLSelect queryFormat (K1 i a) where
--   gtoSelectQuery selection _ = undefined --  simpleSelectGroup sqlDecode 

