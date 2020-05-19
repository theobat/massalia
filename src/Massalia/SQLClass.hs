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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    SQLSelect(toSelectQuery),
    SQLColumn(toColumnListAndDecoder),
    gsqlColumns,
    SQLDefault(getDefault),
    SQLSelectOptions(..),
    SQLColumnConfig(..)
  )
where

import Massalia.QueryFormat
  (
    QueryFormat,
    FromText(fromText),
    SQLEncoder(sqlEncode, ignoreInGenericInstance),
    SQLDecoder(sqlDecode),
    BinaryQuery,
    DefaultParamEncoder,
    param,
    (§),
    inParens
  )
import Massalia.UtilsGQL (Paginated)
import Massalia.SQLUtils (insertIntoWrapper, selectWrapper, rowsAssembler)
import Massalia.GenericUtils (GTypeName(gtypename), GSelectors(selectors))
import Data.String (String, IsString(fromString))
import Hasql.Decoders (Composite)
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
import Massalia.SelectionTree(MassaliaTree)
import qualified Massalia.SelectionTree as MassaliaTree
import qualified Hasql.Decoders as Decoders 
import Protolude hiding (intercalate)
import Massalia.Filter (
    GQLScalarFilter(GQLScalarFilter),
    filterFieldToMabeContent,
    PostgresRange
  )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Massalia.SQLSelectStruct (QueryAndDecoder(..))

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
  default sqlColumns :: forall queryFormat a. (IsString queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
  sqlColumns = gsqlColumns @queryFormat @a

gsqlColumns :: forall queryFormat a. (IsString queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
gsqlColumns = snakeTypename
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

instance (SQLEncoder Text a, GValues (K1 i a) Text) =>
  GValues (K1 i a) Text where
  goToValues (K1 val) = [(sqlEncode val)]
instance (SQLEncoder BinaryQuery a, GValues (K1 i a) BinaryQuery) =>
  GValues (K1 i a) BinaryQuery where
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
    SQLEncoder queryFormat filterType
  ) => GSQLFilter (K1 i (
    Maybe filterType)
  ) queryFormat where
  gtoQueryFormatFilter options (K1 val) = result
    where
      result
        | ignoreInGenericInstance @queryFormat @filterType = []
        | otherwise = case val of
        Nothing -> []
        Just a -> [sqlEncode a]

instance (
    IsString queryFormat, KnownSymbol a,
    SQLEncoder queryFormat [b],
    SQLEncoder queryFormat b,
    SQLEncoder queryFormat c,
    SQLEncoder queryFormat d,
    PostgresRange d
  ) => SQLEncoder queryFormat (GQLScalarFilter a b c d) where
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
-- instance (SQLEncoder BinaryQuery a, GValues (K1 i a) BinaryQuery) =>
--   GValues (K1 i a) BinaryQuery where
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














---------------------- SQLSelect test

-- | A simple default value for the given 'nodeType' type.
class SQLDefault nodeType where
  getDefault :: nodeType

data SQLSelectOptions = SQLSelectOptions {
  ok :: Bool
}
data SQLColumnConfig = SQLColumnConfig {
  columnPrefix :: Text
}

class SQLSelect queryFormat filterType nodeType | nodeType -> filterType where
  toSelectQuery :: (MassaliaTree selectionType, SQLColumn queryFormat filterType nodeType) =>
    Maybe SQLSelectOptions ->
    -- | The selection set (in the form of a 'Tree' interface)
    selectionType ->
    -- | The node's filter type
    Paginated filterType ->
    -- | A query for this node with all its decoder
    QueryAndDecoder queryFormat nodeType

-- | This is the way to get a select query out of a select tree and a filter
class SQLColumn queryFormat filterType domainType | domainType -> filterType where
  toColumnListAndDecoder ::
    (MassaliaTree selectionType) =>
    SQLColumnConfig ->
    -- | The selection set (in the form of a 'Tree' interface).
    selectionType ->
    -- | The node's filter type.
    Maybe filterType ->
    -- | A queryFormatted list of columns and a Hasql decoder.
    ([queryFormat], Composite domainType)
  default toColumnListAndDecoder :: (
      SQLDefault domainType,
      MassaliaTree selectionType,
      GSQLColumn queryFormat filterType (Rep domainType),
      Generic domainType
    ) =>
    SQLColumnConfig -> selectionType -> Maybe filterType -> ([queryFormat], Composite domainType)
  toColumnListAndDecoder opt selectionVal filterVal = (selList, to <$> gdeco)
    where
      (selList, gdeco) = gtoColumnListAndDecoder @queryFormat opt selectionVal filterVal defaultVal
      defaultVal = from $ getDefault @domainType

class GSQLColumn queryFormat filterType (rep :: * -> *) where
  gtoColumnListAndDecoder ::
    (MassaliaTree selectionType) =>
    SQLColumnConfig ->
    -- | The selection set (in the form of a 'Tree' interface)
    selectionType ->
    -- | The node's filter type
    Maybe filterType ->
    -- | The node's default value
    (rep domainType) ->
    -- | A queryFormatted list of columns and a Hasql decoder.
    ([queryFormat], Composite (rep domainType))

-- | A priori this instance should not exist
-- instance GSQLSelect queryFormat filterType U1 where
--   gtoSelectQuery _ _ _ U1 = panic "whaat ?"

-- | Use the Monad instance of the composite hasql type
-- It's where the magic happens
instance (GSQLColumn queryFormat filterType a, GSQLColumn queryFormat filterType b) =>
  GSQLColumn queryFormat filterType (a :*: b) where
  gtoColumnListAndDecoder opt selectionVal filterVal (a :*: b) = result
    where
      result = (structA <> structB, compoCombined)
      (structA, compoA) = gtoColumnListAndDecoder opt selectionVal filterVal a
      (structB, compoB) = gtoColumnListAndDecoder opt selectionVal filterVal b
      compoCombined = do
        ca <- compoA
        cb <- compoB
        pure (ca :*: cb)

instance (GSQLColumn queryFormat filterType a) => GSQLColumn queryFormat filterType (M1 D c a) where
  gtoColumnListAndDecoder opt selectionVal filterVal (M1 x) = second (M1 <$>) res
    where res = gtoColumnListAndDecoder opt selectionVal filterVal x
instance (GSQLColumn queryFormat filterType a) => GSQLColumn queryFormat filterType (M1 C c a) where
  gtoColumnListAndDecoder opt selectionVal filterVal (M1 x) = second (M1 <$>) res
    where res = gtoColumnListAndDecoder opt selectionVal filterVal x

-- | This is where we encouter a leaf, if the leaf is in the tree (asked) we query it
-- otherwise we simply return the default provided value.
instance (
    FromText queryFormat, IsString queryFormat, Selector s,
    SQLDecoder queryFormat filterType t
  ) =>
  GSQLColumn queryFormat filterType (M1 S s (K1 R t)) where
  gtoColumnListAndDecoder opt selection filterValue defaultValue = case lookupRes of
    Nothing -> (mempty, pure defaultValue)
    Just childTree -> result
      where
        result = bimap columnInstanceWrapper decoderWrapper decoded
        decoderWrapper = ((M1 . K1) <$>) . (Decoders.field . Decoders.nonNullable)
        columnInstanceWrapper = pure . (columnPrefixVal &)
        decoded = sqlDecode @queryFormat @filterType @t filterValue childTree
        columnPrefixVal = fromText $ columnPrefix opt
    where
      lookupRes = MassaliaTree.lookupChildren key selection
      key = (fromString $ selName (undefined :: M1 S s (K1 R t) ()))
