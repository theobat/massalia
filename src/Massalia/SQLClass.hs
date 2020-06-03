{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE DataKinds #-}

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
    SelectConstraint,
    SubSelectConstraint,
    basicEntityQuery,
    basicQueryAndDecoder,
    basicDecodeRecordSubquery,
    basicDecodeListSubquery,
    basicDecodeInnerRecord,
    SQLSelect(toSelectQuery),
    SQLRecord(toColumnListAndDecoder, fullTopSelection),
    gsqlColumns,
    SQLDefault(getDefault),
    SQLSelectOption(..),
    SQLRecordConfig(..),
    QueryAndDecoder(..)
  )
where

import Massalia.QueryFormat
  (
    QueryFormat,
    (°),
    FromText(fromText),
    SQLEncoder(sqlEncode),
    SQLDecoder(sqlDecode),
    DecodeTuple (DecodeTuple),
    BinaryQuery,
    DecodeOption(fieldPrefixType),
    DecodeFieldPrefixType(CompositeField),
    decodeName,
    defaultDecodeTuple,
    inParens
  )
import Massalia.Filter (
    GQLScalarFilterCore,
    FilterConstraint,
    filterFieldToMaybeContent
  )
import Massalia.UtilsGQL (Paginated)
import Massalia.Utils (unsafeSnakeCaseT)
import Massalia.SQLUtils (insertIntoWrapper, selectWrapper, rowsAssembler)
import Massalia.GenericUtils (GTypeName(gtypename), GSelectors(selectors))
import Data.String (IsString(fromString))
import Hasql.Decoders (Composite)
import qualified Data.Text as Text
import GHC.Generics (
    D,
    S,
    C,
    R
  )
import Massalia.Utils (simpleSnakeCase, intercalate, toCSVInParens)
import Massalia.SelectionTree(MassaliaNode, MassaliaTree, getName, leaf, over)
import qualified Massalia.SelectionTree as MassaliaTree
import qualified Hasql.Decoders as Decoders 
import Protolude hiding (intercalate)
import Massalia.SQLSelectStruct (
    SelectStruct(..),
    QueryAndDecoder(..),
    selectStructToRecordSubquery,
    selectStructToListSubquery,
    compositeToListDecoderTuple,
    compositeToDecoderTuple,
  )
import qualified Massalia.UtilsGQL as Paginated

type SubSelectConstraint qf filterT childrenT = (
    QueryFormat qf,
    SelectConstraint qf filterT,
    SQLSelect qf filterT childrenT,
    SQLRecord qf filterT childrenT
  )

-- | This is a utility function to facilitate the writing
-- of SQLDecode for inner records.
basicDecodeInnerRecord :: 
  forall qf parentFilterT childrenT treeNode.
  (
    QueryFormat qf,
    forall a. SQLRecord qf a childrenT,
    MassaliaTree treeNode
  ) =>
  Maybe parentFilterT ->
  DecodeOption ->
  treeNode ->
  (Text -> qf, DecodeTuple childrenT)
basicDecodeInnerRecord _ opt selection = result
  where
    result = (colListQFThunk, defaultDecodeTuple $ Decoders.composite decoderVal)
    colListQFThunk name = "(CASE WHEN " <> (fromText compositeName) <> "IS NULL THEN null ELSE row(" <> intercalate "," (colListThunk compositeName) <> ") END)"
      where compositeName = unsafeSnakeCaseT (name ° (getName selection))
    (colListThunk, decoderVal) = toColumnListAndDecoder @qf recordConfig selection filterVal
    recordConfig = SQLRecordConfig {
      recordDecodeOption = opt{fieldPrefixType=CompositeField}
    }
    filterVal = Nothing

-- | A utility function to build a list subquery within an existing query.
-- It's meant to be used in SQLDecode.
basicDecodeRecordSubquery :: 
  forall qf parentFilterT filterT childrenT treeNode.
  (
    SubSelectConstraint qf filterT childrenT,
    MassaliaTree treeNode
  ) =>
  (qf -> SelectStruct qf) ->
  (parentFilterT -> Maybe (Paginated filterT)) ->
  Maybe parentFilterT ->
  DecodeOption ->
  treeNode ->
  (Text -> qf, DecodeTuple childrenT)
basicDecodeRecordSubquery inputFn filterAccessor filterParent opt selection = (listSubquery, newDecoder)
  where
    newDecoder = compositeToDecoderTuple $ decoder subQueryRaw
    listSubquery name = selectStructToRecordSubquery $ query subQueryRaw <> inputFn decodedName
      where decodedName = fromText $ decodeName opt name
    subQueryRaw = basicDecodeSubquery filterAccessor filterParent opt selection

-- | A utility function to build a list subquery within an existing query.
-- It's meant to be used in SQLDecode.
basicDecodeListSubquery ::
  forall qf parentFilterT filterT childrenT treeNode.
  (
    SubSelectConstraint qf filterT childrenT,
    MassaliaTree treeNode
  ) =>
  (qf -> SelectStruct qf) ->
  (parentFilterT -> Maybe (Paginated filterT)) ->
  Maybe parentFilterT ->
  DecodeOption ->
  treeNode ->
  (Text -> qf, DecodeTuple [childrenT])
basicDecodeListSubquery inputFn filterAccessor filterParent opt selection = (listSubquery, newDecoder)
  where
    newDecoder = compositeToListDecoderTuple $ decoder subQueryRaw
    listSubquery name = selectStructToListSubquery $ query subQueryRaw <> inputFn decodedName
      where decodedName = fromText $ decodeName opt name
    subQueryRaw = basicDecodeSubquery filterAccessor filterParent opt selection

basicDecodeSubquery ::
  forall qf parentFilterT filterT childrenT treeNode.
  (
    SubSelectConstraint qf filterT childrenT,
    MassaliaTree treeNode
  ) =>
  (parentFilterT -> Maybe (Paginated filterT)) ->
  Maybe parentFilterT ->
  DecodeOption ->
  treeNode ->
  QueryAndDecoder qf childrenT
basicDecodeSubquery filterAccessor filterParent opt selection = subQueryRaw
  where
    subQueryRaw = toSelectQuery @qf @filterT @childrenT @treeNode selectOption selection filterChild
    selectOption = Just $ SQLSelectOption {selectDecodeOption = opt}
    filterChild = fromMaybe Paginated.defaultPaginated (join $ filterAccessor <$> filterParent)

-- | A simple building block for the 'toSelectQuery' function in the SQLSelect instance.
-- It provides the equivalent of 
-- @
--  SELECT /*... the list of values extracted from the seleciton tree*/
--  FROM /* the given table name */
-- @
basicQueryAndDecoder :: (
    SelectConstraint qf filterType,
    MassaliaTree selectionType,
    SQLRecord qf filterType nodeType
  ) =>
  -- | The sql table name (or alias).
  Text ->
  -- | A set of options
  Maybe SQLSelectOption ->
  -- | The selection set (in the form of a 'Tree' interface)
  selectionType ->
  -- | The node's filter type
  Paginated filterType ->
  -- | A query for this node with all its decoder
  QueryAndDecoder qf nodeType
basicQueryAndDecoder instanceName maybeOpt selection filterValue = QueryAndDecoder {
        query=queryWithColumnList,
        decoder=decoderVal
      }
    where
      queryWithColumnList = rawQuery <> mempty{
          _select = colListThunk instanceName
        }
      (colListThunk, decoderVal) = toColumnListAndDecoder recordOpt selection realFilterValue
      recordOpt = defaultRecordConfig {
          recordDecodeOption=fromMaybe mempty (selectDecodeOption <$> maybeOpt)
        }
      realFilterValue = Paginated.filtered filterValue
      rawQuery = basicEntityQuery realInstanceName filterValue
      realInstanceName = fromText instanceName
      

-- | This yields a query with no selection but integrate filter inlining
-- and pagination arguments.
basicEntityQuery :: (
    SelectConstraint queryFormat filterT
  ) =>
  Text -> Paginated filterT -> SelectStruct queryFormat
basicEntityQuery name filtValue = mempty
      { _from = Just ("\"" <> (fromText name) <> "\""),
        _where = toQueryFormatFilter (Just filterOption) =<< Paginated.filtered filtValue,
        _offsetLimit = Just (sqlEncode <$> Paginated.offset filtValue, sqlEncode $ fromMaybe 10000 $ Paginated.first filtValue)
      }
  where filterOption = defaultFilterOption{filterTableName = name}

type SelectConstraint qf filterType = (
    QueryFormat qf,
    SQLEncoder qf Int,
    SQLFilter qf filterType
  )

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
  sqlTable :: (IsString queryFormat, FromText queryFormat) => queryFormat
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
  default sqlColumns :: forall queryFormat. (IsString queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
  sqlColumns = gsqlColumns @queryFormat @a

gsqlColumns :: forall queryFormat a. (IsString queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
gsqlColumns = snakeTypename
  where
    snakeTypename = (fromString . simpleSnakeCase . fst) <$> listValue
    listValue = selectors @(Rep a) 

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

data SQLFilterOption = SQLFilterOption {
  filterTableName :: Text,
  filterFieldName :: Text
}
defaultFilterOption = SQLFilterOption{filterTableName = "", filterFieldName=""}

class SQLFilter queryFormat record where
  toQueryFormatFilter :: Maybe SQLFilterOption -> record -> Maybe queryFormat
  default toQueryFormatFilter :: (
      IsString queryFormat, Monoid queryFormat, Generic record,
      GSQLFilter (Rep record) queryFormat
    ) =>
    Maybe SQLFilterOption -> record -> Maybe queryFormat
  toQueryFormatFilter options value = case filterGroupList of
    [] -> Nothing
    nonEmptyList -> Just (intercalate " AND " nonEmptyList)
    where filterGroupList = gtoQueryFormatFilter options (from value)

class GSQLFilter f queryFormat where
  gtoQueryFormatFilter :: Maybe SQLFilterOption -> f a -> [queryFormat]

instance (Monoid queryFormat) => GSQLFilter U1 queryFormat where
  gtoQueryFormatFilter _ U1 = mempty
instance (Monoid queryFormat, GSQLFilter a queryFormat, GSQLFilter b queryFormat) =>
  GSQLFilter (a :*: b) queryFormat where
  gtoQueryFormatFilter options (a :*: b) = withStatementA <> withStatementB
    where
      withStatementA = gtoQueryFormatFilter options a
      withStatementB = gtoQueryFormatFilter options b

instance (GSQLFilter a queryFormat) => GSQLFilter (M1 D c a) queryFormat where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
instance (GSQLFilter a queryFormat) => GSQLFilter (M1 C c a) queryFormat where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
  
instance (
    Selector s,
    IsString queryFormat,
    SQLFilter queryFormat filterType
  ) => GSQLFilter (M1 S s (K1 R filterType)) queryFormat where
  gtoQueryFormatFilter options (M1 (K1 val)) = maybeToList (toQueryFormatFilter optionWithSelector val)
    where
      optionWithSelector = (\opt -> opt { filterFieldName = selector }) <$> options
      selector = fromString $ simpleSnakeCase $ selName (undefined :: M1 S s (K1 R t) ())

-- Go through generics
instance (
    IsString queryFormat,
    FromText queryFormat,
    FilterConstraint queryFormat b c d
  ) => SQLFilter queryFormat (GQLScalarFilterCore b c d) where
  toQueryFormatFilter options val = filterFieldToMaybeContent prefix actualFieldName (Just val)
    where
      prefix = (fromText . filterTableName) <$> options
      actualFieldName = fromText (fromMaybe mempty (filterFieldName <$> options))

instance (SQLFilter queryFormat a) => SQLFilter queryFormat (Maybe a) where
  toQueryFormatFilter options val = toQueryFormatFilter options =<< val

instance (IsString queryFormat) => SQLFilter queryFormat (Paginated a) where
  toQueryFormatFilter _ _ = Nothing

      
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
      withStatement input = gtoWithQuery options input

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
        Just PureSelect -> selectValuesQuery Nothing val
        _ -> insertValuesQuery () val


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


---------------------- SQLSelect test

-- | A simple default value for the given 'nodeType' type.
class SQLDefault nodeType where
  getDefault :: nodeType

data SQLSelectOption = SQLSelectOption {
  selectDecodeOption :: DecodeOption
}
data SQLRecordConfig = SQLRecordConfig {
  recordDecodeOption :: DecodeOption
}
defaultRecordConfig :: SQLRecordConfig
defaultRecordConfig = SQLRecordConfig {
  recordDecodeOption = mempty
}

class SQLSelect queryFormat filterType nodeType | nodeType -> filterType where
  toSelectQuery :: (MassaliaTree selectionType, SQLRecord queryFormat filterType nodeType) =>
    Maybe SQLSelectOption ->
    -- | The selection set (in the form of a 'Tree' interface)
    selectionType ->
    -- | The node's filter type
    Paginated filterType ->
    -- | A query for this node with all its decoder
    QueryAndDecoder queryFormat nodeType

-- | This is the way to get a select query out of a select tree and a filter
class SQLRecord queryFormat filterType domainType where
  -- | Given a top name, selects every leaf selector in this type.
  fullTopSelection :: Text -> MassaliaNode
  default fullTopSelection :: (
      GSQLRecord queryFormat filterType (Rep domainType),
      Generic domainType
    ) => Text -> MassaliaNode
  fullTopSelection name = gfullTopSelection @queryFormat @filterType @(Rep domainType) name
  toColumnListAndDecoder ::
    (MassaliaTree selectionType) =>
    SQLRecordConfig ->
    -- | The selection set (in the form of a 'Tree' interface).
    selectionType ->
    -- | The node's filter type.
    Maybe filterType ->
    -- | A queryFormatted list of columns and a Hasql decoder.
    (Text -> [queryFormat], Composite domainType)
  default toColumnListAndDecoder :: (
      SQLDefault domainType,
      MassaliaTree selectionType,
      GSQLRecord queryFormat filterType (Rep domainType),
      Generic domainType
    ) =>
    SQLRecordConfig -> selectionType -> Maybe filterType -> (Text -> [queryFormat], Composite domainType)
  toColumnListAndDecoder opt selectionVal filterVal = (colListThunk, to <$> gdeco)
    where
      (colListThunk, gdeco) = gtoColumnListAndDecoder @queryFormat opt selectionVal filterVal defaultVal
      defaultVal = from $ getDefault @domainType

class GSQLRecord queryFormat filterType (rep :: * -> *) where
  gfullTopSelection :: Text -> MassaliaNode
  gtoColumnListAndDecoder ::
    (MassaliaTree selectionType) =>
    SQLRecordConfig ->
    -- | The selection set (in the form of a 'Tree' interface)
    selectionType ->
    -- | The node's filter type
    Maybe filterType ->
    -- | The node's default value
    (rep domainType) ->
    -- | A queryFormatted list of columns (parametrized by tablename) and a Hasql decoder.
    (Text -> [queryFormat], Composite (rep domainType))

-- | Use the Monad instance of the composite hasql type
-- It's where the magic happens
instance (GSQLRecord queryFormat filterType a, GSQLRecord queryFormat filterType b) =>
  GSQLRecord queryFormat filterType (a :*: b) where
  gfullTopSelection name = resParent
    where
      resParent = (resA <> resB)
      resA = gfullTopSelection @queryFormat @filterType @a name :: MassaliaNode
      resB = gfullTopSelection @queryFormat @filterType @b name :: MassaliaNode
  gtoColumnListAndDecoder opt selectionVal filterVal (a :*: b) = result
    where
      result = (combineCols, compoCombined)
      combineCols input = structA input <> structB input
      (structA, compoA) = gtoColumnListAndDecoder opt selectionVal filterVal a
      (structB, compoB) = gtoColumnListAndDecoder opt selectionVal filterVal b
      compoCombined = do
        ca <- compoA
        cb <- compoB
        pure (ca :*: cb)

instance (GSQLRecord queryFormat filterType a) => GSQLRecord queryFormat filterType (M1 D c a) where
  gfullTopSelection name = gfullTopSelection @queryFormat @filterType @a name
  gtoColumnListAndDecoder opt selectionVal filterVal (M1 x) = second (M1 <$>) res
    where res = gtoColumnListAndDecoder opt selectionVal filterVal x
instance (GSQLRecord queryFormat filterType a) => GSQLRecord queryFormat filterType (M1 C c a) where
  gfullTopSelection name = gfullTopSelection @queryFormat @filterType @a name
  gtoColumnListAndDecoder opt selectionVal filterVal (M1 x) = second (M1 <$>) res
    where res = gtoColumnListAndDecoder opt selectionVal filterVal x

-- | This is where we encouter a leaf, if the leaf is in the tree (asked) we query it
-- otherwise we simply return the default provided value.
instance (
    FromText queryFormat, IsString queryFormat, Selector s,
    SQLDecoder queryFormat filterType t
  ) =>
  GSQLRecord queryFormat filterType (M1 S s (K1 R t)) where
  gfullTopSelection name = leaf name `over` leaf key
    where
      key = (fromString $ selName (undefined :: M1 S s (K1 R t) ()))
  gtoColumnListAndDecoder opt selection filterValue defaultValue = case lookupRes of
    Nothing -> (const mempty, pure defaultValue)
    Just childTree -> result
      where
        result = bimap fnWrapper decoderWrapper decoded
        fnWrapper fn qfName = [fn qfName]
        decoderWrapper = ((M1 . K1) <$>) . Decoders.field
        decoded = (columnFn, nullability decValue)
        (columnFn, DecodeTuple decValue nullability) = decodeRes
        decodeRes = sqlDecode @queryFormat @filterType filterValue decOption childTree
        decOption = recordDecodeOption opt
    where
      lookupRes = MassaliaTree.lookupChildren key selection
      key = (fromString $ selName (undefined :: M1 S s (K1 R t) ()))
