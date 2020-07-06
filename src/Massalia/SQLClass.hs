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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Massalia.SQLClass
-- Description : A module to define SQL-related typeclasses
-- with generic deriving capabilities. Along with QueryFormat,
-- it's the main module of massalia.
--
-- In the entire module (as in the entire project), the @qf@ type variable stands for 
-- query format.
--
module Massalia.SQLClass
  ( SQLName(..),
    SQLColumns(..),
    SQLValues(..),
    DBContext(..),
    WithQueryOption(..),
    SQLFilter(toQueryFormatFilter),
    SQLFilterField (filterStruct),
    SubSelectConstraint,
    basicEntityQuery,
    basicEntityNoFilter,
    basicQueryAndDecoder,
    basicDecodeRecordSubquery,
    basicDecodeListSubquery,
    basicDecodeInnerRecord,
    paginatedFilterToSelectStruct,
    compositeFieldFilter,
    joinFilterFieldSimple,
    joinFilterField,
    SQLSelect(toSelectQuery),
    SQLRecord(toColumnListAndDecoder, fullTopSelection),
    gsqlColumns,
    SQLDefault(getDefault),
    SQLFilterOption(..),
    SQLSelectOption(..),
    SQLRecordConfig(..),
    QueryAndDecoder(..),
    selectConfigFromMapping,
    defaultSelectConfig,
    defaultWithQueryOption,
    InsertQueryOption(..),
    insertValuesQuery,
    decoderFromSQLDefault
  )
where

import qualified Data.Map as Map
import Massalia.QueryFormat
  (
    QueryFormat(sqlEncode),
    (°),
    formattedColName,
    FromText(fromText),
    SQLEncoder,
    SQLDecoder(sqlExpr),
    DecodeTuple (DecodeTuple),
    MassaliaContext(getDecodeOption, setDecodeOption),
    DecodeOption(nameMap, fieldPrefixType),
    DecodeFieldPrefixType(TableName, CompositeField),
    decodeName,
    defaultDecodeTuple,
    inParens,
    joinEq
  )
import Massalia.Filter (
    GQLScalarFilterCore,
    FilterConstraint,
    filterFieldToMaybeContent
  )
import Massalia.UtilsGQL (Paginated)
import Massalia.Utils (unsafeSnakeCaseT)
import Massalia.SQLUtils (insertIntoWrapper, selectWrapper, rowsAssembler)
import Massalia.GenericUtils (proxySelName, GTypeName(gtypename), GSelectors(selectors))
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
    filterConcat,
  )
import qualified Massalia.UtilsGQL as Paginated
import Massalia.UtilsGQL (OrderingBy(..), OrderByWay(..))

type SubSelectConstraint qf contextT subNodeT = (
    QueryFormat qf,
    SQLSelect contextT subNodeT,
    SQLRecord contextT subNodeT
  )

-- | This is a utility function to facilitate the writing
-- of SQLDecode for inner records.
basicDecodeInnerRecord :: 
  forall contextT nodeT treeNode qf.
  (
    SQLRecord contextT nodeT,
    MassaliaTree treeNode,
    MassaliaContext contextT,
    SQLRecord contextT nodeT,
    QueryFormat qf
  ) =>
  contextT ->
  treeNode ->
  (Text -> qf, DecodeTuple nodeT)
basicDecodeInnerRecord context selection = result
  where
    result = (colListQFThunk, defaultDecodeTuple $ Decoders.composite decoderVal)
    colListQFThunk name = "(CASE WHEN "
      <> (fromText compositeName)
      <> " IS NULL THEN null ELSE row("
      <> intercalate "," (colListThunk compositeName)
      <> ") END)"
      where
        parentName = decodeName currentDecodeOpt name
        compositeName = unsafeSnakeCaseT (parentName ° (getName selection))
    (colListThunk, decoderVal) = toColumnListAndDecoder selection updatedContext
    updatedContext = setDecodeOption @contextT (currentDecodeOpt{fieldPrefixType=CompositeField}) context
    currentDecodeOpt = fromMaybe mempty $ getDecodeOption context

-- | A utility function to build a list subquery within an existing query.
-- It's meant to be used in SQLDecode.
basicDecodeRecordSubquery :: 
  forall qf parentContextT childrenContextT childrenT treeNode.
  (
    SQLSelect childrenContextT childrenT,
    MassaliaTree treeNode,
    SQLRecord childrenContextT childrenT,
    QueryFormat qf, MassaliaContext parentContextT
  ) =>
  -- | The context switch between parent and child.
  (parentContextT -> childrenContextT) ->
  -- | The function which performs the join between parents and child.
  (qf -> SelectStruct qf) ->
  -- | The given parent context.
  parentContextT ->
  -- | The selection tree.
  treeNode ->
  (Text -> qf, DecodeTuple childrenT)
basicDecodeRecordSubquery contextSwitch joinFn parentContext selection = (recordSubquery, newDecoder)
  where
    newDecoder = compositeToDecoderTuple $ decoder subQueryRaw
    recordSubquery name = selectStructToRecordSubquery $ query subQueryRaw <> joinFn decodedName
      where
        decodedName = fromText $ decodeName decodeOpt name
        decodeOpt = fromMaybe mempty $ getDecodeOption parentContext
    subQueryRaw = basicDecodeSubquery contextSwitch selection parentContext

-- | A utility function to build a list subquery within an existing query.
-- It's meant to be used in SQLDecode.
basicDecodeListSubquery ::
  forall qf parentContextT childrenContextT childrenT treeNode.
  (
    SQLSelect childrenContextT childrenT,
    MassaliaTree treeNode, SQLRecord childrenContextT childrenT,
    QueryFormat qf, MassaliaContext parentContextT
  ) =>
  -- | The context switch between parent and child.
  (parentContextT -> childrenContextT) ->
  -- | The function which performs the join between parents and child.
  (qf -> SelectStruct qf) ->
  -- | The given parent context.
  parentContextT ->
  -- | The selection tree.
  treeNode ->
  (Text -> qf, DecodeTuple [childrenT])
basicDecodeListSubquery contextSwitch joinFn parentContext selection = (listSubquery, newDecoder)
  where
    newDecoder = compositeToListDecoderTuple $ decoder subQueryRaw
    listSubquery name = selectStructToListSubquery $ query subQueryRaw <> joinFn decodedName
      where
        decodedName = fromText $ decodeName decodeOpt name
        decodeOpt = fromMaybe mempty $ getDecodeOption parentContext
    subQueryRaw = basicDecodeSubquery contextSwitch selection parentContext

basicDecodeSubquery ::
  forall qf parentContextT childrenContextT childNodeT treeNode.
  (
    SQLSelect childrenContextT childNodeT,
    QueryFormat qf,
    MassaliaTree treeNode,
    SQLRecord childrenContextT childNodeT
  ) =>
  (parentContextT -> childrenContextT) ->
  treeNode ->
  parentContextT ->
  QueryAndDecoder qf childNodeT
basicDecodeSubquery contextSwitcher selection parentContextT = subQueryRaw
  where
    subQueryRaw = toSelectQuery selection childrenContext
    childrenContext = contextSwitcher parentContextT

-- | A simple building block for the 'toSelectQuery' function in the SQLSelect instance.
-- It provides the equivalent of 
-- @
--  SELECT /*... the list of values extracted from the selection tree*/
--  FROM /* the given table name */
-- @
basicQueryAndDecoder :: (
    MassaliaContext contextT,
    MassaliaTree selectionType,
    SQLRecord contextT nodeType,
    QueryFormat qf
  ) =>
  -- | The sql table name and pagination filter mapping from the context.
  (contextT -> (Text, SelectStruct qf)) ->
  -- | The selection set (in the form of a 'Tree' interface)
  selectionType ->
  -- | The node's context type
  contextT ->
  -- | A query for this node with all its decoder
  QueryAndDecoder qf nodeType
basicQueryAndDecoder contextTransformer selection context = QueryAndDecoder {
        query=queryWithColumnList,
        decoder=decoderVal
      }
    where
      queryWithColumnList = rawQuery <> mempty{
          _select = colListThunk instanceName
        }
      (colListThunk, decoderVal) = toColumnListAndDecoder selection context
      (instanceName, rawQuery) = contextTransformer context

-- | This is a simple FROM "tablename" query bit with no filtering at all
basicEntityNoFilter :: (
    MassaliaContext a,
    QueryFormat qf
  ) =>
  -- | An __unescaped__ SQL tablename/alias.
  Text ->
  -- | A query context
  a ->
  -- | The tablename and its filtered/roled/customized select query.
  (Text, SelectStruct qf)
basicEntityNoFilter tablename context = (tablename, base)
  where
    base = mempty {_from = Just ("\"" <> fromText realTableName <> "\"")}
    realTableName = decodeName (fromMaybe mempty decodeOption) tablename
    decodeOption = getDecodeOption context

-- | This is a simple FROM "tablename" query bit along with
--  a potential filter
basicEntityQuery :: (
    MassaliaContext a,
    QueryFormat qf, SQLFilter b
  ) =>
  -- | An __unescaped__ SQL tablename/alias.
  Text ->
  -- | A query context to filter function.
  (a -> Maybe b) ->
  -- | A query context
  a ->
  -- | The tablename and its filtered/roled/customized select query.
  (Text, SelectStruct qf)
basicEntityQuery tablename filterAcc context = (tablename, withFilter base)
  where
    withFilter a = case (toQueryFormatFilter opt =<< filterAcc context) of
      Nothing -> a
      Just b -> a <> b 
    base = mempty {_from = Just ("\"" <> fromText realTableName <> "\"")}
    opt = Just defaultFilterOption{
        filterTableName=realTableName,
        filterFieldMap=nameMap $ fromMaybe mempty $ getDecodeOption context
      }
    realTableName = decodeName (fromMaybe mempty decodeOption) tablename
    decodeOption = getDecodeOption context

-- | Transforms a paginated filter into a SelectStruct
-- (which you can add to your existing one through (<>)).
paginatedFilterToSelectStruct :: (
    QueryFormat qf,
    SQLFilter filterT
  ) =>
  Maybe SQLFilterOption -> Paginated filterT -> (SelectStruct qf)
paginatedFilterToSelectStruct filterOption filterValue = result
  where
    result = offsetLimitQy <> filteredAggregate
    filteredAggregate = foldr' filterAggregator mempty filteredList
    filterAggregator filtA existingFilt = case (_where existingFilt, _where filtA) of
      (_, Nothing) -> existingFilt <> filtA
      (Just _, Just _) -> existingFilt <> mempty{_where=Just " OR ("} <> mempty{_where= Just ")"}
      (Nothing, Just _) -> existingFilt <> mempty{_where=Just "("} <> filtA <> mempty{_where= Just ")"}
    filteredList = catMaybes $ (toQueryFormatFilter filterOption) <$> Paginated.filtered filterValue
    offsetLimitQy = mempty {
        _offsetLimit = Just $ offsetLimitFn
      }
    offsetLimitFn = (sqlEncode <$> Paginated.offset filterValue, sqlEncode $ fromMaybe 10000 $ Paginated.first filterValue)

joinFilterFieldSimple :: (QueryFormat qf, SQLFilter record) => (Text, Text, Text) -> Maybe SQLFilterOption -> p -> record -> Maybe (SelectStruct qf)
joinFilterFieldSimple (tableName, tableCol, parentCol) = joinFilterField joinRes
  where
    joinRes a = (joinEq tableName tableCol a parentCol, a)

joinFilterField :: (
  QueryFormat qf,
  SQLFilter record,
  FromText qf) =>
  -- | The name of the table to join to.
  (Text -> (Text, Text)) ->
  Maybe SQLFilterOption -> p -> record -> Maybe (SelectStruct qf)
joinFilterField joinFunction opts _ val = partialRes
      where
        partialRes = (Just (mempty{
          _join = [
              fromText $ joiningRes
            ]
          } <> recordPart))
        (joiningRes, joiningName) = joinFunction fatherTable
        fatherTable = fromMaybe "" (filterTableName <$> opts)
        recordPart = fromMaybe mempty (toQueryFormatFilter updatedOpt val)
        updatedOpt = updateFiltOpt <$> opts
        updateFiltOpt a = a{filterTableName = joiningName}

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
  sqlName :: Text
  sqlTable :: Text
  -- | The default SQLName is merely a snake case alternative of the 
  default sqlName :: (Generic a, GTypeName (Rep a)) => Text
  sqlName = fromString snakeTypename
    where
      snakeTypename = simpleSnakeCase typename
      typename = gtypename @(Rep a)
  -- | The default sqlTable is snake case + dropping the "input" word
  default sqlTable :: (Generic a, GTypeName (Rep a)) => Text
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
  sqlColumns :: (QueryFormat queryFormat) => [queryFormat]
  default sqlColumns :: forall queryFormat. (QueryFormat queryFormat, Generic a, GSelectors (Rep a)) => [queryFormat]
  sqlColumns = gsqlColumns @a

gsqlColumns :: forall a. (Generic a, GSelectors (Rep a)) => forall queryFormat. (QueryFormat queryFormat) => [queryFormat]
gsqlColumns = snakeTypename
  where
    snakeTypename = (fromString . simpleSnakeCase . fst) <$> listValue
    listValue = selectors @(Rep a) 

columnList :: forall a queryFormat. (QueryFormat queryFormat, SQLColumns a) => queryFormat
columnList = fromString $ toCSVInParens (sqlColumns @a)

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
class SQLValues a where
  toSQLValues :: (QueryFormat queryFormat) => a -> [queryFormat]
  default toSQLValues :: (QueryFormat queryFormat, Generic a, GValues (Rep a)) => a -> [queryFormat]
  toSQLValues value = goToValues (from value)

class GValues f where
  goToValues :: forall queryFormat a. (QueryFormat queryFormat) => f a -> [queryFormat]

instance GValues U1 where
  goToValues U1 = mempty
instance (GValues a, GValues b) => GValues (a :*: b) where
  goToValues (a :*: b) = goToValues a <> goToValues b

instance (GValues a) => GValues (M1 D c a) where
  goToValues (M1 x) = goToValues x
instance (GValues a) => GValues (M1 S c a) where
  goToValues (M1 x) = goToValues x
instance (GValues a) => GValues (M1 C c a) where
  goToValues (M1 x) = goToValues x

instance (SQLEncoder a, GValues (K1 i a)) =>
  GValues (K1 i a) where
  goToValues (K1 val) = [(sqlEncode val)]

----------------------------------------------------------------------------
---------------------------- SQL Filter
----------------------------------------------------------------------------

data SQLFilterOption = SQLFilterOption {
  -- | The table name unescaped, just like everywhere in the lib.
  filterTableName :: Text,
  -- | The mapping should use the snake case versions
  filterFieldMap :: Map Text Text,
  filterFieldType :: DecodeFieldPrefixType
}
getFilterFieldName :: Text -> Maybe SQLFilterOption -> Text
getFilterFieldName scFieldName opt = fromMaybe scFieldName $ join look
  where look = (Map.lookup scFieldName . filterFieldMap) <$> opt

defaultFilterOption :: SQLFilterOption
defaultFilterOption = SQLFilterOption{
    filterTableName = "",
    filterFieldMap=mempty,
    filterFieldType=TableName
  }

class SQLFilter record where
  toQueryFormatFilter :: forall qf. (QueryFormat qf) => Maybe SQLFilterOption -> record -> Maybe (SelectStruct qf)
  default toQueryFormatFilter :: forall qf. (
      QueryFormat qf, Generic record,
      GSQLFilter (Rep record)
    ) =>
    Maybe SQLFilterOption -> record -> Maybe (SelectStruct qf)
  toQueryFormatFilter options value = case gtoQueryFormatFilter @(Rep record) @qf options (from value) of
    [] -> Nothing
    nonEmptyList -> Just (filterConcat mempty nonEmptyList)

class GSQLFilter f where
  gtoQueryFormatFilter :: forall qf a. (QueryFormat qf) => Maybe SQLFilterOption -> f a -> [SelectStruct qf]

instance GSQLFilter U1 where
  gtoQueryFormatFilter _ U1 = mempty
instance (GSQLFilter a, GSQLFilter b) =>
  GSQLFilter (a :*: b) where
  gtoQueryFormatFilter options (a :*: b) = withStatementA <> withStatementB
    where
      withStatementA = gtoQueryFormatFilter @a options a
      withStatementB = gtoQueryFormatFilter @b options b

instance (GSQLFilter a) => GSQLFilter (M1 D c a) where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
instance (GSQLFilter a) => GSQLFilter (M1 C c a) where
  gtoQueryFormatFilter options (M1 x) = gtoQueryFormatFilter options x
  
instance (
    Selector s,
    SQLFilterField filterType
  ) => GSQLFilter (M1 S s (K1 R filterType)) where
  gtoQueryFormatFilter options (M1 (K1 val)) = maybeToList result
    where
      result = filterStruct options selector val 
      selector = fromString $ simpleSnakeCase $ selName (proxySelName :: M1 S s (K1 R t) ())

class SQLFilterField fieldType where
  filterStruct ::
    (QueryFormat qf) =>
    -- | A potential dictionary for rewriting names
    Maybe SQLFilterOption ->
    -- | selector __snakecase__ name 
    Text ->
    -- | field value
    fieldType ->
    -- | the resulting query to concatenate
    Maybe (SelectStruct qf)

-- Basic filters instances
instance (
    FilterConstraint b c d
  ) => SQLFilterField (GQLScalarFilterCore b c d) where
  filterStruct options selectorName val = result <$> filterBitResult
    where
      result whClause = mempty {
          _where = Just $ "("<> whClause <> ")"
        }
      filterBitResult = filterFieldToMaybeContent prefixedField (Just val)
      prefixedField = formattedColName (fromMaybe TableName $ (filterFieldType <$> options)) prefix actualFieldName
      prefix = (fromText . filterTableName) <$> options
      actualFieldName = fromText $ getFilterFieldName selectorName options

instance SQLFilter () where
  toQueryFormatFilter _ _ = Nothing
instance SQLFilterField () where
  filterStruct _ _ _ = Nothing
instance (SQLFilter a) => SQLFilter (Maybe a) where
  toQueryFormatFilter options val = toQueryFormatFilter options =<< val
instance (
    SQLFilterField a
  ) => SQLFilterField (Maybe a) where
  filterStruct options selectorName val = (filterStruct options selectorName) =<< val

-- | Paginated filters are for top queries => always
-- (and handled through the SQLRecord machinery)
instance SQLFilterField (Paginated a) where
  filterStruct _ _ _ = Nothing
instance SQLFilterField Bool where
  filterStruct options selectorName val = Just result
    where
      result = mempty {
          _where = Just $ "("<> filterBitResult val <> ")"
        }
      filterBitResult True = formattedField
      filterBitResult False = ("NOT " <> formattedField)
      formattedField = formattedColName (fromMaybe TableName $ (filterFieldType <$> options)) prefix actualFieldName
      prefix = (fromText . filterTableName) <$> options
      actualFieldName = fromText $ getFilterFieldName selectorName options

instance (SQLFilter a) => SQLFilter (Paginated a) where
  toQueryFormatFilter opt val = Just $ paginatedFilterToSelectStruct opt val

-- | This is just the asc/desc part of the equation.
-- But it assumes the ordering yields something to which you can concatenate asc/desc.
instance (SQLFilterField a, Foldable contT) =>
  SQLFilterField (contT (OrderingBy a)) where
  filterStruct maybeOpt selection input = if null input then Nothing else result
    where
      result = foldl' (\existingSQL y -> combine existingSQL $ fullName y) (Just mempty) input
      combine a Nothing = a
      combine Nothing b  = b
      combine (Just a) (Just b)  = Just (a <> b)
      fullName (OrderingBy wayVal colVal) = withWayOrderBy newStruct
        where
          withWayOrderBy (Just selStruct) = Just selStruct{
              _orderBy=fmap (<> ordTrans wayVal) (_orderBy selStruct)
            }
          withWayOrderBy Nothing = Nothing
          newStruct = filterStruct maybeOpt selection colVal
      ordTrans DESC = " DESC" 
      ordTrans ASC = " ASC" 

-- | A function to use when implementing SQLFilterField for composite types.
compositeFieldFilter :: (
    SQLFilter filterT,
    QueryFormat qf
  ) =>
  Maybe SQLFilterOption ->
  Text ->
  filterT ->
  Maybe (SelectStruct qf)
compositeFieldFilter options selectorName val = result
    where
      result = toQueryFormatFilter (transformOptions <$> options) val
      transformOptions baseOpt = baseOpt{
        filterTableName=formattedFieldVal,
        filterFieldType=CompositeField
        }
      formattedFieldVal = formattedColName (fromMaybe TableName $ (filterFieldType <$> options)) prefix actualFieldName
      prefix = (fromText . filterTableName) <$> options
      actualFieldName = fromText $ getFilterFieldName selectorName options

----------------------------------------------------------------------------
---------------------------- DBContext queries
----------------------------------------------------------------------------

-- | A type to specify which type of query should be generated in
-- 'Default' is an insert query statement with values wrapped in
--  a 
data WithQueryOption = WithQueryOption {
    -- | The default options in case none is found in withShape map
    defaultShape :: InsertQueryOption,
    -- | 'sqlName' indexed options.
    withShape :: Map Text InsertQueryOption
  }
defaultWithQueryOption :: WithQueryOption
defaultWithQueryOption = WithQueryOption {
  defaultShape = Insert True False,
  withShape = mempty
}
data InsertQueryOption = PureSelect | Insert {
    hasReturningStar :: Bool,
    hasOnConflictId :: Bool
  } 

class DBContext record where
  toWithQuery :: (QueryFormat queryFormat) => Maybe WithQueryOption -> record -> queryFormat
  default toWithQuery :: (
      QueryFormat queryFormat, Generic record,
      GDBContext (Rep record)
    ) =>
    Maybe WithQueryOption -> record -> queryFormat
  toWithQuery options value = "WITH " <> (intercalate "," genericRes)
    where genericRes = gtoWithQuery options (from value)

class GDBContext f where
  gtoWithQuery :: (QueryFormat queryFormat) => Maybe WithQueryOption -> f a -> [queryFormat]

instance GDBContext U1 where
  gtoWithQuery _ U1 = mempty
instance (GDBContext a, GDBContext b) =>
  GDBContext (a :*: b) where
  gtoWithQuery options (a :*: b) = withStatement a <> withStatement b
    where
      withStatement input = gtoWithQuery options input

instance (GDBContext a) => GDBContext (M1 D c a) where
  gtoWithQuery options (M1 x) = gtoWithQuery options x
instance (GDBContext a) => GDBContext (M1 S c a) where
  gtoWithQuery options (M1 x) = gtoWithQuery options x
instance (GDBContext a) => GDBContext (M1 C c a) where
  gtoWithQuery options (M1 x) = gtoWithQuery options x

instance (
    Foldable collection,
    Functor collection,
    Eq (collection a),
    Monoid (collection a),
    SQLName a,
    SQLColumns a,
    SQLValues a
  ) =>
  GDBContext (K1 i (collection a)) where
  gtoWithQuery options (K1 val) = result
    where
      result
        | val == mempty = []
        | otherwise = pure $ qfSQLName <> " AS " <> (inParens selectInstance)
      qfSQLName = fromText $ sqlName @a
      optionVal = fromMaybe defaultWithQueryOption options
      insertOpt = Map.lookup (sqlName @a) (withShape optionVal)
      selectInstance = case (fromMaybe (defaultShape optionVal) insertOpt) of
        PureSelect -> selectValuesQuery Nothing val
        res@Insert{} -> insertValuesQuery res val

insertValuesQuery :: 
  forall collection recordType queryFormat. (
    Foldable collection,
    Functor collection,
    FromText queryFormat,
    QueryFormat queryFormat,
    SQLValues recordType,
    SQLColumns recordType,
    SQLName recordType
  ) =>
  InsertQueryOption -> collection recordType -> queryFormat
insertValuesQuery opt recordCollection =
  insertHeader <> "\n" <> selectBody <> "\n" <> suffix
  where
    suffix = case opt of
      Insert True True -> onConflictUpdateExcluded @recordType <> "\n RETURNING *"
      Insert True False -> "RETURNING *"
      Insert False True -> onConflictUpdateExcluded @recordType
      _ -> ""
    qfSQLTable = fromText $ sqlTable @recordType
    insertHeader = insertIntoWrapper qfSQLTable columnListVal
    selectBody = selectValuesQuery (Just columnListVal) recordCollection
    columnListVal = columnList @recordType

-- | This assumes the entity has an id column in SQL.
-- The idea is that any dedupe should happen **beforehand** not in the
--  on conflict resolver (because here we assume the dupe id has been
-- retrieved and solved).
onConflictUpdateExcluded ::
  forall recordT qf. (SQLColumns recordT, QueryFormat qf) =>
  qf
onConflictUpdateExcluded = core
  where
    core = " ON CONFLICT (id) DO UPDATE SET " <> foldedSet
    foldedSet = intercalate "," (exclSet <$> colList)
    exclSet vn = vn <> "= EXCLUDED." <> vn
    colList = sqlColumns @recordT

selectValuesQuery ::
  forall collection recordType queryFormat. (
    Foldable collection,
    Functor collection,
    QueryFormat queryFormat,
    SQLValues recordType,
    SQLColumns recordType,
    SQLName recordType
  ) =>
  (Maybe queryFormat) -> collection recordType -> queryFormat
selectValuesQuery (maybeCols) recordCollection = result
  where
    result = selectWrapper name cols assembledRows
    name = fromText $ sqlName @recordType
    assembledRows = ("VALUES " <>) $ rowsAssembler " " listOfRows
    listOfRows = (inParens . intercalate ",") <$> listOfListOfValues
    listOfListOfValues = toSQLValues <$> recordCollection
    cols = fromMaybe (columnList @recordType) maybeCols



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
selectConfigFromMapping :: [(Text, Text)] -> SQLSelectOption
selectConfigFromMapping input = defaultSelectConfig {
    selectDecodeOption = mempty{nameMap = Map.fromList input}
  }
defaultSelectConfig :: SQLSelectOption
defaultSelectConfig = SQLSelectOption {
  selectDecodeOption = mempty
}

-- | This is the class to get an SQL select query out of a selection set ('MassaliaTree'),
-- and a paginated filter.
class SQLSelect contextT nodeType where
  -- | Takes a selection set of GQL fields along and a context type.
  -- Gives an SQL query with a Hasql decoder ('QueryAndDecoder').
  toSelectQuery :: forall queryFormat selectionType. (QueryFormat queryFormat, MassaliaTree selectionType) =>
    -- | The selection set (in the form of a 'Tree' interface)
    selectionType ->
    -- | The node's context. It Has to respect the MassaliaContext interface/class.
    contextT ->
    -- | A query for this node with all its decoder
    QueryAndDecoder queryFormat nodeType

-- | This class is the way to get the list of selection along with the way to decode them in haskell
--  for a given type and its filter.
-- It also provides a way to access all the selectors of a type (fullTopSelection).
-- 
class SQLRecord contextT nodeT where
  -- | Given a top name, selects every leaf selector in this type.
  fullTopSelection :: Text -> MassaliaNode
  default fullTopSelection :: (
      GSQLRecord contextT (Rep nodeT),
      Generic nodeT
    ) => Text -> MassaliaNode
  fullTopSelection name = gfullTopSelection @contextT @(Rep nodeT) name
  toColumnListAndDecoder ::
    forall selectionType queryFormat. (QueryFormat queryFormat, MassaliaTree selectionType) =>
    -- | The selection set (in the form of a 'Tree' interface).
    selectionType ->
    -- | The node's cont. It Has to respect the MassaliaContext interface/class.
    contextT ->
    -- | A queryFormatted list of columns and a Hasql decoder.
    (Text -> [queryFormat], Composite nodeT)
  default toColumnListAndDecoder ::
    forall selectionType queryFormat. (
      QueryFormat queryFormat,
      SQLDefault nodeT,
      MassaliaTree selectionType,
      GSQLRecord contextT (Rep nodeT),
      Generic nodeT
    ) =>
    selectionType ->
    contextT ->
    (Text -> [queryFormat], Composite nodeT)
  toColumnListAndDecoder selectionVal context = fmap (to <$>) $ gtoColumnListAndDecoder selectionVal context defaultVal
    where
      -- (colListThunk, gdeco) = gtoColumnListAndDecoder selectionVal context defaultVal
      defaultVal = from $ getDefault @nodeT

class GSQLRecord contextT (rep :: * -> *) where
  gfullTopSelection :: Text -> MassaliaNode
  gtoColumnListAndDecoder ::
    forall selectionType nodeT queryFormat. (QueryFormat queryFormat, MassaliaTree selectionType) =>
    -- | The selection set (in the form of a 'Tree' interface)
    selectionType ->
    -- | The node's context type
    contextT ->
    -- | The node's default value
    (rep nodeT) ->
    -- | A queryFormatted list of columns (parametrized by tablename) and a Hasql decoder.
    (Text -> [queryFormat], Composite (rep nodeT))

-- | Use the Monad instance of the composite hasql type
-- It's where the magic happens
instance (GSQLRecord contextT a, GSQLRecord contextT b) =>
  GSQLRecord contextT (a :*: b) where
  gfullTopSelection name = resParent
    where
      resParent = (resA <> resB)
      resA = gfullTopSelection @contextT @a name :: MassaliaNode
      resB = gfullTopSelection @contextT @b name :: MassaliaNode
  gtoColumnListAndDecoder selectionVal context (a :*: b) = result
    where
      result = (combineCols, compoCombined)
      combineCols input = structA input <> structB input
      (structA, compoA) = gtoColumnListAndDecoder selectionVal context a
      (structB, compoB) = gtoColumnListAndDecoder selectionVal context b
      compoCombined = do
        ca <- compoA
        cb <- compoB
        pure (ca :*: cb)

instance (GSQLRecord contextT a) => GSQLRecord contextT (M1 D c a) where
  gfullTopSelection name = gfullTopSelection @contextT @a name
  gtoColumnListAndDecoder selectionVal context (M1 x) = second (M1 <$>) res
    where res = gtoColumnListAndDecoder selectionVal context x
instance (GSQLRecord contextT a) => GSQLRecord contextT (M1 C c a) where
  gfullTopSelection name = gfullTopSelection @contextT @a name
  gtoColumnListAndDecoder selectionVal context (M1 x) = second (M1 <$>) res
    where res = gtoColumnListAndDecoder selectionVal context x

-- | This is where we encouter a leaf, if the leaf is in the tree (asked) we query it
-- otherwise we simply return the default provided value.
instance (
    Selector s,
    MassaliaContext contextT,
    SQLDecoder contextT t
  ) =>
  GSQLRecord contextT (M1 S s (K1 R t)) where
  gfullTopSelection name = leaf name `over` leaf key
    where
      key = (fromString $ selName (proxySelName :: M1 S s (K1 R t) ()))
  gtoColumnListAndDecoder selection contextT defaultValue = case lookupRes of
    Nothing -> (const mempty, pure defaultValue)
    Just childTree -> result
      where
        result = bimap fnWrapper decoderWrapper decoded
        fnWrapper fn qfName = [fn qfName]
        decoderWrapper = ((M1 . K1) <$>) . Decoders.field
        decoded = (columnFn, nullability decValue)
        (columnFn, DecodeTuple decValue nullability) = decodeRes
        decodeRes = sqlExpr @contextT @t contextT childTree
    where
      lookupRes = MassaliaTree.lookupChildren key selection
      key = (fromString $ selName (proxySelName :: M1 S s (K1 R t) ()))

decoderFromSQLDefault :: (SQLDefault a) => DecodeTuple a
decoderFromSQLDefault = defaultDecodeTuple $ Decoders.composite (pure getDefault)
