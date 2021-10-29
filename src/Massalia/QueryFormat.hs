{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.QueryFormat
-- Description : A module to define a query format __interface__ for SQL queries. In Hasql, we use dynamic queries
--  because we don't know in advance what parameters will be provided or not by the graphql query. The query format
--  provided is made for communicating efficiently with Postgres so it has no instance of 'Show' or 'Eq' which can be
--  a problem for testing and for debugging. This module defined a typeclass to abstract whatever Type is used for the
--  query formatting.
module Massalia.QueryFormat
  ( QueryFormat (sqlEncode),
    formattedColName,
    SQLEncoder (wrapEncoding, textEncode, binaryEncode, fieldRename),
    SQLDecoder (sqlDecoder, sqlExpr),
    fmapList,
    fmapVector,
    DecodeOption (..),
    DecodeFieldPrefixType (..),
    DecodeTuple (DecodeTuple),
    MassaliaContext (getDecodeOption, setDecodeOption),
    defaultDecodeTuple,
    refineDecoder,
    FromText (fromText),
    IsString (fromString),
    DefaultParamEncoder,
    BinaryQuery,
    TextQuery,
    DedupeBinaryQuery(..),
    (§),
    (°),
    joinEq,
    simpleEq,
    Snippet.param,
    inSingleQuote,
    inParens,
    decodeName,
    decodeNameInContext,
    collectionTextEncode,
    collectionBinaryEncode,
    BinaryEncodeMethod(..),
    PostgresRange (postgresRangeName),
    decodeTupleToHasql,
  )
where

import qualified Data.Map as Map
import Data.String (String)
import qualified Data.String as String (IsString (fromString))
import Data.Text (pack, replace, unpack)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID hiding (fromString, fromText)
import Data.Vector (Vector)
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet, sql)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder ())
import Massalia.SelectionTree (MassaliaTree (getName))
import qualified Data.Sequence as Seq
import Massalia.Utils
  ( Day,
    EmailAddress,
    Inclusivity (EE, EI, IE, II),
    LocalTime,
    Scientific,
    SimpleRange (end, inclusivity, start),
    UTCTime,
    ZonedTime,
    ZonedTimeEq (ZonedTimeEq),
    emailToText,
    emailValidateText,
    unsafeSnakeCaseT,
  )
import Protolude hiding (replace)
import Data.Coerce (coerce)

-- | The text query format is the canonical representation for debugging
-- and printing queries.
type TextQuery = Text

-- | The binary query format is the internal query representation in 'Hasql'.
-- It's called 'Snippet' and it's defined in "Hasql.DynamicStatements.Snippet".
-- It enables the representation of a query alongside it's parametrized values.
-- It has no instance of show, which explains the presence of its 'TextQuery' counterpart.
type BinaryQuery = Snippet

-- | This is a format where we dedupe common binary parts.
-- The price to pay compared to a simple BinaryQuery is "quite" significant, since 
-- every single semigroup concatenation will iterate on the textQueryPartList and lookup
-- on the binary queryPartMap (O (n log n)) and then 
-- For instance, we can do something like:
--
-- @
--  binaryQueryResult $ "SELECT * FROM ok WHERE id=" <> sqlEncode "'%T%'" <> " AND name ilike " <> sqlEncode "'%T%'" 
-- @
-- And get an SQL query with a single parameter (since both sqlEncode values are identical)
--
-- @
--  SELECT * FROM ok WHERE id=$1 AND name ilike name=$1;
-- @
data DedupeBinaryQuery = DedupeBinaryQuery {
  -- | The text to binary comparison.
  queryPartMap :: Map TextQuery BinaryQuery,
  -- | The list of query bits in text format.
  -- To understand the use-case for this field, look at the Semigroup instance.
  textQueryPartList :: Seq TextQuery,
  -- | The final query to send to the database (without any parametrized duplicates)
  binaryQueryResult :: BinaryQuery
}
instance IsString DedupeBinaryQuery where
  fromString st = DedupeBinaryQuery {
      queryPartMap = mempty,
      textQueryPartList = pure (String.fromString st),
      binaryQueryResult = mempty
    }

instance Semigroup DedupeBinaryQuery where
  (<>) !a b = a {
      queryPartMap = binaryRef, -- left biased union
      textQueryPartList = textQueryPartList a Seq.>< textQueryPartList b,
      binaryQueryResult = binaryQueryResult a <> foldl' lookupAdd mempty (textQueryPartList b)
    }
    where
      lookupAdd prevList !textVal = case Map.lookup textVal binaryRef of
        Nothing -> prevList <> fromText textVal
        Just v -> prevList <> v
      binaryRef = Map.union (queryPartMap a) (queryPartMap b)

instance Monoid DedupeBinaryQuery where
  mempty = DedupeBinaryQuery {
      queryPartMap = mempty,
      textQueryPartList = mempty,
      binaryQueryResult = mempty
    }

-- | The ° operator is taking a table name (or an sql alias) and a field name (or a field alias) and
-- yields their escaped combination.
--
-- Examples:
--
-- >>> "foo" ° "bar"
-- "\"foo\".\"bar\""
(°) :: (Semigroup a, IsString a) => a -> a -> a
(°) a b = "\"" <> a <> "\".\"" <> b <> "\""
{-# INLINE (°) #-}

joinEq :: (Semigroup a, IsString a) => a -> a -> a -> a -> a
joinEq tableA fieldA tableB fieldB = "JOIN \"" <> tableA <> "\" ON " <> joinCondition
  where
    joinCondition = simpleEq tableA fieldA tableB fieldB
{-# INLINE joinEq #-}

simpleEq :: (Semigroup a, IsString a) => a -> a -> a -> a -> a
simpleEq tableA fieldA tableB fieldB = (tableA ° fieldA) <> " = " <> (tableB ° fieldB)
{-# INLINE simpleEq #-}

(§) :: (IsString content, Monoid content) => content -> content -> content
(§) a b = a <> "," <> b

class
  ( FromText queryFormat,
    IsString queryFormat,
    Monoid queryFormat
  ) =>
  QueryFormat queryFormat
  where
  sqlEncode :: (SQLEncoder a) => a -> queryFormat

instance QueryFormat TextQuery where
  sqlEncode = textEncode
  {-# INLINABLE sqlEncode #-}

instance QueryFormat String where
  sqlEncode = unpack . textEncode
  {-# INLINABLE sqlEncode #-}

instance QueryFormat ByteString where
  sqlEncode = encodeUtf8 . textEncode
  {-# INLINABLE sqlEncode #-}

instance QueryFormat DedupeBinaryQuery where
  sqlEncode = binaryDedupeEncode
  {-# INLINABLE sqlEncode #-}

instance QueryFormat BinaryQuery where
  sqlEncode = binaryEncode
  {-# INLINABLE sqlEncode #-}

class FromText content where
  fromText :: Text -> content

instance FromText Text where
  fromText = identity
  {-# INLINABLE fromText #-}

instance FromText String where
  fromText = unpack
  {-# INLINABLE fromText #-}

instance FromText ByteString where
  fromText = encodeUtf8
  {-# INLINABLE fromText #-}

instance FromText Snippet where
  fromText = sql . encodeUtf8
  {-# INLINABLE fromText #-}

instance FromText DedupeBinaryQuery where
  fromText txt = DedupeBinaryQuery {
      queryPartMap = mempty,
      textQueryPartList = pure txt,
      binaryQueryResult = fromText txt
    }

class SQLEncoder dataT where
  wrapEncoding :: (QueryFormat qf) => qf -> qf
  wrapEncoding = identity
  textEncode :: dataT -> TextQuery
  default textEncode :: (Show dataT) => dataT -> TextQuery
  textEncode = inSingleQuote . pack . show
  binaryEncode :: dataT -> BinaryQuery
  binaryDedupeEncode :: dataT -> DedupeBinaryQuery
  binaryDedupeEncode a = DedupeBinaryQuery {
      queryPartMap = Map.singleton txtVal bVal,
      textQueryPartList = pure txtVal,
      binaryQueryResult = mempty
    }
    where
      txtVal = textEncode a
      bVal = binaryEncode a
  fieldRename :: Text -> Text
  fieldRename = identity

voidMessage :: Text
voidMessage = " from void cannot happen because Void has no inhabitant and sqlEncode expect Void -> queryFormat"

instance SQLEncoder Void where
  textEncode = panic $ "text" <> voidMessage
  binaryEncode = panic $ "binary" <> voidMessage

instance (Show a, SQLEncoder a) => SQLEncoder (Maybe a) where
  textEncode = (wrapEncoding @a) . maybe "null" textEncode
  binaryEncode = (wrapEncoding @a) . maybe "null" binaryEncode

instance SQLEncoder UUID where
  wrapEncoding a = "" <> a <> "::uuid"
  binaryEncode = Snippet.param

instance SQLEncoder [UUID] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param
instance SQLEncoder (Vector UUID) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder Day where
  binaryEncode = Snippet.param

instance SQLEncoder [Day] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param
instance SQLEncoder (Vector Day) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder LocalTime where
  binaryEncode = Snippet.param

instance SQLEncoder [LocalTime] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param
instance SQLEncoder (Vector LocalTime) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder Text where
  textEncode = inSingleQuote
  binaryEncode = Snippet.param

instance SQLEncoder [Text] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param
instance SQLEncoder (Vector Text) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

boolEncode :: IsString p => Bool -> p
boolEncode True = "true::bool"
boolEncode False = "false::bool"

instance SQLEncoder Bool where
  textEncode = boolEncode
  binaryEncode = boolEncode

instance SQLEncoder [Bool] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . fmap (boolEncode @Text)
instance SQLEncoder (Vector Bool) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . fmap (boolEncode @Text)

instance SQLEncoder Int64 where
  wrapEncoding a = "" <> a <> "::bigint"
  textEncode = pack . show
  binaryEncode = Snippet.param

instance SQLEncoder [Int64] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder (Vector Int64) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder Int where
  wrapEncoding a = "" <> a <> "::int"
  textEncode = pack . show
  binaryEncode = Snippet.param . (fromIntegral @Int @Int64)

instance SQLEncoder [Int] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . (fromIntegral @Int @Int64 <$>)
instance SQLEncoder (Vector Int) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . (fromIntegral @Int @Int64 <$>)

instance SQLEncoder ZonedTime where
  binaryEncode = Snippet.param . zonedTimeToUTC

instance SQLEncoder [ZonedTime] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . (zonedTimeToUTC <$>)
instance SQLEncoder (Vector ZonedTime) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . (zonedTimeToUTC <$>)

deriving via ZonedTime instance SQLEncoder ZonedTimeEq
deriving via [ZonedTime] instance SQLEncoder [ZonedTimeEq]
deriving via (Vector ZonedTime) instance SQLEncoder (Vector ZonedTimeEq)

instance SQLEncoder UTCTime where
  binaryEncode = Snippet.param

instance SQLEncoder [UTCTime] where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder (Vector UTCTime) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param

instance SQLEncoder EmailAddress where
  textEncode = inSingleQuote . emailToText
  binaryEncode = Snippet.param . emailToText

instance SQLEncoder (Vector EmailAddress) where
  textEncode = collectionTextEncode
  binaryEncode = Snippet.param . fmap (show @_ @Text)

inSingleQuote :: (Semigroup a, IsString a) => a -> a
inSingleQuote a = "'" <> a <> "'"

inParens :: (Semigroup a, IsString a) => a -> a
inParens !a = "(" <> a <> ")"


data BinaryEncodeMethod a
  -- | Encodes the collection through a list of text, replaces the @'@ with @"@ but does no casting.
  = TextCollection
  -- | Encodes the collection through a list of text-based rows, replaces the @'@ with @"@
  -- and casts the result with the underlying datatype concatenated with the array symbol of postgres (@[]@).
  | TextCollectionCasted

-- | Binary encodes a list of encodable types.
-- Has almost the same as @collectionTextEncode@, but removes the @ ' @.
--
-- @
--  collectionBinaryEncode (["a", "b"] :: [Text])
-- -- "{ b,a }" (but binary has no instance for show)
-- @
--
collectionBinaryEncode ::
  forall collection dataT.
  (Foldable collection, SQLEncoder dataT) =>
  BinaryEncodeMethod dataT ->
  collection dataT ->
  Snippet
collectionBinaryEncode method collection = case method of
  TextCollection -> Snippet.param (replace "'" "\"" . textEncode <$> toList collection)
  TextCollectionCasted -> (<> "[]") $ wrapEncoding @dataT $ Snippet.param (replace "'" "\"" . textEncode <$> toList collection)
  -- CustomFunction translater -> Snippet.param (translater <$> toList collection)

-- | Text encodes a list of encodable types.
-- 
-- >>> collectionTextEncode (["a", "b"] :: [Text])
-- "'{ b,a }'"
--
collectionTextEncode ::
  (Foldable collection, SQLEncoder dataT, Show dataT) =>
  collection dataT ->
  Text
collectionTextEncode collection = wrapCollection assembled
  where
    assembled = foldr assemblerFun "" collection
    assemblerFun :: (SQLEncoder b, Show b) => b -> Text -> Text
    assemblerFun !val "" = rawEncode val
    assemblerFun !val !currentEncoded = currentEncoded <> "," <> rawEncode val
    rawEncode val = replace "'" "" $ textEncode val
    wrapCollection a = "'{ " <> a <> " }'"

formattedColName :: (QueryFormat qf) => DecodeFieldPrefixType -> Maybe qf -> qf -> qf
formattedColName _ Nothing colName = "\"" <> colName <> "\""
formattedColName CompositeField (Just prefixName) colName = "(" <> prefixName <> ").\"" <> colName <> "\""
formattedColName TableName (Just prefixName) colName = prefixName ° colName

------------------------- Decoder stuff

defaultDecodeExpr ::
  (QueryFormat queryFormat, MassaliaTree selection, MassaliaContext contextT) =>
  contextT ->
  selection ->
  (Text -> queryFormat)
defaultDecodeExpr context input = firstRes
  where
    firstRes rawName = formattedColName (fieldPrefixType decOption) (Just $ tablename rawName) colName
    tablename rawName = fromText $ decodeName decOption rawName
    colName = fromText $ unsafeSnakeCaseT $ getName input
    decOption = fromMaybe mempty (getDecodeOption context)

-- | A data structure to enable a functor instance
-- This is meant to simplify the handling of nullability, and the composition of decoders.
data DecodeTuple decodedT = DecodeTuple
  { decValue :: Decoders.Value decodedT,
    decNValue :: Decoders.Value decodedT -> Decoders.NullableOrNot Decoders.Value decodedT
  }

defaultDecodeTuple :: Decoders.Value decodedT -> DecodeTuple decodedT
defaultDecodeTuple value =
  DecodeTuple
    { decValue = value,
      decNValue = Decoders.nonNullable
    }

-- | Turns a 'DecodeTuple' into a hasql decoder.
decodeTupleToHasql :: DecodeTuple decodedT -> Decoders.NullableOrNot Decoders.Value decodedT
decodeTupleToHasql input = decNValue input $ decValue input

refineDecoder :: (a -> Either Text b) -> DecodeTuple a -> DecodeTuple b
refineDecoder refiner (DecodeTuple dec _) = result
  where
    result = DecodeTuple (Decoders.refine refiner dec) Decoders.nonNullable

fmapDecoderValue :: (Decoders.Value a -> Decoders.Value b) -> DecodeTuple a -> DecodeTuple b
fmapDecoderValue refiner (DecodeTuple dec _) = result
  where
    result = DecodeTuple (refiner dec) Decoders.nonNullable

instance Functor DecodeTuple where
  fmap typeChanger (DecodeTuple dec _) = DecodeTuple (typeChanger <$> dec) Decoders.nonNullable

data DecodeFieldPrefixType = TableName | CompositeField deriving (Show)

data DecodeOption = DecodeOption
  { nameMap :: Map Text Text,
    fieldPrefixType :: DecodeFieldPrefixType
  }
  deriving (Show)

instance Semigroup DecodeOption where
  (<>) a b =
    DecodeOption
      { nameMap = nameMap a <> nameMap b,
        fieldPrefixType = fieldPrefixType a
      }

instance Monoid DecodeOption where
  mempty =
    DecodeOption
      { nameMap = mempty,
        fieldPrefixType = TableName
      }

-- | When using an overridable name sue this function
-- before using it.
decodeName :: DecodeOption -> Text -> Text
decodeName decOpt name = fromMaybe name (Map.lookup name $ nameMap decOpt)

-- | When using an overridable name in a given context 'a'
-- use this function before using the name.
decodeNameInContext :: MassaliaContext a => a -> Text -> Text
decodeNameInContext context name = maybe name (`decodeName` name) (getDecodeOption context)

-- | A class to decode
class SQLDecoder contextT decodedT where
  sqlExpr ::
    ( QueryFormat qf,
      MassaliaTree treeNode,
      MassaliaContext contextT
    ) =>
    contextT ->
    treeNode ->
    (Text -> qf, DecodeTuple decodedT)
  sqlExpr context selection = (defaultDecodeExpr context selection, sqlDecoder @contextT @decodedT)
  sqlDecoder :: DecodeTuple decodedT

fmapList :: forall contextT element. (SQLDecoder contextT element) => DecodeTuple [element]
fmapList = fmapDecoderValue (Decoders.listArray . Decoders.nonNullable) (sqlDecoder @contextT @element)

fmapVector :: forall contextT element. (SQLDecoder contextT element) => DecodeTuple (Vector element)
fmapVector = fmapDecoderValue (Decoders.vectorArray . Decoders.nonNullable) (sqlDecoder @contextT @element)

instance SQLDecoder contextT UUID where
  sqlDecoder = defaultDecodeTuple Decoders.uuid
instance SQLDecoder contextT [UUID] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector UUID) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT Text where
  sqlDecoder = defaultDecodeTuple Decoders.text
instance SQLDecoder contextT (Vector Text) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [Text] where
  sqlDecoder = fmapList
instance SQLDecoder contextT Bool where
  sqlDecoder = defaultDecodeTuple Decoders.bool
instance SQLDecoder contextT (Vector Bool) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [Bool] where
  sqlDecoder = fmapList
instance SQLDecoder contextT EmailAddress where
  sqlDecoder = defaultDecodeTuple $ Decoders.custom $ const emailValidateText
instance SQLDecoder contextT (Vector EmailAddress) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [EmailAddress] where
  sqlDecoder = fmapList
instance SQLDecoder contextT Int64 where
  sqlDecoder = defaultDecodeTuple Decoders.int8
instance SQLDecoder contextT (Vector Int64) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [Int64] where
  sqlDecoder = fmapList
instance SQLDecoder contextT Int where
  sqlDecoder = defaultDecodeTuple (fromIntegral <$> Decoders.int4)
instance SQLDecoder contextT (Vector Int) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [Int] where
  sqlDecoder = fmapList
instance SQLDecoder contextT LocalTime where
  sqlDecoder = defaultDecodeTuple Decoders.timestamp
instance SQLDecoder contextT (Vector LocalTime) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [LocalTime] where
  sqlDecoder = fmapList
instance SQLDecoder contextT Day where
  sqlDecoder = defaultDecodeTuple Decoders.date
instance SQLDecoder contextT (Vector Day) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [Day] where
  sqlDecoder = fmapList
instance SQLDecoder contextT Scientific where
  sqlDecoder = defaultDecodeTuple Decoders.numeric
instance SQLDecoder contextT (Vector Scientific) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [Scientific] where
  sqlDecoder = fmapList
instance SQLDecoder contextT UTCTime where
  sqlDecoder = defaultDecodeTuple Decoders.timestamptz
instance SQLDecoder contextT (Vector UTCTime) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT [UTCTime] where
  sqlDecoder = fmapList

instance
  ( SQLDecoder contextT a
  ) =>
  SQLDecoder contextT (Maybe a)
  where
  sqlExpr = (fmap . fmap . fmap) action (sqlExpr @contextT @a)
  sqlDecoder = action (sqlDecoder @contextT @a)

action :: DecodeTuple a -> DecodeTuple (Maybe a)
action (DecodeTuple decoder _) =
  DecodeTuple
    (Nothing <$ decoder)
    (const $ Decoders.nullable decoder)

-- | This gives the ability to pass, for instance, user roles, user data,
-- authentication information, in every SQL building block of massalia.
-- For instance, if you have an SQLQuery instance for a type A and you
-- want to restrict which sql rows of A are accessible, you can shape
-- the query in the SQLQuery instance based on the token passed around in
-- 'MassaliaContext'.
--
-- The object to pass around is entirely user defined.
class MassaliaContext contextT where
  getDecodeOption :: contextT -> Maybe DecodeOption
  setDecodeOption :: DecodeOption -> contextT -> contextT

-- int4range — Range of integer
-- int8range — Range of bigint
-- numrange — Range of numeric
-- tsrange — Range of timestamp without time zone
-- tstzrange — Range of timestamp with time zone
-- daterange — Range of date

instance
  ( PostgresRange a,
    SQLEncoder a
  ) =>
  SQLEncoder (SimpleRange a)
  where
  textEncode = encodeRange
  binaryEncode = encodeRange
  binaryDedupeEncode = encodeRange

-- | test
--
-- >>> import Massalia.Utils (SimpleRange(SimpleRange))
-- >>> (sqlEncode $ SimpleRange (Just 1 :: Maybe Int) Nothing Nothing) :: Text
-- "int4range(1,null)"
--
-- >>> import Massalia.Utils (SimpleRange(SimpleRange))
-- >>> (sqlEncode $ SimpleRange (Just 1 :: Maybe Int) (Nothing :: Maybe Int) (Just EE)) :: Text
-- "int4range(1,null, '()')"
--
encodeRange ::
  forall qf dataT.
  ( QueryFormat qf,
    SQLEncoder dataT,
    PostgresRange dataT
  ) =>
  SimpleRange dataT ->
  qf
encodeRange value = postgresRangeName @dataT <> "(" <> startValue <> "," <> endValue <> bounds <> ")"
  where
    startValue = getBoundary start
    endValue = getBoundary end
    bounds =
       case inclusivity value of
          Nothing -> ""
          Just II -> ", '[]'"
          Just IE -> ", '[)'"
          Just EI -> ", '(]'"
          Just EE -> ", '()'"
    getBoundary accessor = maybe "null" sqlEncode (accessor value)

class PostgresRange a where
  postgresRangeName :: (IsString textFormat) => textFormat

instance PostgresRange Int where
  postgresRangeName = "int4range"

instance PostgresRange Int32 where
  postgresRangeName = "int4range"

instance PostgresRange Int64 where
  postgresRangeName = "int8range"

instance PostgresRange Integer where
  postgresRangeName = "int8range"

instance PostgresRange UTCTime where
  postgresRangeName = "tsrange"

instance PostgresRange LocalTime where
  postgresRangeName = "tstzrange"

instance PostgresRange ZonedTime where
  postgresRangeName = "tstzrange"

instance PostgresRange ZonedTimeEq where
  postgresRangeName = "tstzrange"

instance PostgresRange Day where
  postgresRangeName = "daterange"

instance PostgresRange Void where
  postgresRangeName = panic "in theory cannot happen, (PostgresRange Void)"
