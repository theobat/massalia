{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : Massalia.QueryFormat
-- Description : A module to define a query format __interface__ for SQL queries. In Hasql, we use dynamic queries
--  because we don't know in advance what parameters will be provided or not by the graphql query. The query format
--  provided is made for communicating efficiently with Postgres so it has no instance of 'Show' or 'Eq' which can be
--  a problem for testing and for debugging. This module defined a typeclass to abstract whatever Type is used for the
--  query formatting.
module Massalia.QueryFormat
  ( QueryFormat,
    SQLEncoder (sqlEncode, wrapEncoding, ignoreInGenericInstance),
    SQLDecoder(sqlDecode),
    DecodeOption(..),
    DecodeFieldPrefixType(..),
    DecodeTuple (DecodeTuple),
    MassaliaContext(getDecodeOption, setDecodeOption),
    defaultDecodeTuple,
    refineDecoder,
    FromText(fromText),
    IsString(fromString),
    DefaultParamEncoder,
    BinaryQuery,
    TextQuery,
    commaAssemble,
    (§),
    (°),
    joinEq,
    simpleEq,
    takeParam,
    takeMaybeParam,
    Snippet.param,
    inSingleQuote,
    inParens,
    commaSepInParens,
    decodeName,
  )
where
import Massalia.SelectionTree (MassaliaTree(getName))
import qualified Data.Map as Map
import Massalia.UtilsGQL (Paginated)
import Data.String (String)
import qualified Data.String as String (IsString (fromString))
import Data.Text (pack, replace, unpack)
import Data.UUID hiding (fromText, fromString)
import Data.Vector (Vector)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder)
import Massalia.Utils (
    EmailAddress, LocalTime,
    Day, Scientific, UTCTime,
    intercalate, intercalateMap, emailToText,
    emailValidateText,
    unsafeSnakeCaseT
  )
import qualified Hasql.Decoders as Decoders
import Protolude hiding (intercalate, replace)

-- | The text query format is the canonical representation for debugging
-- and printing queries.
type TextQuery = Text

-- | The binary query format is the internal query representation in Hasql.
-- It's called 'Snippet' and it's defined in "Hasql.DynamicStatements.Snippet".
-- It enables the representation of a query alongside it's parametrized values.
-- It has no instance of show, which explains the presence of its 'TextQuery' counterpart.
type BinaryQuery = Snippet

-- | The ° operator is taking a table name (or an sql alias) and a field name (or a field alias) and
-- yields their escaped combination.
-- 
-- Examples:
-- >>> "foo" ° "bar"
-- \"fooo\".\"bar\"
(°) :: (Semigroup a, IsString a) => a -> a -> a
(°) a b = "\"" <> a <> "\".\"" <> b <> "\""


joinEq :: (Semigroup a, IsString a) => a -> a -> a -> a -> a
joinEq tableA fieldA tableB fieldB = "JOIN \"" <> tableA <> "\" ON " <> joinCondition
  where joinCondition = simpleEq tableA fieldA tableB fieldB
simpleEq :: (Semigroup a, IsString a) => a -> a -> a -> a -> a
simpleEq tableA fieldA tableB fieldB = (tableA ° fieldA) <> " = " <> (tableB ° fieldB)

(§) :: (IsString content, Monoid content) => content -> content -> content
(§) a b = a <> "," <> b

commaAssemble :: (IsString a, Monoid a) => [a] -> a
commaAssemble = intercalateMap identity ","

-- | Yields a query formatted snippet with the given parameter encoded as a parameter in the query format.
takeParam ::
  (SQLEncoder content paramValueType) =>
  (recordType -> paramValueType) ->
  recordType ->
  content
takeParam accessor record = sqlEncode $ accessor record

takeMaybeParam ::
  (SQLEncoder content paramValueType) =>
  (recordType -> Maybe paramValueType) ->
  recordType ->
  content ->
  content
takeMaybeParam accessor record defaultValue = case (accessor record) of
  Nothing -> defaultValue
  Just value -> sqlEncode value

class (FromText queryFormat, IsString queryFormat, Monoid queryFormat)
  => QueryFormat queryFormat

instance QueryFormat Text
instance QueryFormat Snippet

class FromText content where
  fromText :: Text -> content
instance FromText Text where
  fromText = identity
instance FromText String where
  fromText = unpack
instance FromText Snippet where
  fromText = String.fromString . unpack

class (QueryFormat queryFormat) =>
  SQLEncoder queryFormat underlyingType where
  ignoreInGenericInstance :: Bool
  ignoreInGenericInstance = False
  wrapEncoding :: queryFormat -> queryFormat
  wrapEncoding = identity
  sqlEncode :: underlyingType -> queryFormat

voidMessage :: Text
voidMessage = "cannot happen because Void has no inhabitant and sqlEncode expect Void -> queryFormat"
instance SQLEncoder Text Void where
  sqlEncode = panic $ "(SQLEncoder Text Void)" <> voidMessage
instance SQLEncoder Snippet Void where
  sqlEncode = panic $ "(SQLEncoder Snippet Void)" <> voidMessage

instance (SQLEncoder Text a) => SQLEncoder Text (Maybe a) where
  sqlEncode = (wrapEncoding @Text @a) . maybe "null" sqlEncode
instance (SQLEncoder Snippet a) => SQLEncoder Snippet (Maybe a) where
  sqlEncode = (wrapEncoding @Snippet @a) . maybe "null" sqlEncode
instance (SQLEncoder Text a) => SQLEncoder Text [a] where
  sqlEncode =  collectionTextEncode
instance (DefaultParamEncoder [a]) => SQLEncoder Snippet [a] where
  sqlEncode =  Snippet.param
instance (SQLEncoder Text a) => SQLEncoder Text (Vector a) where
  sqlEncode =  collectionTextEncode
instance (SQLEncoder Snippet a, DefaultParamEncoder (Vector a)) => SQLEncoder Snippet (Vector a) where
  sqlEncode =  Snippet.param
instance (SQLEncoder Text a) => SQLEncoder Text (Seq a) where
  sqlEncode =  collectionTextEncode
instance (SQLEncoder Snippet a, DefaultParamEncoder (Seq a)) => SQLEncoder Snippet (Seq a) where
  sqlEncode =  Snippet.param
instance (SQLEncoder Text a) => SQLEncoder Text (Set a) where
  sqlEncode =  collectionTextEncode
instance (SQLEncoder Snippet a, DefaultParamEncoder (Set a)) => SQLEncoder Snippet (Set a) where
  sqlEncode = Snippet.param
instance (QueryFormat format) => SQLEncoder format (Paginated a) where
  ignoreInGenericInstance = True
  sqlEncode = const ""


instance SQLEncoder Text UUID where
  sqlEncode = (inSingleQuote . toText)
instance SQLEncoder Snippet UUID where
  sqlEncode = Snippet.param
  
instance SQLEncoder Text Text where
  sqlEncode = inSingleQuote
instance SQLEncoder Snippet Text where
  sqlEncode = Snippet.param
  
instance SQLEncoder Text Int64 where
  sqlEncode = pack . show
instance SQLEncoder Snippet Int64 where
  sqlEncode = Snippet.param
instance SQLEncoder Text Int where
  sqlEncode = pack . show
instance SQLEncoder Snippet Int where
  sqlEncode input = Snippet.param (fromIntegral input :: Int64)

instance SQLEncoder Text Day where
  sqlEncode = inSingleQuote . pack . show
instance SQLEncoder Snippet Day where
  sqlEncode = Snippet.param
instance SQLEncoder Text LocalTime where
  sqlEncode = inSingleQuote . pack . show
instance SQLEncoder Snippet LocalTime where
  sqlEncode = Snippet.param

instance SQLEncoder Text EmailAddress where
  sqlEncode = inSingleQuote . emailToText
instance SQLEncoder Snippet EmailAddress where
  sqlEncode = Snippet.param . emailToText 

inSingleQuote :: (Semigroup a, IsString a) => a -> a
inSingleQuote a = "'" <> a <> "'"
inParens :: (Semigroup a, IsString a) => a -> a
inParens a = "(" <> a <> ")"
commaSepInParens :: [[Char]] -> [Char]
commaSepInParens = inParens . (intercalate ",")

collectionTextEncode :: (Foldable collection, SQLEncoder Text underlyingType) =>
  collection underlyingType -> Text
collectionTextEncode collection = wrapCollection assembled
  where
    assembled = foldr assemblerFun "" collection
    assemblerFun :: SQLEncoder Text b => b -> Text -> Text
    assemblerFun val "" = rawEncode val
    assemblerFun val currentEncoded = currentEncoded <> "," <> rawEncode val
    rawEncode val = replace "'" "" $ sqlEncode val
    wrapCollection a = "'{" <> a <> "}'"
    
------------------------- Decoder stuff
scalar ::
  (
    QueryFormat queryFormat, MassaliaTree a,
    MassaliaContext contextT
  ) =>
  Decoders.Value decodedT ->
  contextT ->
  a ->
  (Text -> queryFormat, DecodeTuple decodedT)
scalar decoder context input = case fieldPrefixType decOption of
  CompositeField -> (compo, (DecodeTuple decoder Decoders.nonNullable))
  TableName -> (col, (DecodeTuple decoder Decoders.nonNullable))
  where
    compo rawName = "(" <> tablename rawName <> ").\"" <> colName <> "\""
    col rawName = "\"" <> tablename rawName <> "\".\"" <> colName <> "\""
    tablename rawName = fromText $ decodeName decOption rawName
    colName = fromText $ unsafeSnakeCaseT $ getName input
    decOption = fromMaybe mempty (getDecodeOption context)

data DecodeTuple decodedT = DecodeTuple {
  decValue :: Decoders.Value decodedT,
  decNValue :: Decoders.Value decodedT -> Decoders.NullableOrNot Decoders.Value decodedT
}
defaultDecodeTuple :: Decoders.Value decodedT -> DecodeTuple decodedT
defaultDecodeTuple value = DecodeTuple {
  decValue = value,
  decNValue = Decoders.nonNullable
}
refineDecoder :: (a -> Either Text b) -> DecodeTuple a -> DecodeTuple b
refineDecoder refiner (DecodeTuple decoder _) = result
  where result = DecodeTuple (Decoders.refine refiner decoder) Decoders.nonNullable

instance Functor DecodeTuple where
  fmap typeChanger (DecodeTuple decoder _) = DecodeTuple (typeChanger <$> decoder) Decoders.nonNullable

data DecodeFieldPrefixType = TableName | CompositeField deriving (Show)
data DecodeOption = DecodeOption {
  nameMap :: Map Text Text,
  fieldPrefixType :: DecodeFieldPrefixType
} deriving (Show)
instance Semigroup DecodeOption where
  (<>) a b = DecodeOption {
      nameMap = nameMap a <> nameMap b,
      fieldPrefixType = fieldPrefixType a
    }
instance Monoid DecodeOption where
  mempty = DecodeOption {
    nameMap = mempty,
    fieldPrefixType = TableName
  }

decodeName :: DecodeOption -> Text -> Text
decodeName decOpt name = case fieldPrefixType decOpt of
  TableName -> fromMaybe name (Map.lookup name $ nameMap decOpt)
  _ -> name
  

-- | A class to decode
class (QueryFormat qf, MassaliaContext contextT) => SQLDecoder qf contextT decodedT where
  sqlDecode :: (
    QueryFormat qf,
    MassaliaTree treeNode,
    MassaliaContext contextT
    ) =>
    contextT -> treeNode -> (Text -> qf, DecodeTuple decodedT)
type DecodeConstraint qf contextT = (QueryFormat qf, MassaliaContext contextT)

instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT UUID where
  sqlDecode = scalar Decoders.uuid
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT Text where
  sqlDecode = scalar Decoders.text
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT Bool where
  sqlDecode = scalar Decoders.bool
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT EmailAddress where
  sqlDecode = scalar (Decoders.custom $ const emailValidateText)
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT Int64 where
  sqlDecode = scalar Decoders.int8
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT Int where
  sqlDecode = scalar (fromIntegral <$> Decoders.int8)
 
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT LocalTime where
  sqlDecode = scalar Decoders.timestamp
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT Day where
  sqlDecode = scalar Decoders.date
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT Scientific where
  sqlDecode = scalar Decoders.numeric
instance (DecodeConstraint qf contextT) => SQLDecoder qf contextT UTCTime where
  sqlDecode = scalar Decoders.timestamptz

instance (
    SQLDecoder qf contextT a,
    QueryFormat qf
  ) => SQLDecoder qf contextT (Maybe a) where
  sqlDecode tree context = fmap action $ sqlDecode tree context
    where
      action (DecodeTuple decoder _) = DecodeTuple
        (const Nothing <$> decoder) (const $ Decoders.nullable decoder)

-- | This is the building bloc for the SQLSelect class, it creates the
-- FROM .. WHERE .. etc part of a SELECT SQL query based on a « context » object.
-- It enables the ability to pass, for instance, user roles, user data,
-- authentication information.
class MassaliaContext contextT where
  getDecodeOption :: contextT -> Maybe DecodeOption
  setDecodeOption :: DecodeOption -> contextT -> contextT