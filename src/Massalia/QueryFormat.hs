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
    DecodeOption(DecodeOption),
    DecodeTuple (DecodeTuple),
    defaultDecodeTuple,
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
import Data.Coerce (coerce)
import Massalia.Utils (
    EmailAddress, LocalTime,
    Day, Scientific, UTCTime,
    intercalate, intercalateMap, emailToText,
    emailValidateText
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

instance SQLEncoder Text EmailAddress where
  sqlEncode = inSingleQuote . emailToText
instance SQLEncoder Snippet EmailAddress where
  sqlEncode = Snippet.param . emailToText 

inSingleQuote a = "'" <> a <> "'"
inParens a = "(" <> a <> ")"
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
  (QueryFormat queryFormat, MassaliaTree a ) =>
  Decoders.Value decodedT ->
  DecodeOption ->
  a ->
  (Text -> queryFormat, DecodeTuple decodedT)
scalar decoder decOption input = (col, (DecodeTuple decoder Decoders.nonNullable))
  where
    col rawName = "\"" <> tablename <> "\".\"" <> (fromText $ getName input) <> "\""
      where tablename = fromText $ decodeName decOption rawName

data DecodeTuple decodedT = DecodeTuple {
  decValue :: Decoders.Value decodedT,
  decNValue :: Decoders.Value decodedT -> Decoders.NullableOrNot Decoders.Value decodedT
}
defaultDecodeTuple :: Decoders.Value decodedT -> DecodeTuple decodedT
defaultDecodeTuple value = DecodeTuple {
  decValue = value,
  decNValue = Decoders.nonNullable
}

instance Functor DecodeTuple where
  fmap typeChanger (DecodeTuple decoder nDecoder) = DecodeTuple (typeChanger <$> decoder) Decoders.nonNullable

data DecodeOption = DecodeOption {
  nameMap :: Map Text Text
} deriving (Show)
instance Semigroup DecodeOption where
  (<>) a b = DecodeOption {nameMap = nameMap a <> nameMap b}
instance Monoid DecodeOption where
  mempty = DecodeOption { nameMap = mempty }
decodeName :: DecodeOption -> Text -> Text
decodeName decOpt name = fromMaybe name (Map.lookup name $ nameMap decOpt)

-- | A class to decode
class (QueryFormat qf) => SQLDecoder qf filterType decodedT where
  sqlDecode :: (QueryFormat qf, MassaliaTree treeNode) =>
    Maybe filterType -> DecodeOption -> treeNode -> (Text -> qf, DecodeTuple decodedT)

instance (QueryFormat qf) => SQLDecoder qf filterType UUID where
  sqlDecode _ = scalar Decoders.uuid
instance (QueryFormat qf) => SQLDecoder qf filterType Text where
  sqlDecode _ = scalar Decoders.text
instance (QueryFormat qf) => SQLDecoder qf filterType Bool where
  sqlDecode _ = scalar Decoders.bool
instance (QueryFormat qf) => SQLDecoder qf filterType EmailAddress where
  sqlDecode _ = scalar (Decoders.custom $ const emailValidateText)
instance (QueryFormat qf) => SQLDecoder qf filterType Int64 where
  sqlDecode _ = scalar Decoders.int8
-- instance (QueryFormat qf) => SQLDecoder qf filterType Int where
--   sqlDecode _ a b = ((fromIntegral <$>) . snd) $ scalar Decoders.int8
instance (QueryFormat qf) => SQLDecoder qf filterType LocalTime where
  sqlDecode _ = scalar Decoders.timestamp
instance (QueryFormat qf) => SQLDecoder qf filterType Day where
  sqlDecode _ = scalar Decoders.date
instance (QueryFormat qf) => SQLDecoder qf filterType Scientific where
  sqlDecode _ = scalar Decoders.numeric
instance (QueryFormat qf) => SQLDecoder qf filterType UTCTime where
  sqlDecode _ = scalar Decoders.timestamptz

instance (
    SQLDecoder qf filterType a,
    QueryFormat qf
  ) => SQLDecoder qf filterType (Maybe a) where
  sqlDecode maybeFilter opt tree = fmap action $ sqlDecode maybeFilter opt tree
    where
      action (DecodeTuple decoder nDecoder) = DecodeTuple
        (const Nothing <$> decoder) (const $ Decoders.nullable decoder)

sqlDecodeCoerce :: Coercible a b => (Text -> qf, DecodeTuple a) -> (Text -> qf, DecodeTuple b)
sqlDecodeCoerce = fmap action
  where action (DecodeTuple decoder nDecoder) = DecodeTuple (coerce <$> decoder) Decoders.nonNullable
