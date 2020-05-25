{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}

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
    FromText(fromText),
    IsString(fromString),
    DefaultParamEncoder,
    BinaryQuery,
    TextQuery,
    commaAssemble,
    (§),
    takeParam,
    takeMaybeParam,
    Snippet.param,
    inSingleQuote,
    inParens,
    commaSepInParens
  )
where

import Data.Foldable (foldr1)
import Data.Int (Int64)
import Massalia.SelectionTree (MassaliaTree(getName))
import Data.Sequence (Seq)
import Massalia.UtilsGQL (Paginated, filtered)
import Data.String (String, IsString)
import qualified Data.String as String (IsString (fromString))
import Data.Text (Text, pack, replace, unpack)
import Data.UUID hiding (fromText, fromString)
import Data.Vector (Vector)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Encoders (NullableOrNot, Value)
import Hasql.Implicits.Encoders (DefaultParamEncoder (defaultParam))
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
  a ->
  (queryFormat -> queryFormat, (Decoders.Value decodedT, Nullability decodedT))
scalar decoder input = (col, (decoder, Decoders.nonNullable))
  where col tablename = "\"" <> tablename <> "\".\"" <> (fromText $ getName input) <> "\""


type Nullability decodedT = Decoders.Value decodedT -> Decoders.NullableOrNot Decoders.Value decodedT
-- | A class to decode
class (QueryFormat qf) => SQLDecoder qf filterType decodedT where
  sqlDecode :: (QueryFormat qf, MassaliaTree treeNode) =>
    (Maybe filterType) -> treeNode -> (qf -> qf, (Decoders.Value decodedT, Nullability decodedT))
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
  sqlDecode maybeFilter tree = fmap action $ sqlDecode maybeFilter tree
    where
      action (decoder, _) = (const Nothing <$> decoder, const $ Decoders.nullable decoder) 

