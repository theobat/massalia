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

-- |
-- Module      : Massalia.QueryFormat
-- Description : A module to define a query format __interface__ for SQL queries. In Hasql, we use dynamic queries
--  because we don't know in advance what parameters will be provided or not by the graphql query. The query format
--  provided is made for communicating efficiently with Postgres so it has no instance of 'Show' or 'Eq' which can be
--  a problem for testing and for debugging. This module defined a typeclass to abstract whatever Type is used for the
--  query formatting.
module Massalia.QueryFormat
  ( QueryFormat,
    SQLEncoder (sqlEncode, ignoreInGenericInstance),
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
    intercalate, intercalateMap, emailToText
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
  (SQLEncoder paramValueType content) =>
  (recordType -> paramValueType) ->
  recordType ->
  content
takeParam accessor record = sqlEncode $ accessor record

takeMaybeParam ::
  (SQLEncoder paramValueType content) =>
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
  SQLEncoder underlyingType queryFormat where
  ignoreInGenericInstance :: Bool
  ignoreInGenericInstance = False
  sqlEncode :: underlyingType -> queryFormat

voidMessage = "cannot happen because Void has no inhabitant and SQLEncoder expect Void -> queryFormat"
instance SQLEncoder Void Text where
  sqlEncode = panic $ "(SQLEncoder Void Text)" <> voidMessage
instance SQLEncoder Void Snippet where
  sqlEncode = panic $ "(SQLEncoder Void Snippet)" <> voidMessage

instance (SQLEncoder a Text) => SQLEncoder (Maybe a) Text where
  sqlEncode = maybe "null" sqlEncode
instance (SQLEncoder a Snippet) => SQLEncoder (Maybe a) Snippet where
  sqlEncode = maybe "null" sqlEncode
instance (SQLEncoder a Text) => SQLEncoder [a] Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder [a]) => SQLEncoder [a] Snippet where
  sqlEncode = Snippet.param
instance (SQLEncoder a Text) => SQLEncoder (Vector a) Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder (Vector a)) => SQLEncoder (Vector a) Snippet where
  sqlEncode = Snippet.param
instance (SQLEncoder a Text) => SQLEncoder (Seq a) Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder (Seq a)) => SQLEncoder (Seq a) Snippet where
  sqlEncode = Snippet.param
instance (SQLEncoder a Text) => SQLEncoder (Set a) Text where
  sqlEncode = collectionTextEncode
instance (DefaultParamEncoder (Set a)) => SQLEncoder (Set a) Snippet where
  sqlEncode = Snippet.param
instance (QueryFormat format) => SQLEncoder (Paginated a) format where
  ignoreInGenericInstance = True
  sqlEncode = const ""


instance SQLEncoder UUID Text where
  sqlEncode = inSingleQuote . toText
instance SQLEncoder UUID Snippet where
  sqlEncode = Snippet.param
  
instance SQLEncoder Text Text where
  sqlEncode = inSingleQuote
instance SQLEncoder Text Snippet where
  sqlEncode = Snippet.param
  
instance SQLEncoder Int64 Text where
  sqlEncode = pack . show
instance SQLEncoder Int64 Snippet where
  sqlEncode = Snippet.param
instance SQLEncoder Int Text where
  sqlEncode = pack . show
instance SQLEncoder Int Snippet where
  sqlEncode input = Snippet.param (fromIntegral input :: Int64)

instance SQLEncoder Day Text where
  sqlEncode = inSingleQuote . pack . show
instance SQLEncoder Day Snippet where
  sqlEncode = Snippet.param

instance SQLEncoder EmailAddress Text where
  sqlEncode = inSingleQuote . emailToText
instance SQLEncoder EmailAddress Snippet where
  sqlEncode = Snippet.param . emailToText 

inSingleQuote a = "'" <> a <> "'"
inParens a = "(" <> a <> ")"
commaSepInParens = inParens . (intercalate ",")

collectionTextEncode :: (Foldable collection, SQLEncoder underlyingType Text) =>
  collection underlyingType -> Text
collectionTextEncode collection = wrapCollection assembled
  where
    assembled = foldr assemblerFun "" collection
    assemblerFun :: SQLEncoder b Text => b -> Text -> Text
    assemblerFun val "" = rawEncode val
    assemblerFun val currentEncoded = currentEncoded <> "," <> rawEncode val
    rawEncode val = replace "'" "" $ sqlEncode val
    wrapCollection a = "'{" <> a <> "}'"
    
------------------------- Decoder stuff
scalar :: (QueryFormat queryFormat, MassaliaTree a ) => b -> a -> (queryFormat -> queryFormat, b)
scalar decoder input = (\tablename -> tablename <> "." <> (fromText $ getName input), decoder)

-- | A class to decode
class (QueryFormat queryFormat) => SQLDecoder queryFormat filterType haskellType where
  sqlDecode :: (QueryFormat queryFormat, MassaliaTree treeNode) =>
    (Maybe filterType) -> treeNode -> (queryFormat -> queryFormat, Decoders.Value haskellType)
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType UUID where
  sqlDecode _ = scalar Decoders.uuid
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType Text where
  sqlDecode _ = scalar Decoders.text
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType Int64 where
  sqlDecode _ = scalar Decoders.int8
-- instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType Int where
--   sqlDecode _ a b = ((fromIntegral <$>) . snd) $ scalar Decoders.int8
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType LocalTime where
  sqlDecode _ = scalar Decoders.timestamp
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType Day where
  sqlDecode _ = scalar Decoders.date
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType Scientific where
  sqlDecode _ = scalar Decoders.numeric
instance (QueryFormat queryFormat) => SQLDecoder queryFormat filterType UTCTime where
  sqlDecode _ = scalar Decoders.timestamptz


-- todo:
-- instance SQLDecoder EmailAddress where
--   sqlDecode = emailToText <$> Decoders.custom
