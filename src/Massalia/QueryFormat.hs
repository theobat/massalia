{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.QueryFormat
-- Description : A module to define a query format __interface__ for SQL queries. In Hasql, we use dynamic queries
--  because we don't know in advance what parameters will be provided or not by the graphql query. The query format
--  provided is made for communicating efficiently with Postgres so it has no instance of 'Show' or 'Eq' which can be
--  a problem for testing and for debugging. This module defined a typeclass to abstract whatever Type is used for the
--  query formatting.
module Massalia.QueryFormat
  ( QueryFormat (param, fromText),
    TextEncoder (paramEncode),
    DefaultParamEncoder,
    HasqlSnippet,
    commaAssemble,
    (§),
    takeParam,
    takeMaybeParam,
  )
where

import Data.Foldable (foldr1)
import Data.Int (Int64)
import Data.Sequence (Seq)
import Data.String (IsString)
import qualified Data.String as String (IsString (fromString))
import Data.Text (Text, pack, replace, unpack)
import Data.UUID
import Data.Vector (Vector)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Encoders (NullableOrNot, Value)
import Hasql.Implicits.Encoders (DefaultParamEncoder (defaultParam))
import Massalia.Utils (intercalate, intercalateMap)
import PostgreSQL.Binary.Data (LocalTime)
import Protolude hiding (intercalate, replace)

type HasqlSnippet = Snippet

(§) :: QueryFormat content => content -> content -> content
(§) a b = a <> "," <> b

commaAssemble :: (IsString a, Monoid a) => [a] -> a
commaAssemble = intercalateMap identity ","

-- | Yields a query formatted snippet with the given parameter encoded as a parameter in the query format.
takeParam ::
  (QueryFormat content, TextEncoder paramValueType, DefaultParamEncoder paramValueType) =>
  (recordType -> paramValueType) ->
  recordType ->
  content
takeParam accessor record = param $ accessor record

takeMaybeParam ::
  (QueryFormat content, TextEncoder paramValueType, DefaultParamEncoder paramValueType) =>
  (recordType -> Maybe paramValueType) ->
  recordType ->
  content ->
  content
takeMaybeParam accessor record defaultValue = case (accessor record) of
  Nothing -> defaultValue
  Just value -> param value

instance DefaultParamEncoder Void where
  defaultParam = panic "This should never happen, cannot encode Void as a parameter"

-- | This typeclass is a wrapper around the existing 'Snippet' type in "Hasql.DynamicStatements.Snippet"
-- which conveniently provide the ability to generate queries as 'Text' for testing/debugging
-- purposes while retaining the performance gain by having pure bytecode in production.
class (IsString content, Monoid content) => QueryFormat content where
  -- | A function to transform any 'Text' to the current content type.
  fromText :: Text -> content

  -- | A function to safely encode any variable value into the current content type.
  param :: (TextEncoder a, DefaultParamEncoder a) => a -> content

  -- | A function to encode a parameter using an explicit encoder.
  paramWithEncoder :: (TextEncoder a, DefaultParamEncoder a) => NullableOrNot Value a -> a -> content

-- | ALL the supported Query formats in this library

-- | The legacy 'String' type for your query.
-- BEWARE: this is __not safe in case of SQL injections__, you should **not** actually pass that to the database.
instance QueryFormat [Char] where
  fromText = unpack
  param = unpack . paramEncode
  paramWithEncoder = const (unpack . paramEncode)

-- | The query format for tests, debugging and any human interaction oriented action with your queries.
-- BEWARE: this is __not safe in case of SQL injections__, you should **not** actually pass that to the database.
instance QueryFormat Text where
  fromText = identity
  param = paramEncode
  paramWithEncoder = const paramEncode

-- | The production oriented 'Snippet' type for your query.
-- This is  __safe in case of SQL injections__, it's the right format form communicating with your database.
instance QueryFormat Snippet where
  fromText = String.fromString . unpack -- THIS IS BAD, TODO: use a direct encoding from text
  param = Snippet.param
  paramWithEncoder = Snippet.encoderAndParam

class TextEncoder a where
  paramEncode :: a -> Text

instance TextEncoder Void where
  paramEncode _ = ""

instance TextEncoder UUID where
  paramEncode = wrap . toText

instance TextEncoder Text where
  paramEncode = wrap

instance TextEncoder Int64 where
  paramEncode = pack . show

instance TextEncoder Int where
  paramEncode = pack . show

instance TextEncoder LocalTime where
  paramEncode = pack . show

-- | A Text encoding for any foldable collection in haskell.
-- It encodes everything as an array literal in postgres (@select '{}'::ARRAY[];@).
-- It uses the string literal encoding instead of the ARRAY[] syntax for better type inference.
collectionEncode :: (Foldable a, TextEncoder b) => a b -> Text
collectionEncode collection = wrapCollection assembled
  where
    assembled = foldr assemblerFun "" collection
    assemblerFun :: TextEncoder b => b -> Text -> Text
    assemblerFun val "" = rawEncode val
    assemblerFun val currentEncoded = currentEncoded <> "," <> rawEncode val
    rawEncode val = replace "'" "" $ paramEncode val
    wrapCollection a = "'{" <> a <> "}'"

instance (TextEncoder a) => TextEncoder [a] where
  paramEncode = collectionEncode

instance (TextEncoder a) => TextEncoder (Vector a) where
  paramEncode = collectionEncode

instance (TextEncoder a) => TextEncoder (Set a) where
  paramEncode = collectionEncode

instance (TextEncoder a) => TextEncoder (Seq a) where
  paramEncode = collectionEncode

wrap a = "'" <> a <> "'"
