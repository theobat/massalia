{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MassaliaQueryFormat
  ( QueryFormat (param, fromText),
    TextEncoder (paramEncode),
    DefaultParamEncoder,
    HasqlSnippet,
  )
where

import Data.Int (Int64)
import Data.String (IsString)
import qualified Data.String as String (IsString (fromString))
import Data.Text (Text, pack, unpack, replace)
import Data.UUID
import Data.Vector (Vector)
import Data.Sequence (Seq)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Encoders (NullableOrNot, Value)
import Hasql.Implicits.Encoders (DefaultParamEncoder (defaultParam))
import MassaliaUtils (intercalate, intercalateMap)
import PostgreSQL.Binary.Data (LocalTime)
import Protolude hiding (intercalate, replace)
import Data.Foldable (foldr1)

type HasqlSnippet = Snippet

instance DefaultParamEncoder Void where
  defaultParam = panic "This should never happen, cannot encode Void as a parameter"

-- | This typeclass is a wrapper around the existing 'Snippet' type in Hasql
-- which conveniently provide the ability to generate queries as 'Text' for testing/debugging
-- purposes while retaining the performance gain by having pure bytecode in production.
class (IsString content, Monoid content) => QueryFormat content where
  -- | A function to transform any 'Text' to the current content type.
  fromText :: Text -> content
  -- | A function to safely encode any variable value into the current content type.
  param :: (TextEncoder a, DefaultParamEncoder a) => a -> content
  -- | A function to encode a parameter using an explicit encoder.
  paramWithEncoder :: (TextEncoder a, DefaultParamEncoder a) =>  NullableOrNot Value a -> a -> content

-- | ALL the supported Query formats in this library

-- | The legacy 'String' type for your query.
instance QueryFormat [Char] where
  fromText = unpack
  param = unpack . paramEncode
  paramWithEncoder = const (unpack . paramEncode)

-- | The query format for tests, debugging and any human interaction oriented action with your queries.
instance QueryFormat Text where
  fromText = identity
  param = paramEncode
  paramWithEncoder = const paramEncode

-- | The production oriented 'Snippet' type for your query.
instance QueryFormat Snippet where
  fromText = String.fromString . unpack -- THIS IS BAD, TODO use a direct encoding from text
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


