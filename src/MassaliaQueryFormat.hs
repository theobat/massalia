{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MassaliaQueryFormat
  (QueryFormat(param, fromText),
  TextEncoder(paramEncode),
  DefaultParamEncoder
  )
where

import Data.String (IsString)
import Data.Text (Text, unpack, pack)
import qualified Data.String as String (IsString(fromString))
import MassaliaUtils (intercalate, intercalateMap)
import Hasql.DynamicStatements.Snippet (Snippet)
import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))
import Data.Text hiding (intercalate)
import Data.UUID
import Data.Int (Int64)
import Data.Void (Void)
import qualified Hasql.DynamicStatements.Snippet as Snippet


instance DefaultParamEncoder Void where
  defaultParam = error "cannot call DefaultParamEncoder.defaultParam of Void"

class (IsString content, Monoid content) => QueryFormat content where  
  fromText :: Text -> content
  param :: (TextEncoder a, DefaultParamEncoder a) => a -> content

instance QueryFormat [Char] where
  fromText = unpack
  param = unpack . paramEncode 

instance QueryFormat Text where
  fromText = id
  param = paramEncode

instance QueryFormat Snippet where
  fromText =  String.fromString . unpack -- THIS IS BAD, TODO use a direct encoding from text
  param = Snippet.param

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

instance (TextEncoder a) => TextEncoder [a] where
  paramEncode list = wrapList $ intercalate ", " (((replace "'" "") . paramEncode) <$> list)

wrap a = "'" <> a <> "'"
wrapList a = "'{" <> a <> "}'"
