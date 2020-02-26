{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MassaliaEncoder
  (DynamicParameters(param),
  TextEncoder(paramEncode),
  DefaultParamEncoder
  )
where

import Hasql.Implicits.Encoders (DefaultParamEncoder(defaultParam))
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Data.Text
import Data.UUID
import Data.Int (Int64)
import Data.Void (Void)

class DynamicParameters content where
  param :: (TextEncoder a, DefaultParamEncoder a) => a -> content

instance DynamicParameters Snippet where
  param = Snippet.param
instance DynamicParameters Text where
  param = paramEncode
instance DynamicParameters String where
  param = unpack . paramEncode 

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
