{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module MassaliaSQLPart where

import Data.String (IsString)
import Data.Text (Text, unpack, pack)
import qualified Data.String as String (IsString(fromString))
import MassaliaUtils (intercalate, intercalateMap)
import Hasql.DynamicStatements.Snippet (Snippet)

class (IsString content) => IsTextOrString content where  
  fromString :: String -> content
  fromText :: Text -> content

instance IsTextOrString [Char] where
  fromString = id
  fromText = unpack

instance IsTextOrString Text where
  fromString = pack
  fromText = id

instance IsTextOrString Snippet where
  fromString = String.fromString
  fromText =  String.fromString . unpack -- THIS IS BAD, TODO use a direct encoding from text

newtype ASelectQueryPart partType content = SelectQueryPart content
deriving instance (IsTextOrString content) => IsTextOrString (ASelectQueryPart partType content)
deriving instance (IsString content) => IsString (ASelectQueryPart partType content)
deriving instance (Semigroup content) => Semigroup (ASelectQueryPart partType content)
deriving instance (Monoid content) => Monoid (ASelectQueryPart partType content)

data AssemblingOptions content = AssemblingOptions {
  partSeparator :: content,
  innerSeparator :: content
}
defaultAssemblingOptions = AssemblingOptions {
  partSeparator = "\n",
  innerSeparator = ","
}
testAssemblingOptions = AssemblingOptions {
  partSeparator = " ",
  innerSeparator = ", "
}

getContent :: ASelectQueryPart partType content -> content
getContent (SelectQueryPart content) = content

getListContent :: (Monoid content) => content -> ASelectQueryPart partType content -> [ASelectQueryPart partType content] -> [content]
getListContent separator _ [] = []
getListContent separator prefix valueList = [(getContent prefix) <> intercalate separator (getContent <$> valueList)]

getMaybeContent :: (Monoid content) => ASelectQueryPart partType content -> Maybe (ASelectQueryPart partType content) -> [content]
getMaybeContent _ Nothing = []
getMaybeContent prefix (Just value) = [(getContent prefix) <> getContent value]