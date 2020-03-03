{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MassaliaSQLPart where

import Data.String (IsString)
import qualified Data.String as String (IsString(fromString))
import MassaliaUtils (intercalate, intercalateMap)
import MassaliaQueryFormat (QueryFormat)

newtype ASelectQueryPart partType content = SelectQueryPart content
deriving instance (QueryFormat content) => QueryFormat (ASelectQueryPart partType content)
deriving instance (IsString content) => IsString (ASelectQueryPart partType content)
deriving instance (Semigroup content) => Semigroup (ASelectQueryPart partType content)
deriving instance (Monoid content) => Monoid (ASelectQueryPart partType content)

data AssemblingOptions content = AssemblingOptions {
  partSeparator :: content,
  innerSeparator :: content
}

defaultAssemblingOptions :: QueryFormat content => AssemblingOptions content
defaultAssemblingOptions = AssemblingOptions {
  partSeparator = "\n",
  innerSeparator = ","
}
testAssemblingOptions :: QueryFormat content => AssemblingOptions content
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

