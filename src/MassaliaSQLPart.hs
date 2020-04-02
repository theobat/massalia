{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MassaliaSQLPart where

import Data.String (IsString)
import qualified Data.String as String (IsString(fromString))
import MassaliaUtils (intercalate, intercalateMap)
import MassaliaQueryFormat (QueryFormat)

newtype AQueryPart partType content = AQueryPartConst content
deriving instance (QueryFormat content) => QueryFormat (AQueryPart partType content)
deriving instance (IsString content) => IsString (AQueryPart partType content)
deriving instance (Semigroup content) => Semigroup (AQueryPart partType content)
deriving instance (Monoid content) => Monoid (AQueryPart partType content)

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

getContent :: AQueryPart partType content -> content
getContent (AQueryPartConst content) = content

getListContent :: (Monoid content) => content -> AQueryPart partType content -> [AQueryPart partType content] -> [content]
getListContent separator _ [] = []
getListContent separator prefix valueList = [(getContent prefix) <> intercalate separator (getContent <$> valueList)]

getMaybeContent :: (Monoid content) => AQueryPart partType content -> Maybe (AQueryPart partType content) -> [content]
getMaybeContent _ Nothing = []
getMaybeContent prefix (Just value) = [(getContent prefix) <> getContent value]

