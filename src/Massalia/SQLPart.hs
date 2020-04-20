{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Massalia.SQLPart
-- Description : A module to define a simple wrapper around a query format do distinguis query parts.
-- For now this is mostly used in select statements.
module Massalia.SQLPart where

import Data.String (IsString)
import qualified Data.String as String (IsString (fromString))
import Massalia.QueryFormat (QueryFormat)
import Massalia.Utils (intercalate)

newtype AQueryPart partType content = AQueryPartConst content

deriving instance (QueryFormat content) => QueryFormat (AQueryPart partType content)

deriving instance (IsString content) => IsString (AQueryPart partType content)

deriving instance (Semigroup content) => Semigroup (AQueryPart partType content)

deriving instance (Monoid content) => Monoid (AQueryPart partType content)

data AssemblingOptions content
  = AssemblingOptions
      { partSeparator :: content,
        innerSeparator :: content
      }

defaultAssemblingOptions :: QueryFormat content => AssemblingOptions content
defaultAssemblingOptions =
  AssemblingOptions
    { partSeparator = "\n",
      innerSeparator = ","
    }

testAssemblingOptions :: QueryFormat content => AssemblingOptions content
testAssemblingOptions =
  AssemblingOptions
    { partSeparator = " ",
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
