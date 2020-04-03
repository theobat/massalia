{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaUtils
  ( intercalate,
    intercalateMap,
    QueryArgsPaginated(QueryArgsPaginated, first, offset)
  )
where
import Data.Morpheus.Types (GQLType)
import Data.Text (Text)
import GHC.Generics (Generic)

intercalate :: Monoid a => a -> [a] -> a
intercalate = intercalateMap id

intercalateMap ::  Monoid a => (b -> a) -> a -> [b] -> a
intercalateMap mapper separator currentList = case currentList of
  [] -> mempty
  [e1] -> mapper e1
  (e1:reducedList) -> mapper e1 <> separator <> intercalateMap mapper separator reducedList


-- | A normalized API for all (non paginated) query Args.
data QueryArgs filtered = QueryArgs {
  filtered :: filtered
} deriving (Generic, GQLType)

-- | A normalized API for all paginated query Args.
data QueryArgsPaginated filtered = QueryArgsPaginated {
  filtered :: filtered,
  first :: Int,
  offset :: Int
} deriving (Generic, GQLType)
