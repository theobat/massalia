{-# LANGUAGE DuplicateRecordFields #-}

module MassaliaUtils
  ( intercalate,
    intercalateMap,
  )
where

import Data.Text (Text)

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
}

-- | A normalized API for all paginated query Args.
data QueryArgsPaginated filtered = QueryArgsPaginated {
  filtered :: filtered,
  first :: Int,
  after :: Text
}
