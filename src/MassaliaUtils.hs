module MassaliaUtils
  ( intercalate,
    intercalateMap,
  )
where

intercalate :: Monoid a => a -> [a] -> a
intercalate = intercalateMap id

intercalateMap ::  Monoid a => (b -> a) -> a -> [b] -> a
intercalateMap mapper separator currentList = case currentList of
  [] -> mempty
  [e1] -> mapper e1
  (e1:reducedList) -> mapper e1 <> separator <> intercalateMap mapper separator reducedList