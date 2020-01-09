{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module MorpheusTestdata
    ( testSelection
    , orgSelectionSet
    ) where

import           Data.Morpheus.Kind     (OBJECT, ENUM, SCALAR)
import           Data.Morpheus.Types        (ScalarValue(String), GQLScalar(..), ResolveQ, liftEither, GQLRequest(..),
  GQLResponse, Resolver (..), IORes, GQLRootResolver (..), GQLType(..), Undefined (..))
import           Data.Morpheus.Types.Internal.AST.Selection
                                                (
                                                  ValidArguments,
                                                SelectionContent(SelectionField, SelectionSet),
                                                ValidSelectionRec,
                                                ValidSelectionSet,
                                                ValidSelection,
                                                SelectionSet,
                                                Selection(..),
                                                Arguments
                                                )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key, Position(Position) )


-- require ValidSelectionSet

selectionGen :: ValidArguments -> ValidSelectionRec -> ValidSelection
selectionGen a = Selection a (Position 0 0) Nothing

orgSelectionSet :: ValidSelectionSet
orgSelectionSet =
  [ ("orgId"  , selectionGen [] SelectionField)
  , ("plantList", selectionGen [] $ SelectionSet plantSelTest)
  ]

plantSelTest :: ValidSelectionSet
plantSelTest =
  [ ("plantId"  , selectionGen [] SelectionField)
  -- , ("truckList", selectionGen [] $ SelectionSet truckSelTest)
  ]

truckSelTest :: ValidSelectionSet
truckSelTest =
  [ ("truckId"  , selectionGen [] SelectionField)
  ]

testSelection :: ValidSelectionSet
testSelection = orgSelectionSet

-- selectionSetToString :: Text -> Text -> ValidSelectionSet -> Text
-- selectionSetToString queryName nodeName selset =
--   "query " <> queryName <> " { " <> nodeName <> resolveList selset <> " }"
--   where
--     resolveList partialSelset = (foldr resolved " { " partialSelset) <> " } "
--     resolved (key, Selection{ selectionRec = selectionRec }) queryRes = case selectionRec of
--           SelectionField -> queryRes <> key
--           (ValidSelectionSet deeperSel) -> queryRes <> resolveList deeperSel