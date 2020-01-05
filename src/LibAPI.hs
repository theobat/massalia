{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LibAPI
    ( someFunc
      , PlantFilter(..)
      , scalar
      , SQLStructure(..)
      , defaultSQLStruct
      , compo
      , globalStructureToQuery
      , globalStructureToListStatement
      , plantFilterInstance
    )
where

import           Data.Morpheus.Kind             ( OBJECT
                                                , ENUM
                                                , SCALAR
                                                )
import           Data.Data
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Control.Monad.Except           ( ExceptT(..) )
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( ScalarValue(String)
                                                , GQLScalar(..)
                                                , GQLRequest(..)
                                                , GQLResponse
                                                , Resolver(..)
                                                , IORes
                                                , GQLRootResolver(..)
                                                , GQLType(..)
                                                , Undefined(..)
                                                )
import           Control.Monad.Identity         ( Identity )
import           Hasql.Statement                ( Statement(..) )
import qualified Hasql.Decoders                as Decoders
import qualified Hasql.Encoders                as Encoders
import           Data.Int                       ( Int64 )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key )
import qualified Hasql.Connection              as Connection
import qualified Hasql.Session                 as Session
import           Data.Maybe                     ( fromJust )
import           Data.Bifunctor                 ( first )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.UUID
import qualified Debug.Trace                   as Debug
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import Data.Functor.Contravariant ((>$<))
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( ValidSelectionSet, ValidSelection,
                                                SelectionSet, Selection(..), Arguments,
                                                SelectionContent(SelectionField, SelectionSet))

someFunc :: IO ()
someFunc = undefined

data SQLStructure encoder decoder = SQLStructure {
  wrapFunctionList :: [Text], -- either: "row" or "array_agg", "row"
  selectPart :: [Text],
  fromPart :: Text,
  joinList :: [Text],
  whereConditions :: Text,
  groupByList :: [Text],
  offset :: Int,
  limit :: Int,
  statementEncoder :: Encoders.Params encoder,
  statementDecoder :: Decoders.Composite decoder
}

defaultSQLStruct = SQLStructure {
  wrapFunctionList=[], -- either: "row" or "array_agg", "row"
  selectPart=[],
  fromPart="",
  joinList=[],
  whereConditions="",
  groupByList=[],
  offset=0,
  limit=0,
  statementEncoder=Encoders.noParams,
  statementDecoder=undefined
}

scalar :: Text -> (decoder -> a -> decoder) -> Decoders.Value a -> SQLStructure encoder decoder -> SQLStructure encoder decoder
scalar col updater decoderType sqlST = sqlST {
  selectPart = selectPart sqlST <> [col],
  statementDecoder = do
    entity <- statementDecoder sqlST
    updater entity <$> (Decoders.field (Decoders.nonNullable decoderType))
}

compo :: (ValidSelectionSet -> SQLStructure encoderTypeInA typeInA) -> (Text, Text) -> (typeA -> [typeInA] -> typeA)
  -> ValidSelection -> SQLStructure encoder typeA -> SQLStructure encoder typeA
compo generator conditions updater selSet = subQueryMerger $ resolver selSet
  where
    resolver = generateQuery generator
    subQueryMerger = mergeSubqueryList conditions updater

data GQLScalarFilter scalarType = GQLScalarFilter {
  isEq :: Maybe scalarType,
  isNotEq :: Maybe scalarType,
  isIn :: Maybe [scalarType],
  isNotIn :: Maybe [scalarType]
}

-- utilHasql :: (b -> a) -> Params a -> Maybe b -> Params a
-- utilHasql updater paraA paraB = paraA <> 

-- filterEncoder :: Maybe (GQLScalarFilter scalarType) -> (Encoders.Params scalarType, Text)
-- filterEncoder = maybe noFilterCase filterCase
--   where
--     noFilterCase = (Encoders.noParams, "")
--     filterCase (GQLScalarFilter {isEq = maybeEq, isNotEq = maybeNotEq, isIn = maybeIn, isNotIn = maybeNotIn}) = noFilterCase

data PlantFilter = PlantFilter {
  required :: Bool,
  plantIdFilter :: Maybe (GQLScalarFilter UUID)
}

defaultScalar = GQLScalarFilter {
  isEq = Nothing,
  isNotEq = Nothing,
  isIn = Nothing,
  isNotIn = Nothing
}
plantFilterInstance = PlantFilter {
  required=True,
  plantIdFilter=(Just $ defaultScalar {
    isEq = fromText "60972711-b4f9-46ce-bbc6-6fb7a788a636"
  } )
}

---- Structure to query PART:
-- | The top function once your query struct is finished
-- | to gather the actual results
-- | Beware, this is only the encoder/decoder struct
globalStructureToListStatement :: SQLStructure encoder decoder -> Statement encoder [decoder]
globalStructureToListStatement struct = Statement (encodeUtf8 sql) encodeVal decodeVal True
  where 
    sql = globalStructureToQuery struct
    encodeVal = statementEncoder struct
    singleComposite = Decoders.nonNullable $ Decoders.composite $ statementDecoder struct
    compositeDecoder = Decoders.column $ Decoders.nonNullable $ Decoders.listArray $ singleComposite
    decodeVal = Decoders.singleRow compositeDecoder

-- | Takes an SQLStructure and yield the resulting query
globalStructureToQuery :: SQLStructure encoder decoder -> Text
globalStructureToQuery struct = pureSyntaxAssembler globalSelect (fromPart struct) joinRes whereRes groupRes (offset struct) limitVal
  where
    globalSelect = wrapSelectionResult wrapList selectRes
    wrapList = wrapFunctionList struct
    selectRes = T.intercalate ", " (selectPart struct)
    joinRes = T.intercalate "\n" (joinList struct)
    whereRes = whereConditions struct
    groupRes = T.intercalate ", " (groupByList struct)
    limitVal = limit struct

pureSyntaxAssembler :: Text -> Text -> Text -> Text -> Text -> Int -> Int -> Text
pureSyntaxAssembler select from join whereRes group offsetValue limitValue = finalRes
    where
      formatter = " "
      finalRes = selectFrom <> formatter <> joinWhere <> groupRes <> limitRes
      selectFrom = "SELECT " <> select <> formatter <> "FROM " <> from
      joinWhere = join <> if whereRes == "" then "" else formatter <> "WHERE " <> whereRes
      groupRes = if group == "" then "" else formatter <> "GROUP BY " <> group
      limitRes = if limitValue > 0 then formatter <> "LIMIT " <> (T.pack $ show limitValue) else ""

-- | Takes a list of wrapping funtion (row or array_agg) and yield the compound
wrapSelectionResult :: [Text] -> Text -> Text
wrapSelectionResult wrapList selectResult = foldr wrapperNameApplication selectResult wrapList
  where
    wrapperNameApplication wrapFunc acc = wrapFunc <> "(" <> acc <> ")"



------- Merge two SQLStructure ?? with an UPDATER...


mergeSubqueryList :: (Text, Text) -> (typeA -> [typeInA] -> typeA) -> SQLStructure encoderTypeInA typeInA -> SQLStructure encoder typeA -> SQLStructure encoder typeA
mergeSubqueryList (whereCond, groupCond) updater valueInA valueA = valueA {
  selectPart = selectPart valueA <> ["(" <> globalStructureToQuery adaptedValueInA <> ")"],
  statementDecoder = do
    entity <- statementDecoder valueA
    updater entity <$> (Decoders.field (Decoders.nonNullable $ Decoders.listArray $ Decoders.nonNullable $ Decoders.composite (statementDecoder adaptedValueInA)))
}
  where adaptedValueInA = valueInA {
    wrapFunctionList = ["array_agg"::Text] <> (wrapFunctionList valueInA),
    whereConditions = whereCond,
    groupByList = [groupCond]
  }

generateQuery :: (ValidSelectionSet -> SQLStructure a b) -> ValidSelection -> SQLStructure a b
generateQuery typeHandler nexSelSet = case nexSelSet of
  Selection _ _ _ (SelectionSet selectionSet) -> typeHandler selectionSet
  Selection _ position _ _ -> error $ ("unexpected selection set at position " ++ show position)

-- une fois que j'ai ça,
-- je dois l'intégrer dans la query "parente" donc: ajouter une condition where
-- puis => textifier la query la mettre dans la liste des selectPart du parent
-- et composer les decoder (liste versus pas liste ?)

-- | integrates a subquery in a parent query
mergeSubquery :: (decoderB -> decoderInB -> decoderB) -> SQLStructure encoder decoderInB -> SQLStructure encoder decoderB -> SQLStructure encoder decoderB
mergeSubquery updater structInB structB = structB {
    selectPart = selectPart structB <> [globalStructureToQuery structInB],
    statementDecoder = do
      entity <- statementDecoder structB
      updater entity <$> (Decoders.field (Decoders.nonNullable $ Decoders.composite $ (statementDecoder structInB)))
  }

-- | This should serve as an API Field "pack" which serves for every correspondance between an API field
-- | and pretty much everything else: the selects, the filters and the
-- | inserts ?
data MassaliaField a b = MassaliaField {
  name :: Text,
  filterEncoder :: Text, --TODO
  decoder :: Text, -- TODO
  haskellValue :: a, -- TODO
  hsUpdater :: b -> a -> b -- TODO
}
  
okok :: MassaliaField UUID PlantFilter
okok = MassaliaField {}