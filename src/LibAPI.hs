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
                                                , ResolveQ
                                                , liftEither
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
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( SelectionRec(..)
                                                , ValidSelection
                                                , SelectionSet
                                                )
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
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( SelectionRec(..), ValidSelection, SelectionSet, Selection(..), Arguments )

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

compo :: (SelectionSet -> SQLStructure encoder typeInA) -> (Text, Text) -> (typeA -> [typeInA] -> typeA)
  -> Selection Arguments SelectionRec -> SQLStructure encoder typeA -> SQLStructure encoder typeA
compo generator conditions updater selSet = subQueryMerger $ resolver selSet
  where
    resolver = generateQuery generator
    subQueryMerger = mergeSubqueryList conditions updater

data JoinStructure = JoinStructure {
  joinTargetAlias :: Text,
  joinTarget :: Text,
  joinCondition :: Text
}

data TargetableType = From | Join | LeftJoin
data Target = Target {
  targetType :: TargetableType,
  statement :: Text  
}

data GQLScalarFilter scalarType = GQLScalarFilter {
  isEq :: Maybe scalarType,
  isNotEq :: Maybe scalarType,
  isIn :: Maybe [scalarType],
  isNotIn :: Maybe [scalarType]
}

data PlantFilter = PlantFilter {
  plantId :: Maybe (GQLScalarFilter UUID)
}



---- Structure to query PART:

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
globalStructureToQuery struct = pureSyntaxAssembler globalSelect (fromPart struct) joinRes whereRes groupRes
  where
    globalSelect = wrapSelectionResult wrapList selectRes
    wrapList = wrapFunctionList struct
    selectRes = T.intercalate ", " (selectPart struct)
    joinRes = T.intercalate "\n" (joinList struct)
    whereRes = whereConditions struct
    groupRes = T.intercalate ", " (groupByList struct)

pureSyntaxAssembler :: Text -> Text -> Text -> Text -> Text -> Text
pureSyntaxAssembler select from join whereRes group = finalRes
    where
      formatter = " "
      finalRes = selectFrom <> formatter <> joinWhere <> if group == "" then "" else formatter <> groupRes
      selectFrom = "SELECT " <> select <> formatter <> "FROM " <> from
      joinWhere = join <> if whereRes == "" then "" else formatter <> "WHERE " <> whereRes
      groupRes = "GROUP BY " <> group

-- | Takes a list of wrapping funtion (row or array_agg) and yield the compound
wrapSelectionResult :: [Text] -> Text -> Text
wrapSelectionResult wrapList selectResult = foldr wrapperNameApplication selectResult wrapList
  where
    wrapperNameApplication wrapFunc acc = wrapFunc <> "(" <> acc <> ")"



------- Merge two SQLStructure ?? with an UPDATER...


mergeSubqueryList :: (Text, Text) -> (typeA -> [typeInA] -> typeA) -> SQLStructure encoder typeInA -> SQLStructure encoder typeA -> SQLStructure encoder typeA
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

generateQuery :: (SelectionSet -> SQLStructure a b) -> Selection Arguments SelectionRec -> SQLStructure a b
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
