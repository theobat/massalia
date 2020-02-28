{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSQL where

-- import Text.Inflections (toUnderscore)
-- import Data.Text (Text)
-- import Data.Maybe (fromMaybe)
-- import qualified Data.Text as T
-- import Data.UUID
-- import qualified Hasql.Decoders as Decoders
-- import qualified Hasql.Encoders as Encoders
-- import MassaliaCore (MassaliaStruct(..))
-- import Data.Morpheus.Types.Internal.AST.Selection
--   ( Arguments,
--     Selection (..),
--     SelectionContent (SelectionField, SelectionSet),
--     SelectionSet,
--     ValidSelection,
--     ValidSelectionSet,
--   )

-- data SelectStruct encoder decoder = SelectStruct {
--   wrapFunctionList :: [Text], -- either: "row" or "array_agg", "row"
--   selectPart :: [Text],
--   fromPart :: Text,
--   joinList :: [Text],
--   whereConditions :: Text,
--   groupByList :: [Text],
--   offset :: Int,
--   limit :: Int,
--   statementEncoder :: Encoders.Params encoder,
--   statementDecoder :: Decoders.Composite decoder
-- }

-- defaultSQLStruct = SelectStruct {
--   wrapFunctionList=[], -- either: "row" or "array_agg", "row"
--   selectPart=[],
--   fromPart="",
--   joinList=[],
--   whereConditions="",
--   groupByList=[],
--   offset=0,
--   limit=0,
--   statementEncoder=Encoders.noParams,
--   statementDecoder=undefined
-- }  

-- -- join semantics => join two SelectStruct
-- -- subquery semantics => select a SelectStruct within another

-- scalar :: Text -> (decoder -> a -> decoder) -> Decoders.Value a -> SelectStruct encoder decoder -> SelectStruct encoder decoder
-- scalar col updater decoderType sqlST = sqlST {
--   selectPart = selectPart sqlST <> [col],
--   statementDecoder = do
--     entity <- statementDecoder sqlST
--     updater entity <$> Decoders.field (Decoders.nonNullable decoderType)
-- }

-- instance MassaliaStruct SelectStruct encoder decoder where
--   getInitialValue (_, fromPart) defaultRecord = defaultSQLStruct {
--     wrapFunctionList = ["row"],
--     fromPart=fromPart,
--     statementDecoder = pure defaultRecord,
--     statementEncoder = undefined
--   }
--   simpleCol col updater _ = scalar (case toUnderscore col of
--       Left _ -> error "Failed at transforming field name to underscore = " <> col
--       Right e -> e
--     ) updater
--   exprCol (_, sqlName) updater _ = scalar sqlName updater
--   subColList = compo

-- compo ::
--   Monoid (wrapInA typeInA) =>
--   (Decoders.NullableOrNot Decoders.Value typeInA -> Decoders.Value (wrapInA typeInA)) ->
--   (ValidSelectionSet -> SelectStruct encoderTypeInA typeInA) ->
--   (Text, Text) ->
--   (typeA -> wrapInA typeInA -> typeA) ->
--   ValidSelection ->
--   SelectStruct encoder typeA ->
--   SelectStruct encoder typeA
-- compo decoder generator conditions updater selSet = subQueryMerger $ resolver selSet
--   where
--     resolver = generateQuery generator
--     subQueryMerger = mergeSubqueryList decoder conditions (\e v -> updater e (fromMaybe mempty v))

-- generateQuery :: (ValidSelectionSet -> SelectStruct a b) -> ValidSelection -> SelectStruct a b
-- generateQuery typeHandler nexSelSet = case nexSelSet of
--   Selection _ _ _ (SelectionSet selectionSet) -> typeHandler selectionSet
--   Selection _ position _ _ -> error $ ("unexpected selection set at position " ++ show position)

-- mergeSubqueryList ::
--   (Decoders.NullableOrNot Decoders.Value typeInA -> Decoders.Value (wrapInA typeInA)) ->
--   (Text, Text) ->
--   (typeA -> Maybe (wrapInA typeInA) -> typeA) ->
--   SelectStruct encoderTypeInA typeInA ->
--   SelectStruct encoder typeA ->
--   SelectStruct encoder typeA
-- mergeSubqueryList decoder (whereCond, groupCond) updater valueInA valueA = valueA {
--   selectPart = selectPart valueA <> ["(" <> globalStructureToQuery adaptedValueInA <> ")"],
--   statementDecoder = do
--     entity <- statementDecoder valueA
--     updater entity <$> (Decoders.field (Decoders.nullable $ decoder $ Decoders.nonNullable $ Decoders.composite (statementDecoder adaptedValueInA)))
-- }
--   where adaptedValueInA = valueInA {
--     wrapFunctionList = ["array_agg"::Text] <> (wrapFunctionList valueInA),
--     whereConditions = whereCond,
--     groupByList = [groupCond]
--   }

-- -- | Takes an SelectStruct and yield the resulting query
-- globalStructureToQuery :: SelectStruct encoder decoder -> Text
-- globalStructureToQuery struct = pureSyntaxAssembler globalSelect (fromPart struct) joinRes whereRes groupRes (offset struct) limitVal
--   where
--     globalSelect = wrapSelectionResult wrapList selectRes
--     wrapList = wrapFunctionList struct
--     selectRes = T.intercalate ", " (selectPart struct)
--     joinRes = T.intercalate "\n" (joinList struct)
--     whereRes = whereConditions struct
--     groupRes = T.intercalate ", " (groupByList struct)
--     limitVal = limit struct

-- pureSyntaxAssembler :: Text -> Text -> Text -> Text -> Text -> Int -> Int -> Text
-- pureSyntaxAssembler select from join whereRes group offsetValue limitValue = finalRes
--     where
--       formatter = " "
--       finalRes = selectFrom <> formatter <> joinWhere <> groupRes <> limitRes
--       selectFrom = "SELECT " <> select <> formatter <> "FROM " <> from
--       joinWhere = join <> if whereRes == "" then "" else formatter <> "WHERE " <> whereRes
--       groupRes = if group == "" then "" else formatter <> "GROUP BY " <> group
--       limitRes = if limitValue > 0 then formatter <> "LIMIT " <> (T.pack $ show limitValue) else ""

-- -- | Takes a list of wrapping funtion (row or array_agg) and yield the compound
-- wrapSelectionResult :: [Text] -> Text -> Text
-- wrapSelectionResult wrapList selectResult = foldr wrapperNameApplication selectResult wrapList
--   where
--     wrapperNameApplication wrapFunc acc = wrapFunc <> "(" <> acc <> ")"


-- data SimpleInsertStruct = SimpleInsertStruct {
--   insertInto :: Text,
--   insertColumnList :: [Text]
-- }

-- data MutateStruct = Insert (SimpleInsertStruct) | Update (SimpleInsertStruct)

-- data CTEMutateStruct wrapper encoder decoder = CTEInsertStruct {
--   insertList :: wrapper MutateStruct,
--   selectStruct :: Text
-- }
