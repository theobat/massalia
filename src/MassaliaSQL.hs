{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module MassaliaSQL
    ( SQLStructure(..)
    , defaultSQLStruct
    ) where

import Data.Text
import Data.UUID
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import MassaliaCore (MassaliaStruct(..))

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

-- join semantics => join two SQLStructure
-- subquery semantics => select a SQLStructure within another

scalar :: Text -> (decoder -> a -> decoder) -> Decoders.Value a -> SQLStructure encoder decoder -> SQLStructure encoder decoder
scalar col updater decoderType sqlST = sqlST {
  selectPart = selectPart sqlST <> [col],
  statementDecoder = do
    entity <- statementDecoder sqlST
    updater entity <$> Decoders.field (Decoders.nonNullable decoderType)
}

instance MassaliaStruct SQLStructure encoder decoder where
  simpleCol col updater _ = scalar col updater

test :: MassaliaStruct SQLStructure () decode => [SQLStructure () decoder]
test = [
    defaultSQLStruct,
    defaultSQLStruct
  ]
