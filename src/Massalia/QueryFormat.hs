{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}

-- |
-- Module      : Massalia.QueryFormat
-- Description : A module to define a query format __interface__ for SQL queries. In Hasql, we use dynamic queries
--  because we don't know in advance what parameters will be provided or not by the graphql query. The query format
--  provided is made for communicating efficiently with Postgres so it has no instance of 'Show' or 'Eq' which can be
--  a problem for testing and for debugging. This module defined a typeclass to abstract whatever Type is used for the
--  query formatting.
module Massalia.QueryFormat
  ( QueryFormat(sqlEncode),
    formattedColName,
    SQLEncoder (wrapEncoding, ignoreInGenericInstance, textEncode, binaryEncode, polyEncode),
    SQLDecoder(sqlDecoder, sqlExpr),
    fmapList,
    fmapVector,
    DecodeOption(..),
    DecodeFieldPrefixType(..),
    DecodeTuple (DecodeTuple),
    MassaliaContext(getDecodeOption, setDecodeOption),
    defaultDecodeTuple,
    refineDecoder,
    FromText(fromText),
    IsString(fromString),
    DefaultParamEncoder,
    BinaryQuery,
    TextQuery,
    commaAssemble,
    (§),
    (°),
    joinEq,
    simpleEq,
    Snippet.param,
    inSingleQuote,
    inParens,
    commaSepInParens,
    decodeName,
    decodeNameInContext
  )
where
import Massalia.SelectionTree (MassaliaTree(getName))
import qualified Data.Map as Map
import Data.String (String)
import qualified Data.String as String (IsString (fromString))
import Data.Text (pack, replace, unpack)
import Data.UUID hiding (fromText, fromString)
import Data.Vector (Vector)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Implicits.Encoders (DefaultParamEncoder())
import Massalia.Utils (
    EmailAddress, LocalTime, ZonedTime,
    Day, Scientific, UTCTime,
    intercalate, intercalateMap, emailToText,
    emailValidateText,
    unsafeSnakeCaseT
  )
import Data.Time.LocalTime (zonedTimeToUTC)
import qualified Hasql.Decoders as Decoders
import Protolude hiding (intercalate, replace)

-- | The text query format is the canonical representation for debugging
-- and printing queries.
type TextQuery = Text

-- | The binary query format is the internal query representation in Hasql.
-- It's called 'Snippet' and it's defined in "Hasql.DynamicStatements.Snippet".
-- It enables the representation of a query alongside it's parametrized values.
-- It has no instance of show, which explains the presence of its 'TextQuery' counterpart.
type BinaryQuery = Snippet

-- | The ° operator is taking a table name (or an sql alias) and a field name (or a field alias) and
-- yields their escaped combination.
-- 
-- Examples:
-- >>> "foo" ° "bar"
-- \"fooo\".\"bar\"
(°) :: (Semigroup a, IsString a) => a -> a -> a
(°) a b = "\"" <> a <> "\".\"" <> b <> "\""


joinEq :: (Semigroup a, IsString a) => a -> a -> a -> a -> a
joinEq tableA fieldA tableB fieldB = "JOIN \"" <> tableA <> "\" ON " <> joinCondition
  where joinCondition = simpleEq tableA fieldA tableB fieldB
simpleEq :: (Semigroup a, IsString a) => a -> a -> a -> a -> a
simpleEq tableA fieldA tableB fieldB = (tableA ° fieldA) <> " = " <> (tableB ° fieldB)

(§) :: (IsString content, Monoid content) => content -> content -> content
(§) a b = a <> "," <> b

commaAssemble :: (IsString a, Monoid a) => [a] -> a
commaAssemble = intercalateMap identity ","

class (
    FromText queryFormat, IsString queryFormat, Monoid queryFormat
  ) => QueryFormat queryFormat where
    sqlEncode :: (SQLEncoder a) => a -> queryFormat

instance QueryFormat TextQuery where
  sqlEncode = textEncode
instance QueryFormat String where
  sqlEncode = unpack . textEncode
instance QueryFormat BinaryQuery where
  sqlEncode = binaryEncode

class FromText content where
  fromText :: Text -> content
instance FromText Text where
  fromText = identity
instance FromText String where
  fromText = unpack
instance FromText Snippet where
  fromText = String.fromString . unpack

class SQLEncoder dataT where
  ignoreInGenericInstance :: Bool
  ignoreInGenericInstance = False
  wrapEncoding :: (QueryFormat qf) => qf -> qf
  wrapEncoding = identity
  polyEncode :: (QueryFormat qf) => Maybe (dataT -> qf)
  polyEncode = Nothing
  textEncode :: dataT -> TextQuery
  default textEncode :: (Show dataT) => dataT -> TextQuery
  textEncode = fromMaybe (inSingleQuote . pack . show) (polyEncode @dataT @TextQuery)
  binaryEncode :: dataT -> BinaryQuery
  default binaryEncode :: (DefaultParamEncoder dataT) => dataT -> BinaryQuery
  binaryEncode = fromMaybe (Snippet.param) (polyEncode @dataT @BinaryQuery)

voidMessage :: Text
voidMessage = " from void cannot happen because Void has no inhabitant and sqlEncode expect Void -> queryFormat"
instance SQLEncoder Void where
  textEncode = panic $ "text" <> voidMessage
  binaryEncode = panic $ "binary" <> voidMessage

instance (Show a, SQLEncoder a, DefaultParamEncoder a) => SQLEncoder (Maybe a) where
  textEncode = (wrapEncoding @a) . maybe "null" textEncode
  binaryEncode = (wrapEncoding @a) . maybe "null" binaryEncode

instance (DefaultParamEncoder [a], Show a, SQLEncoder a) => SQLEncoder [a] where
  textEncode = collectionTextEncode
instance (DefaultParamEncoder (Set a), Show a, SQLEncoder a) => SQLEncoder (Set a) where
  textEncode = collectionTextEncode
instance (DefaultParamEncoder (Vector a), Show a, SQLEncoder a) => SQLEncoder (Vector a) where
  textEncode = collectionTextEncode
instance (DefaultParamEncoder (Seq a), Show a, SQLEncoder a) => SQLEncoder (Seq a) where
  textEncode = collectionTextEncode

instance SQLEncoder UUID -- all default
instance SQLEncoder Day -- all default
instance SQLEncoder LocalTime -- all default
instance SQLEncoder Text where
  textEncode = inSingleQuote
instance SQLEncoder Int64 where
  textEncode = pack . show
instance SQLEncoder Int where
  textEncode = pack . show
  binaryEncode = Snippet.param . (fromIntegral @Int @Int64)
instance SQLEncoder ZonedTime where
  binaryEncode = Snippet.param . zonedTimeToUTC
instance SQLEncoder EmailAddress where
  textEncode = inSingleQuote . emailToText
  binaryEncode = Snippet.param . emailToText 

inSingleQuote :: (Semigroup a, IsString a) => a -> a
inSingleQuote a = "'" <> a <> "'"
inParens :: (Semigroup a, IsString a) => a -> a
inParens a = "(" <> a <> ")"
commaSepInParens :: [[Char]] -> [Char]
commaSepInParens = inParens . (intercalate ",")

collectionTextEncode :: (Foldable collection, SQLEncoder dataT, Show dataT) =>
  collection dataT -> Text
collectionTextEncode collection = wrapCollection assembled
  where
    assembled = foldr assemblerFun "" collection
    assemblerFun :: (SQLEncoder b, Show b) => b -> Text -> Text
    assemblerFun val "" = rawEncode val
    assemblerFun val currentEncoded = currentEncoded <> "," <> rawEncode val
    rawEncode val = replace "'" "" $ textEncode val
    wrapCollection a = "'{" <> a <> "}'"

formattedColName :: (QueryFormat qf) => DecodeFieldPrefixType -> Maybe qf -> qf -> qf
formattedColName _ Nothing colName = "\"" <> colName <> "\""
formattedColName CompositeField (Just prefixName) colName = "(" <> prefixName <> ").\"" <> colName <> "\""
formattedColName TableName (Just prefixName) colName = prefixName ° colName

------------------------- Decoder stuff

defaultDecodeExpr :: (QueryFormat queryFormat, MassaliaTree selection, MassaliaContext contextT) =>
  contextT -> selection -> (Text -> queryFormat)
defaultDecodeExpr context input = firstRes
  where
    firstRes rawName = formattedColName (fieldPrefixType decOption) (Just $ tablename rawName) colName
    tablename rawName = fromText $ decodeName decOption rawName
    colName = fromText $ unsafeSnakeCaseT $ getName input
    decOption = fromMaybe mempty (getDecodeOption context)

data DecodeTuple decodedT = DecodeTuple {
  decValue :: Decoders.Value decodedT,
  decNValue :: Decoders.Value decodedT -> Decoders.NullableOrNot Decoders.Value decodedT
}
defaultDecodeTuple :: Decoders.Value decodedT -> DecodeTuple decodedT
defaultDecodeTuple value = DecodeTuple {
  decValue = value,
  decNValue = Decoders.nonNullable
}
refineDecoder :: (a -> Either Text b) -> DecodeTuple a -> DecodeTuple b
refineDecoder refiner (DecodeTuple dec _) = result
  where result = DecodeTuple (Decoders.refine refiner dec) Decoders.nonNullable
fmapDecoderValue :: (Decoders.Value a -> Decoders.Value b) -> DecodeTuple a -> DecodeTuple b
fmapDecoderValue refiner (DecodeTuple dec _) = result
  where result = DecodeTuple (refiner dec) Decoders.nonNullable

instance Functor DecodeTuple where
  fmap typeChanger (DecodeTuple dec _) = DecodeTuple (typeChanger <$> dec) Decoders.nonNullable

data DecodeFieldPrefixType = TableName | CompositeField deriving (Show)
data DecodeOption = DecodeOption {
  nameMap :: Map Text Text,
  fieldPrefixType :: DecodeFieldPrefixType
} deriving (Show)
instance Semigroup DecodeOption where
  (<>) a b = DecodeOption {
      nameMap = nameMap a <> nameMap b,
      fieldPrefixType = fieldPrefixType a
    }
instance Monoid DecodeOption where
  mempty = DecodeOption {
    nameMap = mempty,
    fieldPrefixType = TableName
  }

-- | When using an overridable name sue this function
-- before using it.
decodeName :: DecodeOption -> Text -> Text
decodeName decOpt name = fromMaybe name (Map.lookup name $ nameMap decOpt)
-- | When using an overridable name in a given context 'a'
-- use this function before using the name.
decodeNameInContext :: MassaliaContext a => a -> Text -> Text
decodeNameInContext context name = maybe name (flip decodeName $ name) (getDecodeOption context)
  

-- | A class to decode
class SQLDecoder contextT decodedT where
  sqlExpr :: (
    QueryFormat qf,
    MassaliaTree treeNode,
    MassaliaContext contextT
    ) => contextT -> treeNode -> (Text -> qf, DecodeTuple decodedT)
  sqlExpr context selection = (defaultDecodeExpr context selection, sqlDecoder @contextT @decodedT)
  sqlDecoder :: DecodeTuple decodedT
  
fmapList :: forall contextT element. (SQLDecoder contextT element) => DecodeTuple [element]
fmapList = fmapDecoderValue (Decoders.listArray . Decoders.nonNullable) (sqlDecoder @contextT @element)
fmapVector :: forall contextT element. (SQLDecoder contextT element) => DecodeTuple (Vector element)
fmapVector = fmapDecoderValue (Decoders.vectorArray . Decoders.nonNullable) (sqlDecoder @contextT @element)
instance SQLDecoder contextT UUID where
  sqlDecoder = defaultDecodeTuple Decoders.uuid
instance SQLDecoder contextT [UUID] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector UUID) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT Text where
  sqlDecoder = defaultDecodeTuple Decoders.text
instance SQLDecoder contextT [Text] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector Text) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT Bool where
  sqlDecoder = defaultDecodeTuple Decoders.bool
instance SQLDecoder contextT [Bool] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector Bool) where
  sqlDecoder = fmapVector

instance SQLDecoder contextT EmailAddress where
  sqlDecoder = defaultDecodeTuple $ Decoders.custom $ const emailValidateText
instance SQLDecoder contextT [EmailAddress] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector EmailAddress) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT Int64 where
  sqlDecoder = defaultDecodeTuple $ Decoders.int8
instance SQLDecoder contextT [Int64] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector Int64) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT Int where
  sqlDecoder = defaultDecodeTuple (fromIntegral <$> Decoders.int8)
instance SQLDecoder contextT [Int] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector Int) where
  sqlDecoder = fmapVector

instance SQLDecoder contextT LocalTime where
  sqlDecoder = defaultDecodeTuple Decoders.timestamp
instance SQLDecoder contextT [LocalTime] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector LocalTime) where
  sqlDecoder = fmapVector

instance SQLDecoder contextT Day where
  sqlDecoder = defaultDecodeTuple Decoders.date
instance SQLDecoder contextT [Day] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector Day) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT Scientific where
  sqlDecoder = defaultDecodeTuple Decoders.numeric
instance SQLDecoder contextT [Scientific] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector Scientific) where
  sqlDecoder = fmapVector
instance SQLDecoder contextT UTCTime where
  sqlDecoder = defaultDecodeTuple Decoders.timestamptz
instance SQLDecoder contextT [UTCTime] where
  sqlDecoder = fmapList
instance SQLDecoder contextT (Vector UTCTime) where
  sqlDecoder = fmapVector

instance (
    SQLDecoder contextT a
  ) => SQLDecoder contextT (Maybe a) where
  sqlDecoder = action (sqlDecoder @contextT @a)
    where
      action (DecodeTuple decoder _) = DecodeTuple
        (const Nothing <$> decoder) (const $ Decoders.nullable decoder)

-- | This is the building bloc for the SQLSelect class, it creates the
-- FROM .. WHERE .. etc part of a SELECT SQL query based on a « context » object.
-- It enables the ability to pass, for instance, user roles, user data,
-- authentication information.
class MassaliaContext contextT where
  getDecodeOption :: contextT -> Maybe DecodeOption
  setDecodeOption :: DecodeOption -> contextT -> contextT