{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Massalia.GenericUtils
  ( GTypeName(..),
    GSelectors(..)
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Proxy
import           GHC.Generics
import           Text.Read (readMaybe)
import           Type.Reflection

-- | Generic equivalent to `TypeName`.
class GTypeName f where
  gtypename :: String

-- instance (Datatype s) => GTypeName (M1 S s i) where
--   gtypename = datatypeName @s
-- instance (Datatype s) =>  GTypeName (M1 D s i) where
--   gtypename = datatypeName (undefined :: M1 D s i ())
-- instance (Datatype s) =>  GTypeName (M1 C s (K1 R t)) where
--   gtypename = datatypeName (undefined :: M1 C s (K1 R t) ())
instance (Datatype s) => GTypeName (M1 D s a) where
  gtypename = datatypeName (undefined :: M1 D s i ())
instance GTypeName (M1 C s a) where
  gtypename = ""
instance GTypeName (M1 S s a) where
  gtypename = ""
instance GTypeName U1 where
  gtypename = ""


---- This is the class to get the selectors of a record
class GSelectors rep where
  selectors :: [(String, SomeTypeRep)]

instance GSelectors f => GSelectors (M1 D x f) where
  selectors = selectors @f

instance GSelectors f => GSelectors (M1 C x f) where
  selectors = selectors @f

instance (Selector s, Typeable t) => GSelectors (M1 S s (K1 R t)) where
  selectors =
    [(selName (undefined :: M1 S s (K1 R t) ()) , SomeTypeRep (typeRep @t))]

instance (GSelectors a, GSelectors b) => GSelectors (a :*: b) where
  selectors = selectors @a ++ selectors @b

instance GSelectors U1 where
  selectors = []
