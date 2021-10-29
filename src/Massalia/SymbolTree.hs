{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StarIsType #-}

{-# LANGUAGE UndecidableInstances #-}
module Massalia.SymbolTree
where

import GHC.TypeLits (Symbol)
import Data.Text ( Text )
import Data.Type.Bool (If)
import Data.Type.Equality (type (:~:), type (==))

data SymbolTree = Leaf Symbol | Tree Symbol [SymbolTree]

-- -- type level var
-- type MySelection = 'Tree "nowYouKnow" ' ['Leaf "asdasd"]

-- type family FindField (a :: Symbol) (b :: Symbol) c where
  

-- type family Field (a :: SymbolTree) (b :: Symbol) c where
--   Field ('Leaf leafV) b c = If (leafV == b) c ()
--   Field ('Tree _ leafV) b c = If (leafV == b) c ()

-- data Ok (a :: SymbolTree) = Ok {
--   okok :: Field a "okok" Int,
--   nowYouKnow :: Field a "nowYouKnow" Ojj
-- }
-- data Ojj (a :: SymbolTree) = Ojj {
--   asdasd :: Field a "asdasd" Int,
--   dd :: Field a "dd" Text
-- }

-- test :: Ok ('Tree ' [])
-- test = Ok () ()


  