{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude
import qualified SpecGraphQLSelect
import qualified SpecStaticInsert
import qualified SpecStaticSelect
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" testList
  where
    testList = [staticSelect, graphqlSelect, staticInsert]
    staticSelect = SpecStaticSelect.unitTests
    staticInsert = SpecStaticInsert.unitTests
    graphqlSelect = SpecGraphQLSelect.unitTests
