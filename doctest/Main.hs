
module Main
  ( main
  )
where

import           Prelude

import           Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc", "src/Massalia/Filter.hs"]