{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Criterion.Main
import MassaliaSchema.TestAPI (apiWithoutDB)
import Data.Morpheus.Types (GQLRequest (..), GQLResponse(..))
import Data.Aeson (ToJSON(..))
import Protolude

-- Our benchmark harness.
main =
  defaultMain
    [ bgroup
        "sqlFromGql"
        [ bench "1" $ nfIO $ sqlFromGql 1,
          bench "2" $ nfIO $ sqlFromGql 2,
          bench "5" $ nfIO $ sqlFromGql 5
        ]
    ]

sqlFromGql repeatedQ = do 
  let gFilter = "checkDate: { isNotIn: [ \"2012-07-21\" ], isBetween: {start: \"1991-08-21\", end: \"2077-01-01\" } } "
  let mainQuery = " plantListQueryGenerator (first: 10, offset: 0, globalFilter: { " <> gFilter <> " }) { id name truckList { id }} "
  let queryQ s = "query plantList_test { " <> s <> " }"
  let queryStruct = GQLRequest
        { query = queryQ (mconcat $ replicate repeatedQ mainQuery),
          operationName = Nothing,
          variables = Nothing
        }
  res <- liftIO $ apiWithoutDB queryStruct
  case res of
    Data a -> pure $ toJSON a
    _ -> print res >> panic "error"
  