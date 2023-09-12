Recipe
##########


The library revolves around the idea that you can define a godd enough graph where nodes are tables with filters and vertices are meaningful relations of filtering.
For instance, we can define the following schema (it comes from the dataseed/MassaliaSchema folder in the repo, which is used for integration tests in the repo) : 

.. code-block:: haskell

  data Plant
    = Plant
        { id :: UUID,
          name :: Text,
          createdAt :: LocalTime,
          checkDate :: Day,
          description :: Maybe Text
        }
    deriving (Show, Generic)
  deriving instance SQLRecord (Paginated PlantFilter) Plant

  data PlantFilter = PlantFilter
    { id :: Maybe GQLFilterUUID,
      name :: Maybe GQLFilterText,
      checkDate :: Maybe GQLFilterDay
    }
    deriving
      ( Show,
        Generic,
        SQLFilter
      )
  data Truck
    = Truck
        { id :: UUID,
          vehicleId :: Text
        }
    deriving (Show, Generic, Eq)
  deriving instance SQLRecord (Paginated TruckFilter) Truck
  
  data TruckFilter
    = TruckFilter
        { id :: Maybe GQLFilterUUID,
          vehicleId :: Maybe GQLFilterText
        }
    deriving (Show, Generic, SQLFilter)
  
  instance SQLFilterField TruckFilter where
    filterStruct opts selection val = case selection of
      "exists_truck" -> Just $ handleExistFilter True opts selection val
      "not_exists_truck" -> Just $ handleExistFilter False opts selection val
      _ -> Nothing
      where
        handleExistFilter isExist = existsOrNotPrimitive isExist filterInside actualFilter
        filterInside = True
        actualFilter fatherTableName =
          ( mempty
              { _select = pure "1",
                _from = Just "truck",
                _where = Just condition
              },
            "exists_subquery_name"
          )
          where
            condition = ("truck" ° "plant_id") <> "=" <> (ftn ° "id")
            ftn = fromText fatherTableName


