module Inventory exposing (..)

import AllDict exposing (AllDict)
import Drug exposing (Drug(..))
import DrugQuantity exposing (DrugQuantity(..))


type GunCount
    = GunCount Int


type alias DrugCollection =
    AllDict Drug DrugQuantity Int


type alias DrugHolding =
    ( Drug, DrugQuantity )


type alias Inventory =
    { drugs : DrugCollection
    , maxHolding : DrugQuantity
    , guns : GunCount
    }


quantityOfDrug : DrugCollection -> Drug -> DrugQuantity
quantityOfDrug drugCollection drug =
    AllDict.get drug drugCollection
        |> Maybe.withDefault (DrugQuantity 0)


lookupHolding : DrugCollection -> Drug -> DrugHolding
lookupHolding drugCollection drug =
    ( drug, quantityOfDrug drugCollection drug )


availableInventorySpace : Inventory -> DrugQuantity
availableInventorySpace inventory =
    DrugQuantity.subtract inventory.maxHolding (slotsUsed inventory.drugs)


slotsUsed : DrugCollection -> DrugQuantity
slotsUsed =
    DrugQuantity << AllDict.foldl (\_ (DrugQuantity count) acc -> acc + count) 0


emptyAllDict : AllDict Drug b Int
emptyAllDict =
    AllDict.empty Drug.drugPosition


empty : Inventory
empty =
    (Inventory emptyAllDict (DrugQuantity 100) (GunCount 0))
