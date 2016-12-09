module Inventory exposing (..)

import AllDict exposing (AllDict)
import Drug exposing (Drug(..))
import DrugQuantity exposing (DrugQuantity(..))


type alias DrugCollection =
    AllDict Drug DrugQuantity Int


type alias DrugHolding =
    ( Drug, DrugQuantity )


type alias Inventory =
    { drugs : DrugCollection
    , maxHolding : DrugQuantity
    }


quantityOfDrug : DrugCollection -> Drug -> DrugQuantity
quantityOfDrug drugCollection drug =
    AllDict.get drug drugCollection
        |> Maybe.withDefault (DrugQuantity 0)


lookupHolding : DrugCollection -> Drug -> DrugHolding
lookupHolding drugCollection drug =
    ( drug, quantityOfDrug drugCollection drug )


addDrugs : Drug -> DrugQuantity -> Inventory -> Inventory
addDrugs drug quantity inventory =
    let
        newTotal =
            DrugQuantity.add quantity (quantityOfDrug inventory.drugs drug)
    in
        { inventory | drugs = AllDict.insert drug newTotal inventory.drugs }


removeAllDrug : Drug -> Inventory -> Inventory
removeAllDrug drug inventory =
    { inventory | drugs = AllDict.remove drug inventory.drugs }


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
    (Inventory emptyAllDict (DrugQuantity 100))
