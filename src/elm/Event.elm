module Event exposing (Event(..))

import Drug exposing (Drug)
import DrugQuantity exposing (DrugQuantity)


type Event
    = None
    | PriceHike Drug Int
    | PriceDrop Drug Int
    | FindDrug Drug DrugQuantity
    | Mugging
    | DropDrug Drug Int
