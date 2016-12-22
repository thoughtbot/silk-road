module Event exposing (Event(..))

import Item exposing (Item)
import ItemQuantity exposing (ItemQuantity)


type Event
    = None
    | PriceHike Item Int
    | PriceDrop Item Int
    | FindItem Item ItemQuantity
    | Mugging
    | DropItem Item Int
