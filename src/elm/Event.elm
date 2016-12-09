module Event exposing (Event(..))

import Drug exposing (Drug)


type Event
    = None
    | PriceHike Drug Int
