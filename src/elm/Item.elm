module Item exposing (Item(..), position, all, minPrice, maxPrice)

import List.Extra exposing (elemIndex)
import Currency exposing (Currency(..))


type Item
    = Item1
    | Item2
    | Item3
    | Item4
    | Item5
    | Item6


position : Item -> Int
position item =
    elemIndex item all |> Maybe.withDefault 0


all : List Item
all =
    [ Item1
    , Item2
    , Item3
    , Item4
    , Item5
    , Item6
    ]


maxPrice : Item -> Currency
maxPrice item =
    case item of
        Item1 ->
            Currency 30000

        Item2 ->
            Currency 14000

        Item3 ->
            Currency 4500

        Item4 ->
            Currency 900

        Item5 ->
            Currency 250

        Item6 ->
            Currency 60


minPrice : Item -> Currency
minPrice item =
    case item of
        Item1 ->
            Currency 15000

        Item2 ->
            Currency 5000

        Item3 ->
            Currency 1000

        Item4 ->
            Currency 300

        Item5 ->
            Currency 70

        Item6 ->
            Currency 10
