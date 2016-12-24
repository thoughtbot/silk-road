module Item exposing (Item(..), position, all, minPrice, maxPrice)

import List.Extra exposing (elemIndex)
import Currency exposing (Currency(..))


type Item
    = Cocaine
    | Heroin
    | Acid
    | Weed
    | Speed
    | Ludes


position : Item -> Int
position item =
    elemIndex item all |> Maybe.withDefault 0


all : List Item
all =
    [ Cocaine
    , Heroin
    , Acid
    , Weed
    , Speed
    , Ludes
    ]


maxPrice : Item -> Currency
maxPrice item =
    case item of
        Cocaine ->
            Currency 30000

        Heroin ->
            Currency 14000

        Acid ->
            Currency 4500

        Weed ->
            Currency 900

        Speed ->
            Currency 250

        Ludes ->
            Currency 60


minPrice : Item -> Currency
minPrice item =
    case item of
        Cocaine ->
            Currency 15000

        Heroin ->
            Currency 5000

        Acid ->
            Currency 1000

        Weed ->
            Currency 300

        Speed ->
            Currency 70

        Ludes ->
            Currency 10
