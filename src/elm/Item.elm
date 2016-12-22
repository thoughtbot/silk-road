module Item exposing (Item(..), position, all, minPrice, maxPrice)

import List.Extra exposing (elemIndex)
import Dollar exposing (Dollar(..))


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


maxPrice : Item -> Dollar
maxPrice item =
    case item of
        Cocaine ->
            Dollar 30000

        Heroin ->
            Dollar 14000

        Acid ->
            Dollar 4500

        Weed ->
            Dollar 900

        Speed ->
            Dollar 250

        Ludes ->
            Dollar 60


minPrice : Item -> Dollar
minPrice item =
    case item of
        Cocaine ->
            Dollar 15000

        Heroin ->
            Dollar 5000

        Acid ->
            Dollar 1000

        Weed ->
            Dollar 300

        Speed ->
            Dollar 70

        Ludes ->
            Dollar 10
