module Drug exposing (Drug(..), drugPosition, all, minPrice, maxPrice)

import List.Extra exposing (elemIndex)
import Dollar exposing (Dollar(..))


type Drug
    = Cocaine
    | Heroin
    | Acid
    | Weed
    | Speed
    | Ludes


drugPosition : Drug -> Int
drugPosition drug =
    elemIndex drug all |> Maybe.withDefault 0


all : List Drug
all =
    [ Cocaine
    , Heroin
    , Acid
    , Weed
    , Speed
    , Ludes
    ]


maxPrice : Drug -> Dollar
maxPrice drug =
    case drug of
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


minPrice : Drug -> Dollar
minPrice drug =
    case drug of
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
