module Drug exposing (Drug(..), drugPosition, all)

import List.Extra exposing (elemIndex)


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
