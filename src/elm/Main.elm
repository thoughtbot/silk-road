module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Prices =
    AllDict Drug Dollar String


type Dollar
    = Dollar Int


type GunCount
    = GunCount Int


type Location
    = Manhattan
    | Bronx
    | Ghetto
    | ConeyIsland
    | CentralPark
    | Brooklyn


type Drug
    = Cocaine
    | Heroin
    | Acid
    | Weed
    | Speed
    | Ludes


type DrugCount
    = DrugCount Int


type alias DrugHolding =
    ( Drug, DrugCount )


type alias DrugCollection =
    AllDict Drug DrugCount String


type alias Inventory =
    { drugs : DrugCollection
    , maxHolding : Int
    , guns : GunCount
    }


type alias Model =
    { currentLocation : Location
    , currentPrices : Prices
    , cashOnHand : Dollar
    , trenchCoat : Inventory
    , stash : DrugCollection
    , debt : Dollar
    , bankAccountBalance : Dollar
    }


drugs : List Drug
drugs =
    [ Cocaine
    , Heroin
    , Acid
    , Weed
    , Speed
    , Ludes
    ]


emptyAllDict : AllDict a b String
emptyAllDict =
    AllDict.empty toString


model : Model
model =
    Model
        Manhattan
        emptyAllDict
        (Dollar 2000)
        (Inventory emptyAllDict 100 (GunCount 0))
        emptyAllDict
        (Dollar 5500)
        (Dollar 0)


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html a
view model =
    div []
        [ displayLocation model.currentLocation
        , displayTrenchCoat model.trenchCoat
        ]


displayTrenchCoat : Inventory -> Html a
displayTrenchCoat inventory =
    dl []
        (displayGun inventory.guns
            ++ displayAvailableSlots inventory
            ++ displayDrugs inventory.drugs
        )


displayAvailableSlots : Inventory -> List (Html a)
displayAvailableSlots inventory =
    [ dt [] [ text "Slots available" ]
    , dd [] [ text <| displayDrugCount (totalDrugs inventory.drugs) inventory.maxHolding ]
    ]


displayDrugCount : DrugCount -> Int -> String
displayDrugCount (DrugCount count) maxHolding =
    (toString <| maxHolding - count) ++ "/" ++ (toString maxHolding)


totalDrugs : DrugCollection -> DrugCount
totalDrugs =
    DrugCount << AllDict.foldl (\_ (DrugCount count) acc -> acc + count) 0


displayGun : GunCount -> List (Html a)
displayGun (GunCount guns) =
    [ dt [] [ text "Guns" ]
    , dd [] [ text <| toString guns ]
    ]


displayDrugs : DrugCollection -> List (Html a)
displayDrugs stash =
    List.concatMap (displayDrug << lookupHolding stash) drugs


lookupHolding : DrugCollection -> Drug -> DrugHolding
lookupHolding stash drug =
    AllDict.get drug stash
        |> Maybe.withDefault (DrugCount 0)
        |> (,) drug


displayDrug : DrugHolding -> List (Html a)
displayDrug ( drug, DrugCount count ) =
    [ dt [] [ text <| toString drug ]
    , dd [] [ text <| toString count ]
    ]


displayLocation : Location -> Html a
displayLocation location =
    div []
        [ text <| "Current location: " ++ toString location ]
