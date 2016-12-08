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


type alias Stash =
    AllDict Drug Int String


type alias Inventory =
    { drugs : Stash
    , maxHolding : Int
    , guns : GunCount
    }


type alias Model =
    { currentLocation : Location
    , currentPrices : Prices
    , cashOnHand : Dollar
    , inventory : Inventory
    , stash : Stash
    , debt : Dollar
    , bankAccountBalance : Dollar
    }


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
        , displayInventory model.inventory
        ]


displayInventory : Inventory -> Html a
displayInventory inventory =
    dl []
        (displayGun inventory.guns ++ displayTrenchcoat inventory.maxHolding ++ displayDrugs inventory.drugs)


displayGun : GunCount -> List (Html a)
displayGun (GunCount guns) =
    [ dt [] [ text "Guns" ]
    , dd [] [ text <| toString guns ]
    ]


displayDrugs : Stash -> List (Html a)
displayDrugs =
    List.concatMap displayDrug << AllDict.toList


displayTrenchcoat : Int -> List (Html a)
displayTrenchcoat maxHolding =
    [ dt [] [ text "Trenchcoat" ]
    , dd [] [ text <| toString maxHolding ]
    ]


displayDrug : ( Drug, Int ) -> List (Html a)
displayDrug ( drug, count ) =
    [ dt [] [ text <| toString drug ]
    , dd [] [ text <| toString count ]
    ]


displayLocation : Location -> Html a
displayLocation location =
    div []
        [ text <| "Current location: " ++ toString location ]
