module Main exposing (..)

import Dict exposing (Dict)
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
    Dict Drug Dollar


type Dollar
    = Dollar Int


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
    Dict Drug Int


type alias Inventory =
    { drugs : Stash
    , maxHolding : Int
    , guns : Int
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


model : Model
model =
    Model
        Manhattan
        Dict.empty
        (Dollar 2000)
        (Inventory Dict.empty 100 0)
        Dict.empty
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
    div [] [ text "hello world" ]
