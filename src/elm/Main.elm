module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)
import List.Extra exposing (elemIndex)


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


initialPrices : Prices
initialPrices =
    [ ( Cocaine, Dollar 15000 )
    , ( Heroin, Dollar 5000 )
    , ( Acid, Dollar 1000 )
    , ( Weed, Dollar 300 )
    , ( Speed, Dollar 70 )
    , ( Ludes, Dollar 10 )
    ]
        |> AllDict.fromList drugPosition



-- MODEL


type alias Prices =
    AllDict Drug Dollar Int


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


drugPosition : Drug -> Int
drugPosition drug =
    elemIndex drug drugs |> Maybe.withDefault 0


type DrugQuantity
    = DrugQuantity Int


type alias DrugHolding =
    ( Drug, DrugQuantity )


type alias DrugCollection =
    AllDict Drug DrugQuantity Int


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


emptyAllDict : AllDict Drug b Int
emptyAllDict =
    AllDict.empty drugPosition


model : Model
model =
    Model
        Manhattan
        initialPrices
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
        , displayCashOnHand model.cashOnHand
        , displayTrenchCoat model.trenchCoat
        , displayCurrentPrices model.currentPrices
        ]


displayCashOnHand : Dollar -> Html a
displayCashOnHand dollar =
    div [] [ text <| "Cash on hand: " ++ displayDollars dollar ]


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
    , dd [] [ text <| displayDrugQuantity (totalDrugs inventory.drugs) inventory.maxHolding ]
    ]


displayDrugQuantity : DrugQuantity -> Int -> String
displayDrugQuantity (DrugQuantity count) maxHolding =
    (toString <| maxHolding - count) ++ "/" ++ (toString maxHolding)


totalDrugs : DrugCollection -> DrugQuantity
totalDrugs =
    DrugQuantity << AllDict.foldl (\_ (DrugQuantity count) acc -> acc + count) 0


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
        |> Maybe.withDefault (DrugQuantity 0)
        |> (,) drug


displayDrug : DrugHolding -> List (Html a)
displayDrug ( drug, DrugQuantity count ) =
    [ dt [] [ text <| toString drug ]
    , dd [] [ text <| toString count ]
    ]


displayLocation : Location -> Html a
displayLocation location =
    div []
        [ text <| "Current location: " ++ toString location ]


displayCurrentPrices : Prices -> Html a
displayCurrentPrices prices =
    div []
        [ dl [] (List.concatMap displayPrice <| AllDict.toList prices)
        ]


displayPrice : ( Drug, Dollar ) -> List (Html a)
displayPrice ( drug, dollar ) =
    [ dt [] [ text <| toString drug ]
    , dd [] [ text <| displayDollars dollar ]
    ]


displayDollars : Dollar -> String
displayDollars (Dollar amount) =
    "$" ++ toString amount
