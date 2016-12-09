module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Dollar exposing (Dollar(..))
import Drug exposing (Drug(..))
import DrugQuantity exposing (DrugQuantity(..))
import Inventory exposing (GunCount(..), DrugCollection, Inventory, DrugHolding)


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
        |> AllDict.fromList Drug.drugPosition



-- MODEL


type alias Prices =
    AllDict Drug Dollar Int


type Location
    = Manhattan
    | Bronx
    | Ghetto
    | ConeyIsland
    | CentralPark
    | Brooklyn


type alias Model =
    { currentLocation : Location
    , currentPrices : Prices
    , cashOnHand : Dollar
    , trenchCoat : Inventory
    , stash : DrugCollection
    , debt : Dollar
    , bankAccountBalance : Dollar
    , daysRemaining : Int
    }


emptyAllDict : AllDict Drug b Int
emptyAllDict =
    AllDict.empty Drug.drugPosition


model : Model
model =
    Model Manhattan
        initialPrices
        (Dollar 2000)
        Inventory.empty
        emptyAllDict
        (Dollar 5500)
        Dollar.zero
        31


type Msg
    = NoOp
    | BuyMax Drug
    | SellAll Drug
    | TravelTo Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BuyMax drug ->
            ( buyMax model drug, Cmd.none )

        SellAll drug ->
            ( sellAll model drug, Cmd.none )

        TravelTo location ->
            ( { model | currentLocation = location, daysRemaining = model.daysRemaining - 1 }, Cmd.none )


sellAll : Model -> Drug -> Model
sellAll model drug =
    let
        multiplyThings (DrugQuantity quantity) (Dollar amount) =
            Dollar <| quantity * amount

        totalSalePrice =
            multiplyThings (Inventory.quantityOfDrug model.trenchCoat.drugs drug)
                (Maybe.withDefault Dollar.zero <| AllDict.get drug model.currentPrices)

        newTrenchcoat =
            Inventory.removeAllDrug drug model.trenchCoat
    in
        { model | cashOnHand = Dollar.add model.cashOnHand totalSalePrice, trenchCoat = newTrenchcoat }


buyMax : Model -> Drug -> Model
buyMax model drug =
    let
        purchaseableDrugQuantity_ =
            purchaseableDrugQuantity model drug

        totalPurchasePrice =
            multiplyThings purchaseableDrugQuantity_
                (Maybe.withDefault Dollar.zero <| AllDict.get drug model.currentPrices)

        multiplyThings (DrugQuantity quantity) (Dollar amount) =
            Dollar <| quantity * amount

        newTrenchcoat =
            Inventory.addDrugs drug purchaseableDrugQuantity_ model.trenchCoat
    in
        { model | cashOnHand = Dollar.subtract model.cashOnHand totalPurchasePrice, trenchCoat = newTrenchcoat }


purchaseableDrugQuantity : Model -> Drug -> DrugQuantity
purchaseableDrugQuantity model drug =
    Maybe.withDefault (DrugQuantity 0) <|
        DrugQuantity.minimum
            [ maxQuantityByPrice model.currentPrices model.cashOnHand drug
            , Inventory.availableInventorySpace model.trenchCoat
            ]


maxQuantityByPrice : Prices -> Dollar -> Drug -> DrugQuantity
maxQuantityByPrice prices (Dollar cashOnHand) drug =
    case AllDict.get drug prices of
        Just (Dollar foundDrugPrice) ->
            DrugQuantity <| cashOnHand // foundDrugPrice

        Nothing ->
            DrugQuantity 0


view : Model -> Html Msg
view model =
    div []
        [ displayDaysRemaining model.daysRemaining
        , displayLocation model.currentLocation
        , displayCashOnHand model.cashOnHand
        , displayTrenchCoat model.trenchCoat
        , displayCurrentPrices model.currentPrices
        , displayTravelOptions
        ]


displayDaysRemaining : Int -> Html Msg
displayDaysRemaining count =
    text <| "Days remaining: " ++ (toString count)


displayTravelOptions : Html Msg
displayTravelOptions =
    ul []
        [ li [] [ button [ onClick (TravelTo Brooklyn) ] [ text "Travel to Brooklyn" ] ]
        , li [] [ button [ onClick (TravelTo CentralPark) ] [ text "Travel to Central Park" ] ]
        , li [] [ button [ onClick (TravelTo ConeyIsland) ] [ text "Travel to Coney Island" ] ]
        , li [] [ button [ onClick (TravelTo Ghetto) ] [ text "Travel to the Ghetto" ] ]
        , li [] [ button [ onClick (TravelTo Bronx) ] [ text "Travel to the Bronx" ] ]
        , li [] [ button [ onClick (TravelTo Manhattan) ] [ text "Travel to Manhattan" ] ]
        ]


displayCashOnHand : Dollar -> Html a
displayCashOnHand dollar =
    div [] [ text <| "Cash on hand: " ++ displayDollars dollar ]


displayTrenchCoat : Inventory -> Html Msg
displayTrenchCoat inventory =
    dl []
        (displayGun inventory.guns
            ++ displayAvailableSlots inventory
            ++ displayDrugs inventory.drugs
        )


displayAvailableSlots : Inventory -> List (Html a)
displayAvailableSlots inventory =
    [ dt [] [ text "Slots available" ]
    , dd [] [ text <| displayDrugQuantity (Inventory.availableInventorySpace inventory) inventory.maxHolding ]
    ]


displayDrugQuantity : DrugQuantity -> DrugQuantity -> String
displayDrugQuantity (DrugQuantity available) (DrugQuantity maxHolding) =
    (toString available) ++ "/" ++ (toString maxHolding)


displayGun : GunCount -> List (Html a)
displayGun (GunCount guns) =
    [ dt [] [ text "Guns" ]
    , dd [] [ text <| toString guns ]
    ]


displayDrugs : DrugCollection -> List (Html Msg)
displayDrugs stash =
    List.concatMap (displayDrug << Inventory.lookupHolding stash) Drug.all


displayDrug : DrugHolding -> List (Html Msg)
displayDrug ( drug, DrugQuantity count ) =
    [ dt [] [ text <| toString drug ]
    , dd []
        [ text <| toString count
        , button [ onClick <| SellAll drug ] [ text "Sell all" ]
        ]
    ]


displayLocation : Location -> Html a
displayLocation location =
    div []
        [ text <| "Current location: " ++ toString location ]


displayCurrentPrices : Prices -> Html Msg
displayCurrentPrices prices =
    div []
        [ dl [] (List.concatMap displayPrice <| AllDict.toList prices)
        ]


displayPrice : ( Drug, Dollar ) -> List (Html Msg)
displayPrice ( drug, dollar ) =
    [ dt [] [ text <| toString drug ]
    , dd []
        [ text <| displayDollars dollar
        , button [ onClick <| BuyMax drug ] [ text "Buy max" ]
        ]
    ]


displayDollars : Dollar -> String
displayDollars (Dollar amount) =
    "$" ++ toString amount
