module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)
import Html.Events exposing (onClick)
import Dollar exposing (Dollar(..))
import Drug exposing (Drug(..))
import DrugQuantity exposing (DrugQuantity(..))
import Inventory exposing (GunCount(..), DrugCollection, Inventory, DrugHolding)
import Prices exposing (Prices)
import Random
import Generator


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, generateNewPrices )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


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
    , gameState : GameState
    }


type GameState
    = Running
    | Finished


emptyAllDict : AllDict Drug b Int
emptyAllDict =
    AllDict.empty Drug.drugPosition


model : Model
model =
    Model Manhattan
        Prices.initialPrices
        (Dollar 2000)
        Inventory.empty
        emptyAllDict
        (Dollar 5500)
        Dollar.zero
        3
        Running


type Msg
    = NoOp
    | BuyMax Drug
    | SellAll Drug
    | TravelTo Location
    | NewPrices Prices
    | PayLoanShark


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
            if model.daysRemaining == 1 then
                ( { model | gameState = Finished }, Cmd.none )
            else
                ( arriveAtNewLocation location model, generateNewPrices )

        NewPrices prices ->
            ( { model | currentPrices = prices }, Cmd.none )

        PayLoanShark ->
            ( payLoanShark model, Cmd.none )


payLoanShark : Model -> Model
payLoanShark model =
    let
        amountToPay =
            Dollar.map2 min model.cashOnHand model.debt
    in
        { model
            | debt = Dollar.subtract model.debt amountToPay
            , cashOnHand = Dollar.subtract model.cashOnHand amountToPay
        }


generateNewPrices : Cmd Msg
generateNewPrices =
    Random.generate NewPrices Generator.prices


arriveAtNewLocation : Location -> Model -> Model
arriveAtNewLocation location model =
    { model
        | currentLocation = location
        , daysRemaining = model.daysRemaining - 1
        , debt = calculateInterest model.debt
    }


calculateInterest : Dollar -> Dollar
calculateInterest =
    Dollar.map (toFloat >> ((*) 1.1) >> truncate)


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


calculateScore : Model -> Int
calculateScore model =
    Dollar.subtract model.cashOnHand model.debt
        |> Dollar.toInt


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
    case model.gameState of
        Running ->
            div []
                [ displayDaysRemaining model.daysRemaining
                , displayLocation model.currentLocation
                , displayDebt model.debt
                , displayCashOnHand model.cashOnHand
                , displayTrenchCoat model.trenchCoat
                , displayCurrentPrices model.currentPrices
                , displayTravelOptions
                , displayLoanSharkOptions model.currentLocation
                ]

        Finished ->
            displayScore model


displayDebt : Dollar -> Html a
displayDebt debt =
    text <| "Debt: " ++ displayDollars debt


displayLoanSharkOptions : Location -> Html Msg
displayLoanSharkOptions location =
    if location == Bronx then
        div []
            [ button [ onClick PayLoanShark ] [ text "Pay Loan Shark" ]
            ]
    else
        div [] []


displayScore : Model -> Html a
displayScore model =
    div []
        [ text <| "Your score: " ++ (toString <| calculateScore model)
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
