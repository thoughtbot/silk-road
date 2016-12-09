module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Dollar exposing (Dollar(..))
import Drug exposing (Drug(..))
import DrugQuantity exposing (DrugQuantity(..))
import Inventory exposing (GunCount(..), DrugCollection, Inventory, DrugHolding)
import Prices exposing (Prices)
import Random
import Generator
import Event exposing (Event(..))


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, generateNewPricesAndEvents )
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
    , currentEvent : Event
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
        None
        (Dollar 2000)
        Inventory.empty
        emptyAllDict
        (Dollar 5500)
        Dollar.zero
        31
        Running


type Msg
    = NoOp
    | BuyMax Drug
    | SellAll Drug
    | TravelTo Location
    | TravelArrival ( Prices, Event )
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
                ( arriveAtNewLocation location model, generateNewPricesAndEvents )

        TravelArrival ( prices, events ) ->
            ( applyPricesAndEvents prices events model, Cmd.none )

        PayLoanShark ->
            ( payLoanShark model, Cmd.none )


applyPricesAndEvents : Prices -> Event -> Model -> Model
applyPricesAndEvents prices event model =
    let
        newModel =
            { model | currentPrices = prices, currentEvent = event }
    in
        case event of
            None ->
                newModel

            PriceHike drug multiplier ->
                { newModel
                    | currentPrices = Prices.hike drug multiplier newModel.currentPrices
                }

            PriceDrop drug divisor ->
                { newModel
                    | currentPrices = Prices.drop drug divisor newModel.currentPrices
                }


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


generateNewPricesAndEvents : Cmd Msg
generateNewPricesAndEvents =
    Random.generate TravelArrival Generator.newPricesAndEvents


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
                [ displayEventMessage model.currentEvent
                , main_ []
                    [ section [ class "status" ]
                        [ h2 [] [ text "Status, buddy" ]
                        , displayDaysRemaining model.daysRemaining
                        , displayLocation model.currentLocation
                        , displayCashOnHand model.cashOnHand
                        , displayDebt model.debt
                        , displayTrenchCoat model.trenchCoat
                        , displayLoanSharkOptions model.currentLocation
                        ]
                    , section [ class "prices" ]
                        [ h2 [] [ text "Drug Prices" ]
                        , displayCurrentPrices model.currentPrices
                        ]
                    , section [ class "travel" ]
                        [ h2 [] [ text "Take a trip" ]
                        , displayTravelOptions
                        ]
                    ]
                ]

        Finished ->
            displayScore model


flash : String -> Html a
flash s =
    div [ class "flash" ] [ text s ]


displayEventMessage : Event -> Html a
displayEventMessage event =
    case event of
        None ->
            div [] []

        PriceHike drug _ ->
            flash <| priceHikeMessage drug

        PriceDrop drug _ ->
            flash <| priceDropMessage drug


priceHikeMessage : Drug -> String
priceHikeMessage drug =
    case drug of
        Cocaine ->
            "Cops just busted the local provider. Cocaine prices have spiked"

        Heroin ->
            "Cops just busted the local provider. Heroin prices have spiked"

        Acid ->
            "Production problems have caused a shortage. Acid is super expensive"

        Weed ->
            "Bad harvest this year. Weed is super expensive"

        Speed ->
            "Local provider has retired. Speed is pricey"

        Ludes ->
            "Lotta people want Ludes these days. You're gonna have to pay..."


priceDropMessage : Drug -> String
priceDropMessage drug =
    case drug of
        Cocaine ->
            "A new shipment has just come in from Columbia. Cocaine prices have plummeted"

        Heroin ->
            "Cops just busted the local provider. Heroin prices have spiked"

        Acid ->
            "New production equipment has made Acid more plentiful."

        Weed ->
            "Bumper crop this year. The bottom has fallen out of weed prices"

        Speed ->
            "Someone just dumped speed on the market. Prices are low."

        Ludes ->
            "Someone just hit up the local phramacy. CHEAP LUDES!!!"


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
        [ button [ onClick <| SellAll drug ] [ text "Sell all" ]
        , text <| toString count
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
        [ button [ onClick <| BuyMax drug ] [ text "Buy max" ]
        , text <| displayDollars dollar
        ]
    ]


displayDollars : Dollar -> String
displayDollars (Dollar amount) =
    "$" ++ toString amount
