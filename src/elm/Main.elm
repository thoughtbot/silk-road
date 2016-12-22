module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Dollar exposing (Dollar(..))
import Item exposing (Item(..))
import ItemQuantity exposing (ItemQuantity(..))
import Inventory exposing (ItemCollection, Inventory, ItemHolding)
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
    , stash : ItemCollection
    , debt : Dollar
    , bankAccountBalance : Dollar
    , daysRemaining : Int
    , gameState : GameState
    }


type GameState
    = Running
    | Finished


emptyAllDict : AllDict Item b Int
emptyAllDict =
    AllDict.empty Item.position


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
    | BuyMax Item
    | SellAll Item
    | TravelTo Location
    | TravelArrival ( Prices, Event )
    | PayLoanShark


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BuyMax item ->
            ( buyMax model item, Cmd.none )

        SellAll item ->
            ( sellAll model item, Cmd.none )

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

            PriceHike item multiplier ->
                { newModel
                    | currentPrices = Prices.hike item multiplier newModel.currentPrices
                }

            PriceDrop item divisor ->
                { newModel
                    | currentPrices = Prices.drop item divisor newModel.currentPrices
                }

            FindItem item quantity ->
                { newModel
                    | trenchCoat = Inventory.addItems item quantity model.trenchCoat
                }

            Mugging ->
                { newModel | cashOnHand = Dollar.divideBy 2 newModel.cashOnHand }

            DropItem item divisor ->
                { newModel
                    | trenchCoat = Inventory.drop item divisor model.trenchCoat
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


sellAll : Model -> Item -> Model
sellAll model item =
    let
        multiplyThings (ItemQuantity quantity) (Dollar amount) =
            Dollar <| quantity * amount

        totalSalePrice =
            multiplyThings (Inventory.quantityOfItem model.trenchCoat.items item)
                (Maybe.withDefault Dollar.zero <| AllDict.get item model.currentPrices)

        newTrenchcoat =
            Inventory.removeAllItem item model.trenchCoat
    in
        { model | cashOnHand = Dollar.add model.cashOnHand totalSalePrice, trenchCoat = newTrenchcoat }


buyMax : Model -> Item -> Model
buyMax model item =
    let
        purchaseableItemQuantity_ =
            purchaseableItemQuantity model item

        totalPurchasePrice =
            multiplyThings purchaseableItemQuantity_
                (Maybe.withDefault Dollar.zero <| AllDict.get item model.currentPrices)

        multiplyThings (ItemQuantity quantity) (Dollar amount) =
            Dollar <| quantity * amount

        newTrenchcoat =
            Inventory.addItems item purchaseableItemQuantity_ model.trenchCoat
    in
        { model | cashOnHand = Dollar.subtract model.cashOnHand totalPurchasePrice, trenchCoat = newTrenchcoat }


calculateScore : Model -> Int
calculateScore model =
    Dollar.subtract model.cashOnHand model.debt
        |> Dollar.toInt


purchaseableItemQuantity : Model -> Item -> ItemQuantity
purchaseableItemQuantity model item =
    Maybe.withDefault (ItemQuantity 0) <|
        ItemQuantity.minimum
            [ maxQuantityByPrice model.currentPrices model.cashOnHand item
            , Inventory.availableInventorySpace model.trenchCoat
            ]


maxQuantityByPrice : Prices -> Dollar -> Item -> ItemQuantity
maxQuantityByPrice prices (Dollar cashOnHand) item =
    case AllDict.get item prices of
        Just (Dollar foundItemPrice) ->
            ItemQuantity <| cashOnHand // foundItemPrice

        Nothing ->
            ItemQuantity 0


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Drug Wars" ]
        , displayGame model
        ]


displayGame : Model -> Html Msg
displayGame model =
    case model.gameState of
        Running ->
            displayRunningGame model

        Finished ->
            displayScore model


displayRunningGame : Model -> Html Msg
displayRunningGame model =
    div []
        [ displayEventMessage model.currentEvent
        , main_ []
            [ section [ class "status" ]
                [ h2 [] [ text "Status, buddy" ]
                , displayGameMetadata model
                , displayLoanSharkOptions model.currentLocation
                ]
            , section [ class "prices" ]
                [ h2 [] [ text "Sell" ]
                , displayTrenchCoat model.trenchCoat
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


displayGameMetadata : Model -> Html a
displayGameMetadata model =
    dl []
        [ dt [] [ text "Current location" ]
        , dd [] [ text <| toString model.currentLocation ]
        , dt [] [ text "Days remaining" ]
        , dd [] [ text <| toString model.daysRemaining ]
        , dt [] [ text "Cash on hand" ]
        , dd [] [ text <| displayDollars model.cashOnHand ]
        , dt [] [ text "Debt" ]
        , dd [] [ text <| displayDollars model.debt ]
        , dt [] [ text "Slots available" ]
        , dd [] [ text <| displayItemQuantity (Inventory.availableInventorySpace model.trenchCoat) model.trenchCoat.maxHolding ]
        ]


flash : String -> Html a
flash s =
    div [ class "flash" ] [ text s ]


displayEventMessage : Event -> Html a
displayEventMessage event =
    case event of
        None ->
            div [] []

        PriceHike item _ ->
            flash <| priceHikeMessage item

        PriceDrop item _ ->
            flash <| priceDropMessage item

        Mugging ->
            flash <| "You got mugged! The perpetrator took off with half your cash"

        FindItem item (ItemQuantity amount) ->
            flash <|
                ("You found "
                    ++ toString amount
                    ++ " "
                    ++ toString item
                    ++ " on the ground"
                )

        DropItem item _ ->
            flash <| "Oh no, you dropped a bunch of " ++ toString item ++ ", bud. Bummer"


priceHikeMessage : Item -> String
priceHikeMessage item =
    case item of
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


priceDropMessage : Item -> String
priceDropMessage item =
    case item of
        Cocaine ->
            "A new shipment has just come in from Columbia. Cocaine prices have plummeted"

        Heroin ->
            "No one's doing heroin these days. Prices have plummetted"

        Acid ->
            "New production equipment has made Acid more plentiful."

        Weed ->
            "Bumper crop this year. The bottom has fallen out of weed prices"

        Speed ->
            "Someone just dumped speed on the market. Prices are low."

        Ludes ->
            "Someone just hit up the local phramacy. CHEAP LUDES!!!"


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


displayTrenchCoat : Inventory -> Html Msg
displayTrenchCoat inventory =
    dl [] (displayItems inventory.items)


displayItemQuantity : ItemQuantity -> ItemQuantity -> String
displayItemQuantity (ItemQuantity available) (ItemQuantity maxHolding) =
    (toString available) ++ "/" ++ (toString maxHolding)


displayItems : ItemCollection -> List (Html Msg)
displayItems stash =
    List.concatMap (displayItem << Inventory.lookupHolding stash) Item.all


displayItem : ItemHolding -> List (Html Msg)
displayItem ( item, ItemQuantity count ) =
    [ dt [] [ text <| toString item ]
    , dd []
        [ button [ onClick <| SellAll item ] [ text "Sell all" ]
        , text <| toString count
        ]
    ]


displayCurrentPrices : Prices -> Html Msg
displayCurrentPrices prices =
    div []
        [ dl [] (List.concatMap displayPrice <| AllDict.toList prices)
        ]


displayPrice : ( Item, Dollar ) -> List (Html Msg)
displayPrice ( item, dollar ) =
    [ dt [] [ text <| toString item ]
    , dd []
        [ button [ onClick <| BuyMax item ] [ text "Buy max" ]
        , text <| displayDollars dollar
        ]
    ]


displayDollars : Dollar -> String
displayDollars (Dollar amount) =
    "$" ++ toString amount
