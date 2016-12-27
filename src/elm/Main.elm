module Main exposing (..)

import AllDict exposing (AllDict)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Currency exposing (Currency(..))
import Item exposing (Item(..))
import Location exposing (Location(..))
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


type alias Model =
    { currentLocation : Location
    , currentPrices : Prices
    , currentEvent : Event
    , cashOnHand : Currency
    , cashInBank : Currency
    , inventoryOnHand : Inventory
    , stash : ItemCollection
    , debt : Currency
    , bankAccountBalance : Currency
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
    Model Location1
        Prices.initialPrices
        None
        (Currency 2000)
        Currency.zero
        Inventory.empty
        emptyAllDict
        (Currency 5500)
        Currency.zero
        31
        Running


type Msg
    = NoOp
    | BuyMax Item
    | SellAll Item
    | TravelTo Location
    | TravelArrival ( Prices, Event )
    | PayLender
    | DepositCash
    | WithdrawCash


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

        PayLender ->
            ( payLender model, Cmd.none )

        DepositCash ->
            ( { model | cashInBank = Currency.add model.cashOnHand model.cashInBank, cashOnHand = Currency.zero }, Cmd.none )

        WithdrawCash ->
            ( { model | cashInBank = Currency.zero, cashOnHand = Currency.add model.cashOnHand model.cashInBank }, Cmd.none )


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
                    | inventoryOnHand = Inventory.addItems item quantity model.inventoryOnHand
                }

            Mugging ->
                { newModel | cashOnHand = Currency.divideBy 2 newModel.cashOnHand }

            DropItem item divisor ->
                { newModel
                    | inventoryOnHand = Inventory.drop item divisor model.inventoryOnHand
                }


payLender : Model -> Model
payLender model =
    let
        amountToPay =
            Currency.map2 min model.cashOnHand model.debt
    in
        { model
            | debt = Currency.subtract model.debt amountToPay
            , cashOnHand = Currency.subtract model.cashOnHand amountToPay
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


calculateInterest : Currency -> Currency
calculateInterest =
    Currency.map (toFloat >> ((*) 1.1) >> truncate)


sellAll : Model -> Item -> Model
sellAll model item =
    let
        multiplyThings (ItemQuantity quantity) (Currency amount) =
            Currency <| quantity * amount

        totalSalePrice =
            multiplyThings (Inventory.quantityOfItem model.inventoryOnHand.items item)
                (Maybe.withDefault Currency.zero <| AllDict.get item model.currentPrices)

        newInventoryOnHand =
            Inventory.removeAllItem item model.inventoryOnHand
    in
        { model | cashOnHand = Currency.add model.cashOnHand totalSalePrice, inventoryOnHand = newInventoryOnHand }


buyMax : Model -> Item -> Model
buyMax model item =
    let
        purchaseableItemQuantity_ =
            purchaseableItemQuantity model item

        totalPurchasePrice =
            multiplyThings purchaseableItemQuantity_
                (Maybe.withDefault Currency.zero <| AllDict.get item model.currentPrices)

        multiplyThings (ItemQuantity quantity) (Currency amount) =
            Currency <| quantity * amount

        newInventoryOnHand =
            Inventory.addItems item purchaseableItemQuantity_ model.inventoryOnHand
    in
        { model | cashOnHand = Currency.subtract model.cashOnHand totalPurchasePrice, inventoryOnHand = newInventoryOnHand }


calculateScore : Model -> Int
calculateScore model =
    let
        positiveCurrency =
            Currency.add model.cashInBank model.cashOnHand

        negativeCurrency =
            model.debt
    in
        Currency.subtract positiveCurrency negativeCurrency
            |> Currency.toInt


purchaseableItemQuantity : Model -> Item -> ItemQuantity
purchaseableItemQuantity model item =
    Maybe.withDefault (ItemQuantity 0) <|
        ItemQuantity.minimum
            [ maxQuantityByPrice model.currentPrices model.cashOnHand item
            , Inventory.availableInventorySpace model.inventoryOnHand
            ]


maxQuantityByPrice : Prices -> Currency -> Item -> ItemQuantity
maxQuantityByPrice prices (Currency cashOnHand) item =
    case AllDict.get item prices of
        Just (Currency foundItemPrice) ->
            ItemQuantity <| cashOnHand // foundItemPrice

        Nothing ->
            ItemQuantity 0


gameStyles : Attribute a
gameStyles =
    style
        [ ( "fontFamily", "Courier,sans-serif" )
        , ( "background", "#000" )
        , ( "color", "#71f442" )
        , ( "fontSize", "16px" )
        ]


view : Model -> Html Msg
view model =
    div [ gameStyles ]
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
                , displayLenderOptions model.currentLocation
                ]
            , section [ class "prices" ]
                [ h2 [] [ text "Sell" ]
                , displayInventoryOnHand model.inventoryOnHand
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
        , dd [] [ text <| locationName model.currentLocation ]
        , dt [] [ text "Days remaining" ]
        , dd [] [ text <| toString model.daysRemaining ]
        , dt [] [ text "Cash on hand" ]
        , dd [] [ text <| displayCurrency model.cashOnHand ]
        , dt [] [ text "Debt" ]
        , dd [] [ text <| displayCurrency model.debt ]
        , dt [] [ text "Bank" ]
        , dd [] [ text <| displayCurrency model.cashInBank ]
        , dt [] [ text "Slots available" ]
        , dd [] [ text <| displayItemQuantity (Inventory.availableInventorySpace model.inventoryOnHand) model.inventoryOnHand.maxHolding ]
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
                    ++ itemName item
                    ++ " on the ground"
                )

        DropItem item _ ->
            flash <| "Oh no, you dropped a bunch of " ++ itemName item ++ ", bud. Bummer"


priceHikeMessage : Item -> String
priceHikeMessage item =
    case item of
        Item1 ->
            "Cops just busted the local provider. Cocaine prices have spiked"

        Item2 ->
            "Cops just busted the local provider. Heroin prices have spiked"

        Item3 ->
            "Production problems have caused a shortage. Acid is super expensive"

        Item4 ->
            "Bad harvest this year. Weed is super expensive"

        Item5 ->
            "Local provider has retired. Speed is pricey"

        Item6 ->
            "Lotta people want Ludes these days. You're gonna have to pay..."


priceDropMessage : Item -> String
priceDropMessage item =
    case item of
        Item1 ->
            "A new shipment has just come in from Columbia. Cocaine prices have plummeted"

        Item2 ->
            "No one's doing heroin these days. Prices have plummetted"

        Item3 ->
            "New production equipment has made Acid more plentiful."

        Item4 ->
            "Bumper crop this year. The bottom has fallen out of weed prices"

        Item5 ->
            "Someone just dumped speed on the market. Prices are low."

        Item6 ->
            "Someone just hit up the local phramacy. CHEAP LUDES!!!"


displayLenderOptions : Location -> Html Msg
displayLenderOptions location =
    if Location.home location then
        div []
            [ button [ onClick PayLender ] [ text "Pay Loan Shark" ]
            , button [ onClick DepositCash ] [ text "Deposit cash" ]
            , button [ onClick WithdrawCash ] [ text "Withdraw cash" ]
            ]
    else
        div [] []


displayScore : Model -> Html a
displayScore model =
    div []
        [ text <| "Your score: " ++ (toString <| calculateScore model)
        ]


locationName : Location -> String
locationName location =
    case location of
        Location1 ->
            "the Bronx"

        Location2 ->
            "Central Park"

        Location3 ->
            "Coney Island"

        Location4 ->
            "the Ghetto"

        Location5 ->
            "Brooklyn"

        Location6 ->
            "Manhattan"


displayTravelOptions : Html Msg
displayTravelOptions =
    ul []
        (List.map travelButton Location.all)


travelButton : Location -> Html Msg
travelButton location =
    li
        []
        [ button
            [ onClick (TravelTo location) ]
            [ text <| "Travel to " ++ locationName location ]
        ]


displayInventoryOnHand : Inventory -> Html Msg
displayInventoryOnHand inventory =
    dl [] (displayItems inventory.items)


displayItemQuantity : ItemQuantity -> ItemQuantity -> String
displayItemQuantity (ItemQuantity available) (ItemQuantity maxHolding) =
    (toString available) ++ "/" ++ (toString maxHolding)


displayItems : ItemCollection -> List (Html Msg)
displayItems stash =
    List.concatMap (displayItem << Inventory.lookupHolding stash) Item.all


displayItem : ItemHolding -> List (Html Msg)
displayItem ( item, ItemQuantity count ) =
    [ dt [] [ text <| itemName item ]
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


displayPrice : ( Item, Currency ) -> List (Html Msg)
displayPrice ( item, currency ) =
    [ dt [] [ text <| itemName item ]
    , dd []
        [ button [ onClick <| BuyMax item ] [ text "Buy max" ]
        , text <| displayCurrency currency
        ]
    ]


displayCurrency : Currency -> String
displayCurrency (Currency amount) =
    "$" ++ toString amount


itemName : Item -> String
itemName item =
    case item of
        Item1 ->
            "Cocaine"

        Item2 ->
            "Heroin"

        Item3 ->
            "Acid"

        Item4 ->
            "Weed"

        Item5 ->
            "Speed"

        Item6 ->
            "Ludes"
