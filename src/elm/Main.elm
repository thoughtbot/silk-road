port module Main exposing (..)

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
import I18n exposing (Translation(..), translate)


port setPageTitle : String -> Cmd a


gameStyle : I18n.Language
gameStyle =
    I18n.SilkRoad


initialModelAndEffects : ( Model, Cmd Msg )
initialModelAndEffects =
    ( model
    , Cmd.batch
        [ generateNewPricesAndEvents
        , setPageTitle <| translate gameStyle GameTitle
        ]
    )


main : Program Never Model Msg
main =
    Html.program
        { init = initialModelAndEffects
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
    | DisplayPrices
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


maxDebt : Currency
maxDebt =
    Currency.fromInt 13000


canBorrowMore : Model -> Bool
canBorrowMore model =
    Currency.greaterThan maxDebt model.debt


type Msg
    = NoOp
    | BuyMax Item
    | SellAll Item
    | TravelTo Location
    | TravelArrival ( Prices, Event )
    | PayLender
    | DepositCash
    | WithdrawCash
    | BorrowMax
    | SeePrices
    | ReturnToGame


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

        BorrowMax ->
            ( borrowMax model, Cmd.none )

        DepositCash ->
            ( { model | cashInBank = Currency.add model.cashOnHand model.cashInBank, cashOnHand = Currency.zero }, Cmd.none )

        WithdrawCash ->
            ( { model | cashInBank = Currency.zero, cashOnHand = Currency.add model.cashOnHand model.cashInBank }, Cmd.none )

        SeePrices ->
            ( { model | gameState = DisplayPrices }, Cmd.none )

        ReturnToGame ->
            ( { model | gameState = Running }, Cmd.none )


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


borrowableAmount : Model -> Currency
borrowableAmount model =
    Currency.subtract maxDebt model.debt


borrowMax : Model -> Model
borrowMax model =
    if canBorrowMore model then
        { model
            | debt = maxDebt
            , cashOnHand = Currency.add model.cashOnHand (borrowableAmount model)
        }
    else
        model


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
    case gameStyle of
        I18n.DrugWars ->
            style
                [ ( "fontFamily", "Courier,sans-serif" )
                , ( "background", "#000" )
                , ( "color", "#71f442" )
                , ( "fontSize", "16px" )
                ]

        I18n.SilkRoad ->
            style
                [ ( "fontFamily", "'Press Start 2P',sans-serif" )
                , ( "background", "#d5d5b8" )
                , ( "color", "#40403A" )
                ]


view : Model -> Html Msg
view model =
    div [ gameStyles ]
        [ h1 [] [ text <| translate gameStyle GameTitle ]
        , displayGame model
        ]


displayGame : Model -> Html Msg
displayGame model =
    case model.gameState of
        Running ->
            displayRunningGame model

        DisplayPrices ->
            displayPrices

        Finished ->
            displayScore model


displayRunningGame : Model -> Html Msg
displayRunningGame model =
    div []
        [ displayEventMessage model.currentEvent
        , main_ []
            [ section [ class "status" ]
                [ h2 [] [ text <| translate gameStyle StatusHeader ]
                , displayGameMetadata model
                , displayLenderOptions model.currentLocation
                , button
                    [ onClick SeePrices ]
                    [ text <| translate gameStyle SeePriceRangesButton ]
                ]
            , section [ class "prices" ]
                [ h2 [] [ text <| translate gameStyle SellItemsHeader ]
                , displayInventoryOnHand model.inventoryOnHand
                ]
            , section [ class "prices" ]
                [ h2 [] [ text <| translate gameStyle BuyItemsHeader ]
                , displayCurrentPrices model.currentPrices
                ]
            , section [ class "travel" ]
                [ h2 [] [ text <| translate gameStyle TravelHeader ]
                , displayTravelOptions
                ]
            ]
        ]


displayGameMetadata : Model -> Html a
displayGameMetadata model =
    dl []
        [ dt [] [ text <| translate gameStyle CurrentLocationHeader ]
        , dd [] [ text <| locationName model.currentLocation ]
        , dt [] [ text <| translate gameStyle DaysRemainingHeader ]
        , dd [] [ text <| toString model.daysRemaining ]
        , dt [] [ text <| translate gameStyle CurrencyOnHandHeader ]
        , dd [] [ text <| displayCurrency model.cashOnHand ]
        , dt [] [ text <| translate gameStyle DebtHeader ]
        , dd [] [ text <| displayCurrency model.debt ]
        , dt [] [ text <| translate gameStyle BankHeader ]
        , dd [] [ text <| displayCurrency model.cashInBank ]
        , dt [] [ text <| translate gameStyle AvailableInventoryHeader ]
        , dd [] [ text <| displayItemQuantity (Inventory.availableInventorySpace model.inventoryOnHand) model.inventoryOnHand.maxHolding ]
        ]


displayPrices : Html Msg
displayPrices =
    div []
        [ h2 [] [ text <| translate gameStyle PriceRangeHeader ]
        , dl [] (List.concatMap displayPriceRange Item.all)
        , button
            [ onClick ReturnToGame ]
            [ text <| translate gameStyle ReturnToGameButton ]
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
            flash <| translate gameStyle (PriceHikeMessage { item = item })

        PriceDrop item _ ->
            flash <| translate gameStyle (PriceDropMessage { item = item })

        Mugging ->
            flash <| translate gameStyle MuggedMessage

        FindItem item quantity ->
            flash <| translate gameStyle (FoundItemMessage { item = item, quantity = quantity })

        DropItem item _ ->
            flash <| translate gameStyle (DroppedItemMessage { item = item })


displayLenderOptions : Location -> Html Msg
displayLenderOptions location =
    if Location.home location then
        div []
            [ button [ onClick PayLender ] [ text <| translate gameStyle PayLenderButton ]
            , button [ onClick BorrowMax ] [ text <| translate gameStyle BorrowMaxButton ]
            , button [ onClick DepositCash ] [ text <| translate gameStyle DepositCashButton ]
            , button [ onClick WithdrawCash ] [ text <| translate gameStyle WithdrawCashButton ]
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


displayPriceRange : Item -> List (Html a)
displayPriceRange item =
    [ dt [] [ text <| itemName item ]
    , dd [] [ text <| priceRange item ]
    ]


priceRange : Item -> String
priceRange item =
    "("
        ++ displayCurrency (Item.minPrice item)
        ++ "-"
        ++ displayCurrency (Item.maxPrice item)
        ++ ")"


displayCurrency : Currency -> String
displayCurrency currency =
    translate gameStyle (CurrencyText { currency = currency })


itemName : Item -> String
itemName item =
    translate gameStyle (ItemName { item = item })


locationName : Location -> String
locationName location =
    translate gameStyle (LocationName { location = location })
