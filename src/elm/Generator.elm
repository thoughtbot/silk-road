module Generator exposing (prices, newPricesAndEvents)

import Random exposing (Generator)
import Random.Extra as RandomE
import Item exposing (Item(..))
import Dollar exposing (Dollar(..))
import Prices exposing (Prices)
import Event exposing (Event(..))
import AllDict
import ItemQuantity exposing (ItemQuantity(..))


newPricesAndEvents : Generator ( Prices, Event )
newPricesAndEvents =
    Random.map2 (,) prices event


event : Generator Event
event =
    RandomE.frequency
        [ ( 50, noEvent )
        , ( 15, priceHike )
        , ( 15, priceDrop )
        , ( 10, findItems )
        , ( 4, dropItems )
        , ( 6, mugging )
        ]


noEvent : Generator Event
noEvent =
    RandomE.constant None


priceHike : Generator Event
priceHike =
    Random.map2 PriceHike item hikeMultiplier


priceDrop : Generator Event
priceDrop =
    Random.map2 PriceDrop item dropDivisor


mugging : Generator Event
mugging =
    RandomE.constant Mugging


findItems : Generator Event
findItems =
    Random.map2 FindItem item quantityItemsFound


dropItems : Generator Event
dropItems =
    Random.map2 DropItem item dropDivisor


prices : Generator Prices
prices =
    Random.map (AllDict.fromList Item.position) priceList


priceList : Generator (List ( Item, Dollar ))
priceList =
    List.map price Item.all
        |> RandomE.combine


price : Item -> Generator ( Item, Dollar )
price item =
    Random.map ((,) item) (dollar item)


hikeMultiplier : Generator Int
hikeMultiplier =
    Random.int 7 10


dropDivisor : Generator Int
dropDivisor =
    Random.int 2 5


item : Generator Item
item =
    RandomE.sample Item.all
        |> Random.map (Maybe.withDefault Ludes)


quantityItemsFound : Generator ItemQuantity
quantityItemsFound =
    Random.int 2 9
        |> Random.map ItemQuantity


dollar : Item -> Generator Dollar
dollar item =
    let
        (Dollar max) =
            Item.maxPrice item

        (Dollar min) =
            Item.minPrice item
    in
        Random.int min max |> Random.map Dollar
