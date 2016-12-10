module Generator exposing (prices, newPricesAndEvents)

import Random exposing (Generator)
import Random.Extra as RandomE
import Drug exposing (Drug(..))
import Dollar exposing (Dollar(..))
import Prices exposing (Prices)
import Event exposing (Event(..))
import AllDict
import DrugQuantity exposing (DrugQuantity(..))


newPricesAndEvents : Generator ( Prices, Event )
newPricesAndEvents =
    Random.map2 (,) prices event


event : Generator Event
event =
    RandomE.frequency
        [ ( 50, noEvent )
        , ( 15, priceHike )
        , ( 15, priceDrop )
        , ( 10, findDrugs )
        , ( 4, dropDrugs )
        , ( 6, mugging )
        ]


noEvent : Generator Event
noEvent =
    RandomE.constant None


priceHike : Generator Event
priceHike =
    Random.map2 PriceHike drug hikeMultiplier


priceDrop : Generator Event
priceDrop =
    Random.map2 PriceDrop drug dropDivisor


mugging : Generator Event
mugging =
    RandomE.constant Mugging


findDrugs : Generator Event
findDrugs =
    Random.map2 FindDrug drug quantityDrugsFound


dropDrugs : Generator Event
dropDrugs =
    Random.map2 DropDrug drug dropDivisor


prices : Generator Prices
prices =
    Random.map (AllDict.fromList Drug.drugPosition) priceList


priceList : Generator (List ( Drug, Dollar ))
priceList =
    List.map price Drug.all
        |> RandomE.combine


price : Drug -> Generator ( Drug, Dollar )
price drug =
    Random.map ((,) drug) (dollar drug)


hikeMultiplier : Generator Int
hikeMultiplier =
    Random.int 7 10


dropDivisor : Generator Int
dropDivisor =
    Random.int 2 5


drug : Generator Drug
drug =
    RandomE.sample Drug.all
        |> Random.map (Maybe.withDefault Ludes)


quantityDrugsFound : Generator DrugQuantity
quantityDrugsFound =
    Random.int 2 9
        |> Random.map DrugQuantity


dollar : Drug -> Generator Dollar
dollar drug =
    let
        (Dollar max) =
            Drug.maxPrice drug

        (Dollar min) =
            Drug.minPrice drug
    in
        Random.int min max |> Random.map Dollar
