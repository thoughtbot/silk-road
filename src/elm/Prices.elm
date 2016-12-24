module Prices exposing (Prices, initialPrices, hike, drop)

import AllDict exposing (AllDict)
import Item exposing (Item(..))
import Currency exposing (Currency(..))


type alias Prices =
    AllDict Item Currency Int


initialPrices : Prices
initialPrices =
    [ ( Cocaine, Currency 15000 )
    , ( Heroin, Currency 5000 )
    , ( Acid, Currency 1000 )
    , ( Weed, Currency 300 )
    , ( Speed, Currency 70 )
    , ( Ludes, Currency 10 )
    ]
        |> AllDict.fromList Item.position


hike : Item -> Int -> Prices -> Prices
hike item multiplier =
    AllDict.update item (Maybe.map (Currency.map ((*) multiplier)))


drop : Item -> Int -> Prices -> Prices
drop item divisor =
    AllDict.update item (Maybe.map (Currency.map (((flip (//)) divisor))))
