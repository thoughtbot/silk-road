module Prices exposing (Prices, initialPrices, hike, drop)

import AllDict exposing (AllDict)
import Item exposing (Item(..))
import Currency exposing (Currency(..))


type alias Prices =
    AllDict Item Currency Int


initialPrices : Prices
initialPrices =
    [ ( Item1, Currency 15000 )
    , ( Item2, Currency 5000 )
    , ( Item3, Currency 1000 )
    , ( Item4, Currency 300 )
    , ( Item5, Currency 70 )
    , ( Item6, Currency 10 )
    ]
        |> AllDict.fromList Item.position


hike : Item -> Int -> Prices -> Prices
hike item multiplier =
    AllDict.update item (Maybe.map (Currency.map ((*) multiplier)))


drop : Item -> Int -> Prices -> Prices
drop item divisor =
    AllDict.update item (Maybe.map (Currency.map (((flip (//)) divisor))))
