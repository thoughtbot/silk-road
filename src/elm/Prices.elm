module Prices exposing (Prices, initialPrices, hike, drop)

import AllDict exposing (AllDict)
import Item exposing (Item(..))
import Dollar exposing (Dollar(..))


type alias Prices =
    AllDict Item Dollar Int


initialPrices : Prices
initialPrices =
    [ ( Cocaine, Dollar 15000 )
    , ( Heroin, Dollar 5000 )
    , ( Acid, Dollar 1000 )
    , ( Weed, Dollar 300 )
    , ( Speed, Dollar 70 )
    , ( Ludes, Dollar 10 )
    ]
        |> AllDict.fromList Item.position


hike : Item -> Int -> Prices -> Prices
hike item multiplier =
    AllDict.update item (Maybe.map (Dollar.map ((*) multiplier)))


drop : Item -> Int -> Prices -> Prices
drop item divisor =
    AllDict.update item (Maybe.map (Dollar.map (((flip (//)) divisor))))
