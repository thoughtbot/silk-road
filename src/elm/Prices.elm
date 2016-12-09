module Prices exposing (Prices, initialPrices)

import AllDict exposing (AllDict)
import Drug exposing (Drug(..))
import Dollar exposing (Dollar(..))


type alias Prices =
    AllDict Drug Dollar Int


initialPrices : Prices
initialPrices =
    [ ( Cocaine, Dollar 15000 )
    , ( Heroin, Dollar 5000 )
    , ( Acid, Dollar 1000 )
    , ( Weed, Dollar 300 )
    , ( Speed, Dollar 70 )
    , ( Ludes, Dollar 10 )
    ]
        |> AllDict.fromList Drug.drugPosition
