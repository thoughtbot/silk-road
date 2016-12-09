module Generator exposing (prices)

import Random exposing (Generator)
import Random.Extra as RandomE
import Drug exposing (Drug)
import Dollar exposing (Dollar(..))
import Prices exposing (Prices)
import AllDict


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


dollar : Drug -> Generator Dollar
dollar drug =
    let
        (Dollar max) =
            Drug.maxPrice drug

        (Dollar min) =
            Drug.minPrice drug
    in
        Random.int min max |> Random.map Dollar
