module Currency exposing (..)


type Currency
    = Currency Int


zero : Currency
zero =
    Currency 0


add : Currency -> Currency -> Currency
add =
    map2 (+)


subtract : Currency -> Currency -> Currency
subtract =
    map2 (-)


divideBy : Int -> Currency -> Currency
divideBy divisor =
    map (flip (//) divisor)


fromInt : Int -> Currency
fromInt =
    Currency


toInt : Currency -> Int
toInt (Currency int) =
    int


greaterThan : Currency -> Currency -> Bool
greaterThan (Currency a) (Currency b) =
    a > b


map : (Int -> Int) -> Currency -> Currency
map f (Currency a) =
    Currency <| f a


map2 : (Int -> Int -> Int) -> Currency -> Currency -> Currency
map2 f (Currency a) (Currency b) =
    Currency <| f a b
