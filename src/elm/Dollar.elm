module Dollar exposing (..)


type Dollar
    = Dollar Int


zero : Dollar
zero =
    Dollar 0


add : Dollar -> Dollar -> Dollar
add =
    map2 (+)


subtract : Dollar -> Dollar -> Dollar
subtract =
    map2 (-)


divideBy : Int -> Dollar -> Dollar
divideBy divisor =
    map (flip (//) divisor)


toInt : Dollar -> Int
toInt (Dollar int) =
    int


map : (Int -> Int) -> Dollar -> Dollar
map f (Dollar a) =
    Dollar <| f a


map2 : (Int -> Int -> Int) -> Dollar -> Dollar -> Dollar
map2 f (Dollar a) (Dollar b) =
    Dollar <| f a b
