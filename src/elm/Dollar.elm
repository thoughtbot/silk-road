module Dollar exposing (Dollar(..), add, subtract, zero)


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


map2 : (Int -> Int -> Int) -> Dollar -> Dollar -> Dollar
map2 f (Dollar a) (Dollar b) =
    Dollar <| f a b
