module Dollar exposing (Dollar(..), add, subtract)


type Dollar
    = Dollar Int


add : Dollar -> Dollar -> Dollar
add =
    map2 (+)


subtract : Dollar -> Dollar -> Dollar
subtract =
    map2 (-)


map2 : (Int -> Int -> Int) -> Dollar -> Dollar -> Dollar
map2 f (Dollar a) (Dollar b) =
    Dollar <| f a b
