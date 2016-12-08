module Dollar exposing (Dollar(..), add, subtract)


type Dollar
    = Dollar Int


add (Dollar a) (Dollar b) =
    Dollar <| a + b


subtract (Dollar a) (Dollar b) =
    Dollar <| a - b
