module ItemQuantity exposing (ItemQuantity(..), add, subtract, minimum, map, map2)


type ItemQuantity
    = ItemQuantity Int


minimum : List ItemQuantity -> Maybe ItemQuantity
minimum itemQuantities =
    Maybe.map
        (\firstValue -> List.foldl (map2 min) firstValue itemQuantities)
        (List.head itemQuantities)


add : ItemQuantity -> ItemQuantity -> ItemQuantity
add =
    map2 (+)


subtract : ItemQuantity -> ItemQuantity -> ItemQuantity
subtract =
    map2 (-)


map : (Int -> Int) -> ItemQuantity -> ItemQuantity
map f (ItemQuantity a) =
    ItemQuantity <| f a


map2 : (Int -> Int -> Int) -> ItemQuantity -> ItemQuantity -> ItemQuantity
map2 f (ItemQuantity a) (ItemQuantity b) =
    ItemQuantity <| f a b
