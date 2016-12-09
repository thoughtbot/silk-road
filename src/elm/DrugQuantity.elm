module DrugQuantity exposing (DrugQuantity(..), add, subtract, minimum, map2)


type DrugQuantity
    = DrugQuantity Int


minimum : List DrugQuantity -> Maybe DrugQuantity
minimum drugQuantities =
    Maybe.map
        (\firstValue -> List.foldl (map2 min) firstValue drugQuantities)
        (List.head drugQuantities)


add : DrugQuantity -> DrugQuantity -> DrugQuantity
add =
    map2 (+)


subtract : DrugQuantity -> DrugQuantity -> DrugQuantity
subtract =
    map2 (-)


map2 : (Int -> Int -> Int) -> DrugQuantity -> DrugQuantity -> DrugQuantity
map2 f (DrugQuantity a) (DrugQuantity b) =
    DrugQuantity <| f a b
