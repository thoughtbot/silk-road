module DrugQuantity exposing (DrugQuantity(..), add, subtract)


type DrugQuantity
    = DrugQuantity Int


add : DrugQuantity -> DrugQuantity -> DrugQuantity
add =
    map2 (+)


subtract : DrugQuantity -> DrugQuantity -> DrugQuantity
subtract =
    map2 (-)


map2 : (Int -> Int -> Int) -> DrugQuantity -> DrugQuantity -> DrugQuantity
map2 f (DrugQuantity a) (DrugQuantity b) =
    DrugQuantity <| f a b
