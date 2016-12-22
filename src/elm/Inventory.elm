module Inventory exposing (..)

import AllDict exposing (AllDict)
import Item exposing (Item(..))
import ItemQuantity exposing (ItemQuantity(..))


type alias ItemCollection =
    AllDict Item ItemQuantity Int


type alias ItemHolding =
    ( Item, ItemQuantity )


type alias Inventory =
    { items : ItemCollection
    , maxHolding : ItemQuantity
    }


quantityOfItem : ItemCollection -> Item -> ItemQuantity
quantityOfItem itemCollection item =
    AllDict.get item itemCollection
        |> Maybe.withDefault (ItemQuantity 0)


lookupHolding : ItemCollection -> Item -> ItemHolding
lookupHolding itemCollection item =
    ( item, quantityOfItem itemCollection item )


addItems : Item -> ItemQuantity -> Inventory -> Inventory
addItems item quantity inventory =
    let
        quantityToAdd =
            ItemQuantity.map2 min
                quantity
                (availableInventorySpace inventory)

        newTotal =
            ItemQuantity.add quantityToAdd (quantityOfItem inventory.items item)
    in
        { inventory | items = AllDict.insert item newTotal inventory.items }


drop : Item -> Int -> Inventory -> Inventory
drop item divisor inventory =
    { inventory | items = AllDict.update item (Maybe.map (ItemQuantity.map (((flip (//)) divisor)))) inventory.items }


removeAllItem : Item -> Inventory -> Inventory
removeAllItem item inventory =
    { inventory | items = AllDict.remove item inventory.items }


availableInventorySpace : Inventory -> ItemQuantity
availableInventorySpace inventory =
    ItemQuantity.subtract inventory.maxHolding (slotsUsed inventory.items)


slotsUsed : ItemCollection -> ItemQuantity
slotsUsed =
    ItemQuantity << AllDict.foldl (\_ (ItemQuantity count) acc -> acc + count) 0


emptyAllDict : AllDict Item b Int
emptyAllDict =
    AllDict.empty Item.position


empty : Inventory
empty =
    (Inventory emptyAllDict (ItemQuantity 100))
