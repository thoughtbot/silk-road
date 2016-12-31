module I18n exposing (Language(..), Translation(..), translate)

import Item exposing (Item(..))
import Currency exposing (Currency(..))
import Location exposing (Location(..))
import ItemQuantity exposing (ItemQuantity(..))


type Language
    = DrugWars
    | SilkRoad


type alias TranslationSet =
    { drugWars : String
    , silkRoad : String
    }


type Translation
    = GameTitle
    | CurrencyText { currency : Currency }
    | CurrentLocationHeader
    | DaysRemainingHeader
    | CurrencyOnHandHeader
    | DebtHeader
    | BankHeader
    | StatusHeader
    | SellItemsHeader
    | BuyItemsHeader
    | TravelHeader
    | AvailableInventoryHeader
    | PriceRangeHeader
    | ItemName { item : Item }
    | LocationName { location : Location }
    | PriceHikeMessage { item : Item }
    | PriceDropMessage { item : Item }
    | MuggedMessage
    | DroppedItemMessage { item : Item }
    | FoundItemMessage { item : Item, quantity : ItemQuantity }
    | PayLenderButton
    | BorrowMaxButton
    | DepositCashButton
    | WithdrawCashButton
    | ReturnToGameButton
    | SeePriceRangesButton
    | RestartGameButton


translate : Language -> Translation -> String
translate language trans =
    let
        translationSet =
            case trans of
                GameTitle ->
                    TranslationSet "Drug Wars" "Silk Road"

                CurrencyText { currency } ->
                    let
                        displayCurrency (Currency amount) =
                            toString amount
                    in
                        TranslationSet ("$" ++ displayCurrency currency) (displayCurrency currency ++ "G")

                CurrentLocationHeader ->
                    TranslationSet "Current Location" "Market"

                DaysRemainingHeader ->
                    TranslationSet "Days Remaining" "Weeks Remaining"

                CurrencyOnHandHeader ->
                    TranslationSet "Cash" "Gold"

                DebtHeader ->
                    TranslationSet "Debt" "Debt"

                BankHeader ->
                    TranslationSet "Bank" "Exchange"

                StatusHeader ->
                    TranslationSet "Status, buddy" "Game Status"

                SellItemsHeader ->
                    TranslationSet "Sell" "Sell Goods"

                BuyItemsHeader ->
                    TranslationSet "Drug Prices" "Buy Goods"

                TravelHeader ->
                    TranslationSet "Take a trip" "Journey"

                AvailableInventoryHeader ->
                    TranslationSet "Slots" "Slots"

                PriceRangeHeader ->
                    TranslationSet "Price Ranges" "Price Ranges"

                ItemName { item } ->
                    case item of
                        Item1 ->
                            TranslationSet "Cocaine" "Silk"

                        Item2 ->
                            TranslationSet "Heroin" "Tortoise Shell"

                        Item3 ->
                            TranslationSet "Acid" "Myrrh"

                        Item4 ->
                            TranslationSet "Weed" "Ruby"

                        Item5 ->
                            TranslationSet "Speed" "Oil"

                        Item6 ->
                            TranslationSet "Ludes" "Meat"

                LocationName { location } ->
                    case location of
                        Location1 ->
                            TranslationSet "the Bronx" "Bagdad"

                        Location2 ->
                            TranslationSet "Central Park" "Kairo"

                        Location3 ->
                            TranslationSet "Coney Island" "Konstantinopel"

                        Location4 ->
                            TranslationSet "the Ghetto" "Buchara"

                        Location5 ->
                            TranslationSet "Brooklyn" "Peking"

                        Location6 ->
                            TranslationSet "Manhattan" "Samarkand"

                PriceHikeMessage { item } ->
                    case item of
                        Item1 ->
                            TranslationSet "Cops just busted the local provider. Cocaine prices have spiked" "Sanctions have heavily reduced silk exports. Silk prices have increased drastically."

                        Item2 ->
                            TranslationSet "Cops just busted the local provider. Heroin prices have spiked" "Increased acidity in the water has reduced tortoise migrations. Shell prices have gone up."

                        Item3 ->
                            TranslationSet "Production problems have caused a shortage. Acid is super expensive" "Famine swept the region, and Myrrh usage has increased for burials. Myrrh prices have increased."

                        Item4 ->
                            TranslationSet "Bad harvest this year. Weed is super expensive" "Merchants have shipped Ruby surplus based on demand; little availability has resulted in higher prices."

                        Item5 ->
                            TranslationSet "Local provider has retired. Speed is pricey" "A cold has spread throughout the land, and heating oil use has increased. Prices reflect higher demand."

                        Item6 ->
                            TranslationSet "Lotta people want Ludes these days. You're gonna have to pay..." "A disease has killed a significant portion of the livestock; meat is scarce."

                PriceDropMessage { item } ->
                    case item of
                        Item1 ->
                            TranslationSet "A new shipment has just come in from Columbia. Cocaine prices have plummeted" "Silk production has been reproduced over the season and more garments are available than usual. Silk prices have dropped."

                        Item2 ->
                            TranslationSet "No one's doing heroin these days. Prices have plummetted" "A merchant brought more Tortoise Shell than demand required, so prices have dropped throughout the market."

                        Item3 ->
                            TranslationSet "New production equipment has made Acid more plentiful." "Myrrh production has increased, resulting in excess at the market. Prices have plummeted."

                        Item4 ->
                            TranslationSet "Bumper crop this year. The bottom has fallen out of weed prices" "Ruby prices have dropped after an increased interest in other gemstones."

                        Item5 ->
                            TranslationSet "Someone just dumped speed on the market. Prices are low." "Unseasonable heat has spread throughout the land; oil prices have suffered as a result."

                        Item6 ->
                            TranslationSet "Someone just hit up the local phramacy. CHEAP LUDES!!!" "Multiple cattle herders have brought livestock; prices have been reduced so they can rid themselves of excess inventory."

                MuggedMessage ->
                    TranslationSet "You got mugged! The perpetrator took off with half your cash" "Thieves robbed you overnight, and made off with half of your gold."

                DroppedItemMessage { item } ->
                    TranslationSet
                        ("Oh no, you dropped a bunch of " ++ translate language (ItemName { item = item }) ++ ", bud. Bummer")
                        ("You hit a rocky patch of road and lost some of your " ++ translate language (ItemName { item = item }) ++ ".")

                FoundItemMessage { item, quantity } ->
                    let
                        displayAmount (ItemQuantity amount) =
                            toString amount
                    in
                        TranslationSet
                            ("You found " ++ displayAmount quantity ++ " " ++ translate language (ItemName { item = item }) ++ " on the ground")
                            ("When stopping for a break, you found " ++ displayAmount quantity ++ " " ++ translate language (ItemName { item = item }) ++ " hidden in a ditch.")

                PayLenderButton ->
                    TranslationSet "Pay Loan Shark" "Repay Lender"

                BorrowMaxButton ->
                    TranslationSet "Borrow max from Loan Shark" "Borrow max from Lender"

                DepositCashButton ->
                    TranslationSet "Deposit cash" "Deposit gold"

                WithdrawCashButton ->
                    TranslationSet "Withdraw cash" "Withdraw gold"

                SeePriceRangesButton ->
                    TranslationSet "See price ranges" "See price ranges"

                ReturnToGameButton ->
                    TranslationSet "Return to game" "Return to game"

                RestartGameButton ->
                    TranslationSet "Play again?" "Play again?"
    in
        case language of
            DrugWars ->
                .drugWars translationSet

            SilkRoad ->
                .silkRoad translationSet
