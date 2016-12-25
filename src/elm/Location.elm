module Location exposing (Location(..), all, home)


type Location
    = Location1
    | Location2
    | Location3
    | Location4
    | Location5
    | Location6


all : List Location
all =
    [ Location1, Location2, Location3, Location4, Location5, Location6 ]


home : Location -> Bool
home =
    (==) Location1
