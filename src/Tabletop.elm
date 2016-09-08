module Tabletop exposing (..)


type alias Position =
    ( Int, Int )


posX : Position -> Int
posX ( x', _ ) =
    x'

posY : Position -> Int
posY ( _, y' ) =
    y'


type alias Inch =
    Int


type alias Tabletop =
    { width : Int
    , height : Int
    }
