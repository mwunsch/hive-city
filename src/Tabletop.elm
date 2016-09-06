module Tabletop exposing (..)


type alias Position =
    ( Int, Int )


posX ( x', _ ) =
    x'


posY ( _, y' ) =
    y'


type alias Inch =
    Int


type alias Tabletop =
    { width : Int
    , height : Int
    }
