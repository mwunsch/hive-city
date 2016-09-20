module Tabletop exposing (..)

import Svg exposing (rect, Svg)
import Svg.Attributes exposing (..)


{-| The Tabletop module exposes types and functions related to the
structure of the game board as well as positioning and movement on the
board.

-}
type alias Tabletop =
    { width : Int
    , height : Int
    }


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


view : Tabletop -> List (Svg msg) -> Svg msg
view tabletop children =
    rect
        [ width (tabletop.width |> toString)
        , height (tabletop.height |> toString)
        , fill "red"
        ]
        children
