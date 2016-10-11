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
    ( Float, Float )


posX : Position -> Float
posX ( x', _ ) =
    x'


posY : Position -> Float
posY ( _, y' ) =
    y'


positionFromMouseCoords : ( Int, Int ) -> Float -> Position
positionFromMouseCoords ( x, y ) scale =
    let
        transform : Int -> Float
        transform a =
            (toFloat a) / scale

        x' =
            transform x

        y' =
            transform y
    in
        ( x', y' )


type alias Inch =
    Int


distance : Position -> Position -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    let
        y' =
            (y1 - y2) ^ 2

        x' =
            (x1 - x2) ^ 2
    in
        (x' + y') |> sqrt


positionFromDirection : Position -> Position -> Float -> Position
positionFromDirection start end len =
    let
        h =
            distance start end

        x' =
            (posX end) - (posX start)

        y' =
            (posY end) - (posY start)

        angle =
            acos <| y' / h

        co =
            if x' > 0 then
                1
            else
                -1
    in
        ( (posX start) + (sin angle * len) * co
        , (posY start) + (cos angle * len)
        )


view : Tabletop -> List (Svg msg) -> Svg msg
view tabletop children =
    rect
        [ width (tabletop.width |> toString)
        , height (tabletop.height |> toString)
        , fill "silver"
        ]
        children
