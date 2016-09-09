module Tabletop exposing (..)

import Html exposing (Html)
import Svg exposing (..)

{-| The Tabletop module exposes types and functions related to the structure of
the game board as well as positioning and movement on the board.

-}

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

view : Tabletop -> Html Msg
view tabletop =
    svg [ ] [ ]
