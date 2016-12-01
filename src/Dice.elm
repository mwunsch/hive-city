module Dice exposing (..)

import List
import Random exposing (Generator, generate)
import Svg exposing (Svg, text_, g)
import Svg.Attributes exposing (..)
import Utilities exposing (onClickWithoutPropagation, textNode)


type alias Die =
    Int


type alias Dice =
    Generator (List Die)


{-| The `d` function is a constructor function for dice. Like
`andThen`, the arguments are reversed. If the rulebook says "2D6", you
would use the pipe argument like:

    2 |> d 6

-}
d : Int -> Int -> Dice
d val num =
    Random.int 1 val
        |> Random.list num


d6 : Int -> Dice
d6 num =
    num |> d 6


oneD6 : Dice
oneD6 =
    d6 1


{-| From the rulebook:

> There isn’t actually any such thing as a 3- sided dice, instead a D3
> means roll a D6 and halve the result rounding up.

-}
d3 : Int -> Dice
d3 num =
    d6 num
        |> Random.map (List.map (toFloat >> (*) 0.5 >> ceiling))


roll : (List Die -> msg) -> Dice -> Cmd msg
roll =
    generate


viewRoll : Int -> Int -> String -> msg -> Svg msg
viewRoll num val mod msg =
    text_
        [ fontSize "2"
        , alignmentBaseline "middle"
        , textAnchor "start"
        , onClickWithoutPropagation msg
        , cursor "pointer"
        ]
        (textNode "🎲")
