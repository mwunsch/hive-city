module Tabletop exposing (..)

import Random exposing (Generator)
import String
import Svg exposing (Svg, rect, g, line)
import Svg.Attributes exposing (..)


{-| The Tabletop module exposes types and functions related to the
structure of the game board as well as positioning and movement on the
board.

-}
type alias Tabletop =
    { width : Int
    , height : Int
    }


type alias Inch =
    Float


type alias Position =
    ( Float, Float )


posX : Position -> Float
posX ( x', _ ) =
    x'


posY : Position -> Float
posY ( _, y' ) =
    y'


positionGenerator : Tabletop -> Generator Position
positionGenerator { width, height } =
    Random.pair (Random.float 0 (toFloat width)) (Random.float 0 (toFloat height))


offTable : Position
offTable =
    ( -1, -1 )


foot : Float -> Inch
foot =
    (*) 12


millimeter : Float -> Inch
millimeter =
    (*) 0.0393701


by : Float -> Float -> Tabletop
by w h =
    Tabletop (round <| foot w) (round <| foot h)


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


center : Tabletop -> Position
center table =
    ( toFloat table.width / 2, toFloat table.height / 2 )


distance : Position -> Position -> Inch
distance ( x1, y1 ) ( x2, y2 ) =
    let
        y' =
            (y1 - y2) ^ 2

        x' =
            (x1 - x2) ^ 2
    in
        sqrt (x' + y')


positionFromDirection : Position -> Position -> Inch -> Position
positionFromDirection start end len =
    let
        h =
            distance start end

        x' =
            (posX end) - (posX start)

        y' =
            (posY end) - (posY start)

        angle =
            acos (y' / h)

        co =
            if x' > 0 then
                1
            else
                -1
    in
        ( (posX start) + (sin angle * len) * co
        , (posY start) + (cos angle * len)
        )


isWithinDistance : Inch -> Position -> Position -> Bool
isWithinDistance r start end =
    distance start end < r


view : Tabletop -> Svg msg
view tabletop =
    rect
        [ width (tabletop.width |> toString)
        , height (tabletop.height |> toString)
        , fill "silver"
        ]
        []


viewMeasuringTape : Position -> Position -> Inch -> Svg msg
viewMeasuringTape start end range =
    let
        length =
            distance start end

        ( rangeX, rangeY ) =
            if length > range then
                positionFromDirection start end range
            else
                end
    in
        g []
            [ line
                [ x1 (start |> posX |> toString)
                , y1 (start |> posY |> toString)
                , x2 (rangeX |> toString)
                , y2 (rangeY |> toString)
                , stroke "yellow"
                , strokeWidth (millimeter 16 |> toString)
                ]
                []
            , line
                [ x1 (rangeX |> toString)
                , y1 (rangeY |> toString)
                , x2 (end |> posX |> toString)
                , y2 (end |> posY |> toString)
                , stroke "grey"
                , strokeWidth (millimeter 16 |> toString)
                ]
                []
            ]


transformTranslate : Position -> Svg.Attribute msg
transformTranslate ( x, y ) =
    List.map toString [ x, y ]
        |> String.join ","
        |> (\str -> String.concat [ "translate(", str, ")" ])
        |> transform
