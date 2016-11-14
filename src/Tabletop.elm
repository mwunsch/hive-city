module Tabletop exposing (..)

import Random exposing (Generator)
import String
import Svg exposing (Svg, rect, g, line, marker, circle, path, polygon)
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
posX ( x, _ ) =
    x


posY : Position -> Float
posY ( _, y ) =
    y


aspectRatio : Tabletop -> Float
aspectRatio { width, height } =
    toFloat width / toFloat height


positionGenerator : Tabletop -> Generator Position
positionGenerator { width, height } =
    Random.pair (Random.float 1 (toFloat width - 1)) (Random.float 1 (toFloat height - 1))


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


center : Tabletop -> Position
center table =
    ( toFloat table.width / 2, toFloat table.height / 2 )


distance : Position -> Position -> Inch
distance ( x1, y1 ) ( x2, y2 ) =
    let
        y =
            (y1 - y2) ^ 2

        x =
            (x1 - x2) ^ 2
    in
        sqrt (x + y)


positionFromDirection : Position -> Position -> Inch -> Position
positionFromDirection start end len =
    let
        h =
            distance start end

        x =
            (posX end) - (posX start)

        y =
            (posY end) - (posY start)

        angle =
            acos (y / h)

        co =
            if x > 0 then
                1
            else
                -1
    in
        ( (posX start) + (sin angle * len) * co
        , (posY start) + (cos angle * len)
        )


angle : Position -> Position -> Float
angle ( startX, startY ) ( endX, endY ) =
    let
        y =
            endY - startY

        x =
            endX - startX
    in
        atan2 y x |> (*) 180 |> (flip (/)) pi


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

        markerRef =
            if (length <= range) then
                "url(#measuring-marker)"
            else
                "none"
    in
        line
            [ x1 (start |> posX |> toString)
            , y1 (start |> posY |> toString)
            , x2 (rangeX |> toString)
            , y2 (rangeY |> toString)
            , stroke "yellow"
            , strokeWidth (millimeter 16 |> toString)
            , markerEnd markerRef
            , opacity "0.65"
            , strokeLinecap "round"
            , class "measuringTape"
            ]
            []


viewMarker : Svg msg
viewMarker =
    marker
        [ id "measuring-marker"
        , markerHeight "2"
        , markerWidth "2"
        , refX "0.5"
        , refY "1"
        , markerUnits "strokeWidth"
        , orient "auto"
        ]
        [ Svg.path [ d "M0,0 L0,2 L2,1 z", fill "yellow" ] [] ]


transformTranslate : Position -> Svg.Attribute msg
transformTranslate ( x, y ) =
    List.map toString [ x, y ]
        |> String.join ","
        |> (\str -> String.concat [ "translate(", str, ")" ])
        |> transform
