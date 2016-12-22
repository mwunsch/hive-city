module Utilities exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onWithOptions, on)
import Json.Decode as Json
import List
import Mouse
import Svg exposing (Svg, foreignObject)


onClickWithoutPropagation : msg -> Attribute msg
onClickWithoutPropagation message =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed message)


textNode : String -> List (Html msg)
textNode =
    Html.text >> List.repeat 1


htmlAsSvg : String -> List (Attribute msg) -> Html msg -> Svg msg
htmlAsSvg name attrs =
    List.repeat 1
        >> Html.div
            [ class name
            , attribute "xmlns" "http://www.w3.org/1999/xhtml"
            ]
        >> List.repeat 1
        >> foreignObject attrs


onEventWithPosition : (Mouse.Position -> msg) -> String -> Html.Attribute msg
onEventWithPosition message event =
    on event <|
        Json.map message Mouse.position
