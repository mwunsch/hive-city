module Utilities exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import List
import Svg exposing (Svg, foreignObject)


onClickWithoutPropagation : msg -> Attribute msg
onClickWithoutPropagation message =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed message)


textNode : String -> List (Html msg)
textNode =
    Html.text >> List.repeat 1


htmlAsSvg : List (Attribute msg) -> Html msg -> Svg msg
htmlAsSvg attrs =
    List.repeat 1
        >> Html.div [ attribute "xmlns" "http://www.w3.org/1999/xhtml" ]
        >> List.repeat 1
        >> foreignObject attrs
