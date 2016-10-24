module Utilities exposing (..)

import Html exposing (Html, Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import List


onClickWithoutPropagation : msg -> Attribute msg
onClickWithoutPropagation message =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed message)


textNode : String -> List (Html msg)
textNode =
    Html.text >> List.repeat 1
