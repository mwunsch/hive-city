module Gang exposing (..)

import Dict exposing (Dict)
import Model exposing (Model, Id)
import Svg exposing (Svg)

type alias Gang = Dict Id Model

view : Gang -> (Model -> msg) -> List (Svg msg)
view gang msg =
    Dict.values gang
        |> List.map (\fighter -> Model.view fighter (msg fighter))
