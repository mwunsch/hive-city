module Gang exposing (..)

import Dict exposing (Dict)
import List
import Model exposing (Model, Id)
import Random exposing (Generator)
import Svg exposing (Svg)


type alias Gang =
    Dict Id Model


empty : Gang
empty =
    Dict.empty


fromList : List Model -> Gang
fromList =
    Dict.fromList << List.map (\m -> ( m.id, m ))


generator : Generator Gang
generator =
    Random.list 5 Model.generator
        |> Random.map fromList


view : Gang -> (Model -> msg) -> List (Svg msg)
view gang msg =
    Dict.values gang
        |> List.map (\fighter -> Model.view fighter (msg fighter))
