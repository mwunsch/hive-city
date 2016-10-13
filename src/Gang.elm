module Gang exposing (..)

import Dict exposing (Dict, toList)
import List
import Model exposing (Model, Id)
import Random exposing (Generator, andThen)
import Svg exposing (Svg)
import Tabletop exposing (Tabletop)


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


positionedGenerator : Tabletop -> Generator Gang
positionedGenerator table =
    Random.list 5 Model.generator
        `andThen`
            \models ->
                Random.list (List.length models) (Tabletop.positionGenerator table)
                    |> Random.map (List.map2 (,) models)
                    |> Random.map (List.map (\( model, pos ) -> { model | position = pos }))
                    |> Random.map fromList


view : Gang -> (Model -> msg) -> List (Svg msg)
view gang msg =
    Dict.values gang
        |> List.map (\fighter -> Model.view fighter (msg fighter))
