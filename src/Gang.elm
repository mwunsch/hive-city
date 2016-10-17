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


update : Id -> (Maybe Model -> Maybe Model) -> Gang -> Gang
update =
    Dict.update


get : Id -> Gang -> Maybe Model
get =
    Dict.get


add : Model -> Gang -> Gang
add model gang =
    Dict.insert model.id model gang


map : (Id -> Model -> Model) -> Gang -> Gang
map =
    Dict.map


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
                    |> Random.map
                        (fromList << List.map2 (\model pos -> { model | position = pos }) models)


view : Gang -> (Model -> msg) -> List (Svg msg)
view gang msg =
    Dict.values gang
        |> List.map (\fighter -> Model.view fighter (msg fighter))
