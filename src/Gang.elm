module Gang exposing (Gang, init, empty, name, rename, roster, id, stash, recruit, update, fromList, toList, toArray, get, map, generator, positionedGenerator)

import Array exposing (Array)
import Dict exposing (Dict, toList)
import List
import Maybe
import Model exposing (Model, Id)
import Random exposing (Generator, andThen)
import Svg exposing (Svg, g)
import Tabletop exposing (Tabletop)
import Uuid exposing (Uuid, uuid)


type Gang
    = Gang
        { roster : Dict Id Model
        , credits : Int
        , name : String
        , id : Uuid
        }


init : Gang
init =
    Gang
        { roster = Dict.empty
        , credits = 1000
        , name = "Grimm's Reavers"
        , id = Uuid.scheme
        }


empty : Gang
empty =
    Gang
        { roster = Dict.empty
        , credits = 0
        , name = ""
        , id = Uuid.scheme
        }


name : Gang -> String
name (Gang { name }) =
    name


rename : String -> Gang -> Gang
rename name (Gang params) =
    Gang { params | name = name }


roster : Gang -> Dict Id Model
roster (Gang { roster }) =
    roster


id : Gang -> Uuid
id (Gang { id }) =
    id


stash : Gang -> Int
stash (Gang { credits }) =
    credits


{-| From the rulebook:

> You have 1000 Guilder credits to spend on recruiting and arming your
  gang within the following guidelines.

+ Minimum 3 Fighers: A gang must have at least three models.
+ Leader: your gang must have one leader. Not more. Not less!
+ Gangers: You can include as many gangers as you can afford.
+ Heavies: A gang can have up to two heavies but no more.
+ Juves: No more than half the gang can be made up of juves.
+ Knifes: All fighters are assumed to have a knife even if the model doesn't have one.

-}
recruit : Model -> Gang -> Maybe Gang
recruit model (Gang params) =
    let
        cost =
            Model.cost model

        remainingCredits =
            params.credits - cost
    in
        if remainingCredits >= 0 then
            Just
                (Gang
                    { params
                        | roster = (Dict.insert model.id model params.roster)
                        , credits = remainingCredits
                    }
                )
        else
            Nothing


update : Id -> (Maybe Model -> Maybe Model) -> Gang -> Gang
update id transform (Gang params) =
    Gang { params | roster = (Dict.update id transform params.roster) }


toList : Gang -> List Model
toList (Gang { roster }) =
    Dict.values roster


fromList : List Model -> Gang
fromList models =
    List.foldl (\model gang -> recruit model gang |> Maybe.withDefault empty) init models


toArray : Gang -> Array Model
toArray =
    toList >> Array.fromList


get : Id -> Gang -> Maybe Model
get id (Gang { roster }) =
    Dict.get id roster


map : (Id -> Model -> Model) -> Gang -> Gang
map transform (Gang params) =
    Gang { params | roster = Dict.map transform params.roster }


generator : Generator Gang
generator =
    Model.generator Model.Leader
        |> andThen
            (\leader ->
                Random.list 4 (Model.generator Model.Ganger)
                    |> Random.map ((::) leader)
                    |> Random.map fromList
            )
        |> andThen
            (\(Gang gang) ->
                uuid
                    |> Random.map (\id -> Gang { gang | id = id })
            )


positionedGenerator : Tabletop -> Generator Gang
positionedGenerator table =
    generator
        |> andThen
            (\(Gang gang) ->
                let
                    fighters =
                        Dict.toList gang.roster
                in
                    Random.list (List.length fighters) (Tabletop.positionGenerator table)
                        |> Random.map
                            (List.map2 (\( id, fighter ) pos -> ( id, { fighter | position = pos } )) fighters)
                        |> Random.map (Dict.fromList)
                        |> Random.map (\fighters -> Gang { gang | roster = fighters })
            )
