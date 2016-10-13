module Player exposing (..)

import Dict
import Gang exposing (Gang)
import Maybe exposing (andThen)
import Model exposing (Model)
import Tabletop exposing (Tabletop, Position)


type alias Player =
    { gang : Gang
    , selection : Maybe Model.Id
    , movementIntention : Position
    }


init : Tabletop -> Player
init table =
    { gang = Gang.empty
    , selection = Nothing
    , movementIntention = Tabletop.center table
    }


selectModel : Player -> Model.Id -> Player
selectModel player id =
    let
        updatedPlayer =
            updateGangMember (deselectAll player) id <| Maybe.map (\f -> { f | selected = True })
    in
        { updatedPlayer | selection = Just id }


deselectAll : Player -> Player
deselectAll player =
    let
        updatedGang =
            Dict.map (\k v -> { v | selected = False }) player.gang
    in
        { player
            | gang = updatedGang
            , selection = Nothing
        }


getGangMember : Player -> Model.Id -> Maybe Model
getGangMember player id =
    Dict.get id player.gang


updateGangMember : Player -> Model.Id -> (Maybe Model -> Maybe Model) -> Player
updateGangMember player id f =
    { player | gang = player.gang |> Dict.update id f }


getSelectedGangMember : Player -> Maybe Model
getSelectedGangMember player =
    player.selection `andThen` (\id -> getGangMember player id)
