module Player exposing (..)

import Action exposing (Action(..))
import Gang exposing (Gang)
import Maybe exposing (andThen)
import Model exposing (Model)
import Svg exposing (Svg)
import Tabletop exposing (Tabletop, Position)
import Turn exposing (Phase)


type alias Player =
    { gang : Gang
    , selection : Maybe Model.Id
    , movementIntention : Position
    , action : Action
    }


init : Tabletop -> Player
init table =
    { gang = Gang.empty
    , selection = Nothing
    , movementIntention = Tabletop.center table
    , action = Await
    }


selectModel : Player -> Model.Id -> Player
selectModel player id =
    { player
        | gang =
            .gang (deselectAll player)
                |> Gang.update id (Maybe.map (\f -> { f | selected = True }))
        , selection = Just id
    }


deselectAll : Player -> Player
deselectAll player =
    { player
        | gang =
            player.gang
                |> Gang.map (\k v -> { v | selected = False })
        , selection = Nothing
    }


getSelectedGangMember : Player -> Maybe Model
getSelectedGangMember player =
    player.selection `andThen` (flip Gang.get) player.gang


view : Player -> Phase -> Svg msg
view player phase =
    getSelectedGangMember player
        |> Maybe.map (Action.view player.action phase)
        |> Maybe.withDefault (Action.emptyView)
