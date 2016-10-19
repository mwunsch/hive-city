module Player exposing (..)

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
