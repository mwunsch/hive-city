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
        , action = Await
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


type Instruction
    = Movement Model Position


takeAction : Instruction -> Player -> Player
takeAction instruction player =
    case instruction of
        Movement fighter pos ->
            let
                move : Model -> Model
                move f =
                    case Model.attemptMove f pos of
                        Ok model ->
                            model

                        Err ( _, model ) ->
                            model
            in
                { player | gang = Gang.update fighter.id (Maybe.map move) player.gang }

await : Player -> Player
await player =
    { player | action = Await }



-- VIEW


view : Player -> Phase -> (Action -> msg) -> Svg msg
view player phase msg =
    getSelectedGangMember player
        |> Maybe.map (actionView player phase msg)
        |> Maybe.withDefault (Action.emptyView)


actionView : Player -> Phase -> (Action -> msg) -> Model -> Svg msg
actionView player phase msg fighter =
    case player.action of
        Await ->
            Action.viewSelection phase fighter msg

        Move ->
            Tabletop.viewMeasuringTape fighter.position player.movementIntention fighter.remainingMove

        _ ->
            Action.emptyView
