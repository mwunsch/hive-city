module Player exposing (..)

import Action exposing (Action(..))
import Gang exposing (Gang)
import Maybe exposing (andThen)
import Model exposing (Model)
import Svg exposing (Svg)
import Tabletop exposing (Tabletop, Position)
import Task exposing (Task)
import Turn exposing (Phase)


type alias Player =
    { gang : Gang
    , selection : Maybe Model.Id
    , movementIntention : Position
    , action : Action
    , target : Maybe Model.Id
    }


init : Tabletop -> Player
init table =
    { gang = Gang.empty
    , selection = Nothing
    , movementIntention = Tabletop.center table
    , action = Await
    , target = Nothing
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


{-| A Player is always taking some Action (even just Awaiting
input). The `Instruction` type describes the execution of that action,
along with what is necessary to execute it.

-}
type Instruction
    = Moving Model Position
    | Shooting Model Model


type Failure
    = FailedToMove Model


{-| The `execute` function executes the Instruction, and returns a pair of
an updated Player and a `Task`.

Why a Task? It's a stub so that we can figure out Dice rolls and
animation. Likely, this will become a `Cmd`.

Note that an Instruction can be executed even when the Player is not
explicitly taking some Action. The returned Task is parameterized with
the Action that corresponds to the Instruction.
-}
execute : Instruction -> Player -> ( Player, Task Failure Action )
execute instruction player =
    case instruction of
        Moving fighter pos ->
            let
                move : Model -> Model
                move f =
                    case Model.attemptMove f pos of
                        Ok model ->
                            model

                        Err ( _, model ) ->
                            model
            in
                ( { player | gang = Gang.update fighter.id (Maybe.map move) player.gang }
                , Task.succeed Move
                )

        Shooting attacker target ->
            ( { player | target = Just target.id }
            , Task.succeed Shoot
            )


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

        Shoot ->
            Action.emptyView

        _ ->
            Action.unimplementedView player.action fighter
