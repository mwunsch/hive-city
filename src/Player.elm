module Player exposing (..)

import Action exposing (Action(..), Failure(..))
import Dice exposing (Dice, oneD6)
import Gang exposing (Gang)
import Maybe exposing (andThen)
import Model exposing (Model)
import Svg exposing (Svg, g)
import Svg.Attributes exposing (fill)
import Tabletop exposing (Tabletop, Position)
import Task exposing (Task)
import Turn exposing (Phase)
import Weapons exposing (Weapon, autogun)


type alias Player =
    { gang : Gang
    , selection : Maybe Model.Id
    , movementIntention : Position
    , action : Action
    , target : Maybe Model.Id
    , color : String
    }


init : Tabletop -> Player
init table =
    { gang = Gang.empty
    , selection = Nothing
    , movementIntention = Tabletop.center table
    , action = Await
    , target = Nothing
    , color = "blue"
    }


selectModel : Player -> Model.Id -> Player
selectModel player id =
    let
        select =
            .gang (deselectAll player)
                |> Gang.update id (Maybe.map (\f -> { f | selected = True }))
    in
        { player
            | gang = select
            , selection = Gang.get id select |> Maybe.map (.id)
            , action = Await
        }


deselectAll : Player -> Player
deselectAll player =
    { player
        | gang =
            player.gang
                |> Gang.map (\k v -> { v | selected = False })
        , selection = Nothing
        , target = Nothing
        , action = Await
    }


getSelectedGangMember : Player -> Maybe Model
getSelectedGangMember player =
    player.selection |> andThen ((flip Gang.get) player.gang)


updateSelectedGangMember : Player -> (Model -> Model) -> Gang
updateSelectedGangMember player transform =
    let
        updateGang : Model -> Gang
        updateGang model =
            Gang.update model.id (Maybe.map transform) player.gang
    in
        getSelectedGangMember player
            |> Maybe.map (updateGang)
            |> Maybe.withDefault player.gang


getClosestModelInWeaponRange : Player -> Weapon -> Maybe Model
getClosestModelInWeaponRange player weapon =
    getSelectedGangMember player
        |> Maybe.map (Model.withinShootingRange (Gang.toList player.gang) weapon)
        |> Maybe.withDefault []
        |> List.head


{-| A Player is always taking some Action (even just Awaiting
input). The `Instruction` type describes the execution of that action,
along with what is necessary to execute it.

-}
type Instruction
    = Moving Model Position
    | Running Model Position
    | Shooting Model Model Weapon


type alias DiceRoll =
    ( Int, Int, Instruction )


{-| The `execute` function executes the Instruction, and returns a
pair of an updated Player and a `Cmd (Result Failure Action)`.

Note that an Instruction can be executed even when the Player is not
explicitly taking some Action. The returned Cmd is parameterized with
the Action that corresponds to the Instruction.

The caller of execute (likely the main `update` loop, is assumed to
use the `Tuple.map*` functions to massage the returned pair.

-}
execute : Instruction -> Dice -> Player -> ( Player, Cmd (Result Failure Action) )
execute instruction dice player =
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
                , Task.perform Ok (Task.succeed Move)
                )

        Running fighter pos ->
            ( { player | gang = Gang.update fighter.id (Maybe.map ((flip Model.run) pos)) player.gang }
            , Task.perform Ok (Task.succeed Run)
            )

        Shooting attacker target weapon ->
            ( { player | target = Just target.id }
            , dice |> Dice.roll (Model.shoot attacker weapon target >> Weapons.toResult MissedShot (Shoot weapon))
            )


await : Player -> Player
await player =
    { player | action = Await }



-- VIEW


view : Player -> Phase -> Svg msg
view player phase =
    getSelectedGangMember player
        |> Maybe.map (actionView player phase)
        |> Maybe.withDefault (Action.emptyView)


gangView : Player -> (Model -> msg) -> Svg msg
gangView player msg =
    Gang.toList player.gang
        |> List.map (\fighter -> Model.view fighter (msg fighter))
        |> g [ fill player.color ]


actionView : Player -> Phase -> Model -> Svg msg
actionView player phase fighter =
    case player.action of
        Await ->
            Action.viewSelection fighter

        Move ->
            Tabletop.viewMeasuringTape fighter.position player.movementIntention fighter.remainingMove

        Run ->
            Tabletop.viewMeasuringTape fighter.position player.movementIntention (fighter.remainingMove * 2)

        Shoot weapon ->
            Tabletop.viewArc fighter.position fighter.bearing (Weapons.maxRange weapon)

        _ ->
            Action.unimplementedView player.action fighter
