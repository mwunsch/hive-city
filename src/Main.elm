module Main exposing (..)

import Action exposing (Action)
import Array
import Gang exposing (Gang)
import Html exposing (Html)
import Html.App as App
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Keyboard
import List exposing (map)
import Maybe exposing (andThen)
import Model exposing (Model)
import Mouse
import Player exposing (Player)
import Random
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Tabletop, positionFromMouseCoords)
import Task
import Turn exposing (Turn, Phase(..))
import Window


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias GameState =
    { player : Player
    , tabletop : Tabletop
    , windowWidth : Int
    , windowScale : Float
    , turn : Turn
    }


init : ( GameState, Cmd Msg )
init =
    let
        table =
            6 `Tabletop.by` 4
    in
        ( { player = Player.init table
          , tabletop = table
          , windowWidth = 1000
          , windowScale = 10
          , turn = Turn.init
          }
        , Cmd.batch
            [ Task.perform (always NoOp) Resize Window.width
            , Random.generate Generate (Gang.positionedGenerator table)
            ]
        )



-- UPDATE


type Msg
    = Select Model
    | Click Mouse.Position
    | Command Action
    | Complete Action
    | Hover Mouse.Position
    | KeyPress Keyboard.KeyCode
    | Resize Int
    | Generate Gang
    | NoOp
    | Log String String


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        Select model ->
            case game.player.action of
                Action.Shoot ->
                    case Player.getSelectedGangMember game.player of
                        Just fighter ->
                            Player.execute (Player.Shooting fighter model) game.player
                                |> \( player, task ) ->
                                    ( { game | player = player }
                                    , Task.perform (always NoOp) Complete task
                                    )

                        Nothing ->
                            ( game, Cmd.none )

                _ ->
                    ( { game | player = Player.selectModel game.player model.id }, Cmd.none )

        Click { x, y } ->
            case Player.getSelectedGangMember game.player of
                Just fighter ->
                    let
                        pos =
                            positionFromMouseCoords ( x, y ) game.windowScale
                    in
                        case game.player.action of
                            Action.Move ->
                                Player.execute (Player.Moving fighter pos) game.player
                                    |> \( player, task ) ->
                                        ( { game | player = player }
                                        , Task.perform (always NoOp) Complete task
                                        )

                            _ ->
                                if Tabletop.isWithinDistance 2 fighter.position pos then
                                    ( game, Cmd.none )
                                else
                                    ( { game | player = Player.deselectAll game.player }, Cmd.none )

                Nothing ->
                    ( game, Cmd.none )

        Command action ->
            ( { game | player = game.player |> \p -> { p | action = action } }, Cmd.none )

        Complete action ->
            let
                updateGame : GameState
                updateGame =
                    case action of
                        Action.Move ->
                            if Gang.toList game.player.gang |> List.any (\m -> m.remainingMove > 0) then
                                game
                            else
                                { game
                                    | turn = Turn.advance game.turn
                                    , player = Player.deselectAll game.player
                                }

                        _ ->
                            game
            in
                ( { updateGame | player = Player.await updateGame.player }, Cmd.none )

        Hover { x, y } ->
            game.player
                |> (\player -> { player | movementIntention = positionFromMouseCoords ( x, y ) game.windowScale })
                |> \player -> ( { game | player = player }, Cmd.none )

        KeyPress key ->
            let
                lookupFighter : Int -> GameState
                lookupFighter index =
                    Gang.toArray game.player.gang
                        |> Array.get index
                        |> Maybe.map (\{ id } -> { game | player = Player.selectModel game.player id })
                        |> Maybe.withDefault game
            in
                case key of
                    27 ->
                        -- ESCAPE
                        ( { game | player = Player.deselectAll game.player }, Cmd.none )

                    49 ->
                        ( lookupFighter 0, Cmd.none )

                    50 ->
                        ( lookupFighter 1, Cmd.none )

                    51 ->
                        ( lookupFighter 2, Cmd.none )

                    52 ->
                        ( lookupFighter 3, Cmd.none )

                    53 ->
                        ( lookupFighter 4, Cmd.none )

                    54 ->
                        ( lookupFighter 5, Cmd.none )

                    _ ->
                        ( game, Cmd.none )

        Resize w ->
            ( { game
                | windowWidth = w
                , windowScale = (toFloat w) / (toFloat game.tabletop.width)
              }
            , Cmd.none
            )

        Generate gang ->
            ( { game
                | player =
                    game.player |> \p -> { p | gang = gang }
              }
            , Cmd.none
            )

        NoOp ->
            ( game, Cmd.none )

        Log tag value ->
            let
                log =
                    Debug.log tag value
            in
                ( game, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GameState -> Sub Msg
subscriptions game =
    Sub.batch
        [ Window.resizes (\size -> Resize size.width)
        , Mouse.moves Hover
        , Keyboard.presses KeyPress
        ]



-- VIEW


onClickWithCoords : (Mouse.Position -> msg) -> Html.Attribute msg
onClickWithCoords message =
    on "click" <|
        Json.map message Mouse.position


view : GameState -> Html Msg
view game =
    let
        actionSelection =
            Player.view game.player (Turn.phase game.turn) Command

        selectedFighterProfile =
            Player.getSelectedGangMember game.player
                |> Maybe.map Model.viewProfile
                |> Maybe.withDefault (Html.table [] [])

        definitions =
            defs [] []
    in
        Html.div []
            [ svg
                [ viewBox
                    ([ 0, 0, game.tabletop.width, game.tabletop.height ] |> map toString |> join " ")
                , width
                    (game.windowWidth |> toString)
                , onClickWithCoords Click
                ]
                [ definitions
                , Tabletop.view game.tabletop
                , actionSelection
                , Gang.view game.player.gang Select
                ]
            , Html.strong [] [ Html.text (Turn.phase game.turn |> toString) ]
            , selectedFighterProfile
            ]
