module Main exposing (..)

import Action exposing (Action)
import Array
import Gang exposing (Gang)
import Html exposing (Html)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
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
import Tabletop exposing (Tabletop)
import Task
import Turn exposing (Turn, Phase(..))
import Utilities exposing (textNode, htmlAsSvg)
import Window


main : Program Never GameState Msg
main =
    Html.program
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
    , offset : Int
    , turn : Turn
    , contextMessage : Maybe ContextMessage
    }


type alias ContextMessage =
    ( String, String )


init : ( GameState, Cmd Msg )
init =
    let
        table =
            Tabletop.by 6 4
    in
        ( { player = Player.init table
          , tabletop = table
          , windowWidth = 1000
          , windowScale = 10
          , offset = 10
          , turn = Turn.init
          , contextMessage = Nothing
          }
        , Cmd.batch
            [ Task.perform Resize Window.width
            , Random.generate Generate (Gang.positionedGenerator table)
            ]
        )



-- UPDATE


type Msg
    = Select Model
    | Click Mouse.Position
    | Command Action
    | Complete Action
    | Advance
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
                Action.Shoot weapon ->
                    case Player.getSelectedGangMember game.player of
                        Just fighter ->
                            Player.execute (Player.Shooting fighter model weapon) game.player
                                |> \( player, task ) ->
                                    ( { game | player = player }
                                    , Task.perform Complete task
                                    )

                        Nothing ->
                            update NoOp game

                _ ->
                    ( { game | player = Player.selectModel game.player model.id }, Cmd.none )

        Click { x, y } ->
            case Player.getSelectedGangMember game.player of
                Just fighter ->
                    let
                        pos =
                            positionFromMouseCoords ( x, y ) game
                    in
                        case game.player.action of
                            Action.Move ->
                                Player.execute (Player.Moving fighter pos) game.player
                                    |> \( player, task ) ->
                                        ( { game | player = player }
                                        , Task.perform Complete task
                                        )

                            Action.Run ->
                                Player.execute (Player.Running fighter pos) game.player
                                    |> \( player, task ) ->
                                        ( { game | player = player }
                                        , Task.perform Complete task
                                        )

                            _ ->
                                if Tabletop.isWithinDistance 2 fighter.position pos then
                                    ( game, Cmd.none )
                                else
                                    ( { game
                                        | player = Player.deselectAll game.player
                                        , contextMessage = Nothing
                                      }
                                    , Cmd.none
                                    )

                Nothing ->
                    update NoOp game

        Command action ->
            let
                message : Maybe ContextMessage
                message =
                    case action of
                        Action.Move ->
                            Just ( "lightblue", "Click to move your dude" )

                        Action.Run ->
                            Just ( "lightblue", "Click to run to a point" )

                        Action.Shoot weapon ->
                            let
                                closest =
                                    Player.getSelectedGangMember game.player
                                        |> Maybe.map (Model.withinShootingRange (Gang.toList game.player.gang) weapon)
                                        |> Maybe.withDefault []
                                        |> List.map (\{ id } -> Debug.log "Closest" id)
                            in
                                Just ( "red", "Select a target" )

                        _ ->
                            Nothing
            in
                ( { game
                    | player = game.player |> \p -> { p | action = action }
                    , contextMessage = message
                  }
                , Cmd.none
                )

        Complete action ->
            let
                updateGame : GameState
                updateGame =
                    case Turn.phase game.turn of
                        Movement ->
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
                ( { updateGame
                    | player = Player.await updateGame.player
                    , contextMessage = Nothing
                  }
                , Cmd.none
                )

        Advance ->
            ( { game
                | turn = Turn.advance game.turn
                , player = Player.deselectAll game.player
              }
            , Cmd.none
            )

        Hover { x, y } ->
            let
                pos =
                    positionFromMouseCoords ( x, y ) game

                pivot : Gang
                pivot =
                    case Turn.phase game.turn of
                        Movement ->
                            Player.updateSelectedGangMember game.player
                                (\model -> { model | bearing = Tabletop.angle model.position pos })

                        _ ->
                            game.player.gang
            in
                game.player
                    |> (\player -> { player | movementIntention = pos, gang = pivot })
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

                    109 ->
                        case Player.getSelectedGangMember game.player of
                            Just _ ->
                                update (Command Action.Move) game

                            Nothing ->
                                update NoOp game

                    114 ->
                        case Player.getSelectedGangMember game.player of
                            Just _ ->
                                update (Command Action.Run) game

                            Nothing ->
                                update NoOp game

                    _ ->
                        update NoOp game

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


positionFromMouseCoords : ( Int, Int ) -> GameState -> Tabletop.Position
positionFromMouseCoords ( x, y ) { windowScale, offset } =
    let
        transform : Int -> Float
        transform a =
            toFloat a / windowScale

        scaleX =
            transform x

        scaleY =
            transform y - toFloat (offset // 2)
    in
        ( scaleX, scaleY )



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

        letterbox =
            game.offset // 2

        definitions =
            defs []
                [ Svg.clipPath [ id "clip-off-table" ]
                    [ rect
                        [ width (game.tabletop.width |> toString)
                        , height (game.tabletop.height |> toString)
                        ]
                        []
                    ]
                , Tabletop.viewMarker
                ]

        contextMessage =
            let
                currentTurn =
                    [ Turn.round game.turn |> toString, Turn.phase game.turn |> toString ]
                        |> String.join " - "
            in
                Maybe.withDefault ( "white", currentTurn ) game.contextMessage
                    |> \( color, msg ) ->
                        text_
                            [ y "-2.5"
                            , x "50%"
                            , fill color
                            , fontSize "1.4"
                            , fontFamily "monospace"
                            , textAnchor "middle"
                            ]
                            (textNode msg)

        phaseAdvancement =
            let
                msg =
                    case game.contextMessage of
                        Just _ ->
                            textNode ""

                        Nothing ->
                            textNode "→"
            in
                text_
                    [ y "-2.5"
                    , x "65%"
                    , fill "red"
                    , fontSize "2"
                    , fontFamily "monospace"
                    , textAnchor "middle"
                    , onClick Advance
                    , Svg.Attributes.cursor "pointer"
                    ]
                    msg

        selectedFighterProfile =
            Player.getSelectedGangMember game.player
                |> Maybe.map Model.viewProfile
                |> Maybe.withDefault (Html.table [] [])
                |> htmlAsSvg "selected-fighter-profile"
                    [ x (game.tabletop.width // 2 |> toString)
                    , y (game.tabletop.height |> toString)
                    , width "50%"
                    , height (letterbox |> toString)
                    , fontFamily "monospace"
                    , fontSize "1"
                    , Svg.Attributes.style "color: mediumslateblue; text-align: right;"
                    ]

        controlArea : Tabletop.Position
        controlArea =
            ( toFloat letterbox
            , game.tabletop.height + (letterbox // 2) |> toFloat
            )

        controls =
            Player.getSelectedGangMember game.player
                |> Maybe.map (\fighter -> Action.viewControls (Turn.phase game.turn) fighter Command)
                |> Maybe.map (List.repeat 1)
                |> Maybe.withDefault []
                |> g
                    [ Tabletop.transformTranslate controlArea
                    , class "controls"
                    ]

        gameplay =
            g
                [ onClickWithCoords Click
                , Svg.Attributes.clipPath "url(#clip-off-table)"
                ]
                [ Tabletop.view game.tabletop
                , actionSelection
                , Gang.view game.player.gang Select
                ]
    in
        Html.div []
            [ svg
                [ viewBox
                    ([ 0, negate letterbox, game.tabletop.width, game.tabletop.height + game.offset ]
                        |> List.map toString
                        |> join " "
                    )
                , width (game.windowWidth |> toString)
                , Svg.Attributes.style "background-color: black"
                ]
                [ definitions
                , contextMessage
                , phaseAdvancement
                , selectedFighterProfile
                , controls
                , gameplay
                ]
            ]
