module Main exposing (..)

import Action exposing (Action, Failure)
import Array
import Dice
import Game exposing (Game)
import Gang exposing (Gang)
import Html exposing (Html)
import Html.Events exposing (on, onClick)
import Json.Decode as Json
import Keyboard
import List exposing (map)
import Maybe exposing (andThen)
import Model exposing (Model)
import Mouse
import Player exposing (Player, Instruction(..), DiceRoll)
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
    , rolling : Maybe DiceRoll
    , game : Game
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
          , rolling = Nothing
          , game = Game.init
          }
        , Cmd.batch
            [ Task.perform Resize Window.width
            , Random.generate Generate (Gang.positionedGenerator table)
            , Random.generate Begin (Game.generator)
            ]
        )



-- UPDATE


type Msg
    = Select Model
    | Click Mouse.Position
    | Command Action
    | Roll Instruction
    | Complete (Result Failure Action)
    | Advance
    | Hover Mouse.Position
    | KeyPress Keyboard.KeyCode
    | Resize Int
    | Generate Gang
    | NoOp
    | Log String String
    | Begin Game


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        Begin newGame ->
            { game | game = Game.update (Game.Begin newGame) game.game }
                |> (flip (,)) Cmd.none

        Select model ->
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
                                update (Roll <| Player.Moving fighter pos) game

                            Action.Run ->
                                update (Roll <| Player.Running fighter pos) game

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
            case action of
                Action.Move ->
                    ( { game
                        | contextMessage = Just ( "lightblue", "Click to move your dude" )
                        , player = game.player |> \p -> { p | action = action }
                      }
                    , Cmd.none
                    )

                Action.Run ->
                    ( { game
                        | contextMessage = Just ( "lightblue", "Click to run to a point" )
                        , player = game.player |> \p -> { p | action = action }
                      }
                    , Cmd.none
                    )

                Action.Shoot weapon ->
                    let
                        attackerAndTarget =
                            Player.getSelectedGangMember game.player
                                |> andThen
                                    (\shooter ->
                                        Player.getClosestModelInWeaponRange game.player weapon
                                            |> Maybe.map ((,) shooter)
                                    )
                    in
                        case attackerAndTarget of
                            Just ( attacker, target ) ->
                                ( { game
                                    | contextMessage = Just ( "red", "Target acquired" )
                                    , rolling = Just ( 1, 6, Player.Shooting attacker target weapon )
                                    , player = game.player |> \p -> { p | action = action }
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { game
                                    | contextMessage = Just ( "red", "No target in range!" )
                                    , player = game.player |> \p -> { p | action = action }
                                  }
                                , Cmd.none
                                )

                _ ->
                    ( { game
                        | contextMessage = Nothing
                        , player = game.player |> \p -> { p | action = action }
                      }
                    , Cmd.none
                    )

        Roll instruction ->
            Player.execute instruction game.player
                |> Tuple.mapFirst (\p -> { game | player = p, rolling = Nothing })
                |> Tuple.mapSecond (Cmd.map Complete)

        Complete actionResult ->
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
    let
        sublist =
            [ Window.resizes (\size -> Resize size.width)
            , Keyboard.presses KeyPress
            ]
    in
        Sub.batch <|
            case Turn.phase game.turn of
                Movement ->
                    Mouse.moves Hover :: sublist

                _ ->
                    sublist



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
            game.rolling
                |> Maybe.map (\( num, val, instruction ) -> Dice.viewRoll num val "" <| Roll instruction)
                |> Maybe.map (List.repeat 1)
                |> Maybe.withDefault
                    (Player.getSelectedGangMember game.player
                        |> Maybe.map (\fighter -> Action.viewControls (Turn.phase game.turn) fighter Command)
                        |> Maybe.map (List.repeat 1)
                        |> Maybe.withDefault []
                    )
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
                , Player.gangView game.player Select
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
