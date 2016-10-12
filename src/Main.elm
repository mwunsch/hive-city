module Main exposing (..)

import Gang
import Html exposing (Html)
import Html.App as App
import Html.Events exposing (on, onClick)
import Json.Decode as Json exposing ((:=))
import Keyboard
import List exposing (map)
import Maybe exposing (andThen)
import Model exposing (Model)
import Mouse
import Player exposing (Player)
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Tabletop, positionFromMouseCoords)
import Task
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
    }


init : ( GameState, Cmd Msg )
init =
    let
        table =
            Tabletop 72 48
    in
        ( { player = Player.init table
          , tabletop = table
          , windowWidth = 1000
          , windowScale = 10
          }
        , Task.perform (always NoOp) Resize Window.width
        )



-- UPDATE


type Msg
    = Select Model
    | Click Mouse.Position
    | Hover Mouse.Position
    | KeyPress Keyboard.KeyCode
    | Resize Int
    | NoOp


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        Select model ->
            ( { game | player = Player.selectModel game.player model.id }, Cmd.none )

        Click { x, y } ->
            case Player.getSelectedGangMember game.player of
                Just fighter ->
                    let
                        attemptMove : Model -> Maybe Model
                        attemptMove model =
                            case
                                positionFromMouseCoords ( x, y ) game.windowScale
                                    |> Model.attemptMove model
                            of
                                Ok m ->
                                    Just m

                                Err ( s, m ) ->
                                    Just m
                    in
                        ( { game
                            | player =
                                Player.updateGangMember game.player fighter.id (\m -> m `andThen` attemptMove)
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( game, Cmd.none )

        Hover { x, y } ->
            let
                updatePlayer player =
                    { player | movementIntention = positionFromMouseCoords ( x, y ) game.windowScale }
            in
                ( { game | player = updatePlayer game.player }, Cmd.none )

        KeyPress key ->
            case key of
                -- ESCAPE
                27 ->
                    ( { game | player = Player.deselectAll game.player }, Cmd.none )

                _ ->
                    ( game, Cmd.none )

        Resize w ->
            ( { game
                | windowWidth = w
                , windowScale = (toFloat w) / (toFloat game.tabletop.width)
              }
            , Cmd.none
            )

        NoOp ->
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
        measuringTape =
            Player.getSelectedGangMember game.player
                |> Maybe.map (\fighter -> Tabletop.viewMeasuringTape fighter.position game.player.movementIntention fighter.remainingMove :: [])
                |> Maybe.withDefault []
    in
        svg
            [ viewBox
                ([ 0, 0, game.tabletop.width, game.tabletop.height ] |> map toString |> join " ")
            , width
                (game.windowWidth |> toString)
            , onClickWithCoords Click
            ]
            (Tabletop.view game.tabletop
                :: measuringTape
                ++ Gang.view game.player.gang Select
            )
