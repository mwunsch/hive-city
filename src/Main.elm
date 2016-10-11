module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Events exposing (on, onClick)
import Json.Decode as Json exposing ((:=))
import Keyboard
import List exposing (map)
import Maybe exposing (andThen)
import Model exposing (Model)
import Mouse
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (posX, posY, Tabletop, positionFromMouseCoords)
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
    { fighter : Model
    , playerSelection : Maybe Model
    , tabletop : Tabletop
    , windowWidth : Int
    , windowScale : Float
    , movementIntention : Tabletop.Position
    }


init : ( GameState, Cmd Msg )
init =
    ( { fighter =
            Model.averageFighter ( 50, 25 )
      , playerSelection = Nothing
      , tabletop = Tabletop 100 50
      , windowWidth = 1000
      , windowScale = 10
      , movementIntention = ( 50, 25 )
      }
    , Task.perform (\_ -> NoOp) Resize Window.width
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
            let
                update =
                    { model | selected = True }
            in
                ( { game
                    | fighter = update
                    , playerSelection = Just update
                  }
                , Cmd.none
                )

        Click { x, y } ->
            let
                moveFighter : Result Model.MovementError Model
                moveFighter =
                    case game.playerSelection of
                        Just fighter ->
                            Model.attemptMove fighter <| positionFromMouseCoords ( x, y ) game.windowScale

                        Nothing ->
                            Ok game.fighter

                updateGame =
                    case moveFighter of
                        Ok m ->
                            { game
                                | fighter = m
                                , playerSelection = game.playerSelection `andThen` \_ -> Just m
                            }

                        Err ( s, m ) ->
                            { game
                                | fighter = m
                                , playerSelection = Nothing
                            }
            in
                ( updateGame, Cmd.none )

        Hover { x, y } ->
            case game.playerSelection of
                Just fighter ->
                    ( { game
                        | movementIntention = positionFromMouseCoords ( x, y ) game.windowScale
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( game, Cmd.none )

        KeyPress key ->
            case key of
                -- ESCAPE
                27 ->
                    ( { game | playerSelection = Nothing }, Cmd.none )

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
        , case game.playerSelection of
            Just _ ->
                Mouse.moves Hover

            Nothing ->
                Sub.none
        , case game.playerSelection of
            Just _ ->
                Keyboard.presses KeyPress

            Nothing ->
                Sub.none
        ]



-- VIEW


onClickWithCoords : (Mouse.Position -> msg) -> Html.Attribute msg
onClickWithCoords message =
    on "click" <|
        Json.map message Mouse.position


view : GameState -> Html Msg
view game =
    let
        fighter =
            Model.view game.fighter <| Select game.fighter

        movementArea =
            case game.playerSelection of
                Nothing ->
                    [ fighter ]

                Just x ->
                    [ fighter
                    , Model.movementView x game.movementIntention
                    ]
    in
        svg
            [ viewBox
                ([ 0, 0, game.tabletop.width, game.tabletop.height ] |> map toString |> join " ")
            , width
                (game.windowWidth |> toString)
            , onClickWithCoords Click
            ]
            (Tabletop.view game.tabletop [] :: movementArea)
