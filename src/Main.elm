module Main exposing (..)

import Dict
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
    { player : Player
    , tabletop : Tabletop
    , windowWidth : Int
    , windowScale : Float
    }


init : ( GameState, Cmd Msg )
init =
    ( { player = Player.init
      , tabletop = Tabletop 100 50
      , windowWidth = 1000
      , windowScale = 10
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
                p =
                    Player.selectModel game.player model.id
            in
                ( { game | player = p }, Cmd.none )

        Click { x, y } ->
            case game.player.selection of
                Just id ->
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
                                Player.updateGang game.player id (\m -> m `andThen` attemptMove)
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
                    let
                        p =
                            Player.deselectAll game.player
                    in
                        ( { game | player = p }, Cmd.none )

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
        , case game.player.selection of
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
        fightersView =
            Dict.values game.player.gang
                |> map (\f -> Model.view f <| Select f)

        movementArea =
            case game.player.selection of
                Just id ->
                    Player.getGangMember game.player id
                        |> Maybe.map (\f -> Model.movementView f game.player.movementIntention :: [])
                        |> Maybe.withDefault []

                Nothing ->
                    []
    in
        svg
            [ viewBox
                ([ 0, 0, game.tabletop.width, game.tabletop.height ] |> map toString |> join " ")
            , width
                (game.windowWidth |> toString)
            , onClickWithCoords Click
            ]
            (Tabletop.view game.tabletop []
                :: movementArea
                ++ fightersView
            )
