module Main exposing (..)

import Model exposing (Model)
import Tabletop exposing (posX, posY, Tabletop)
import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse
import Window
import Task
import String exposing (join)
import List exposing (map)


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
    , windowWidth : Int
    }


init : ( GameState, Cmd Msg )
init =
    ( { fighter =
            Model.averageFighter ( 50, 25 )
      , playerSelection = Nothing
      , windowWidth = 1000
      }
    , Task.perform (\_ -> NoOp) Resize Window.width
    )



-- UPDATE


type Msg
    = Select Model
    | Deselect
    | Click Mouse.Position
    | Resize Int
    | NoOp


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        Select fighter ->
            ( { game | playerSelection = Just fighter }, Cmd.none )

        Deselect ->
            ( { game | playerSelection = Nothing }, Cmd.none )

        Click position ->
            let
                moveFighter =
                    case game.playerSelection of
                        Just fighter ->
                            Model.move fighter ( position.x, position.y )

                        Nothing ->
                            game.fighter
            in
                ( { game | fighter = moveFighter }, Cmd.none )

        Resize w ->
            ( { game | windowWidth = w }, Cmd.none )

        NoOp ->
            ( game, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GameState -> Sub Msg
subscriptions game =
    Window.resizes (\size -> Resize size.width)



-- VIEW


view : GameState -> Html Msg
view game =
    let
        fighter =
            text'
                [ fontSize "1"
                , fontFamily "monospace"
                , textAnchor "middle"
                , fill color
                , game.fighter.position |> posX |> toString |> x
                , game.fighter.position |> posY |> toString |> y
                , onClick <| Select game.fighter
                , Svg.Attributes.cursor "pointer"
                ]
                [ text "@" ]

        color =
            case game.playerSelection of
                Nothing ->
                    "black"

                Just x ->
                    "white"

        movementArea =
            case game.playerSelection of
                Nothing ->
                    [ fighter ]

                Just x ->
                    [ fighter
                    , circle
                        [ cx (x.position |> posX |> toString)
                        , cy (x.position |> posY |> toString)
                        , r (x.profile.move |> toString)
                        , fill "white"
                        , fillOpacity "0.25"
                        ]
                        []
                    ]

        tabletop =
            Tabletop 100 50
    in
        svg
            [ [ 0, 0, tabletop.width, tabletop.height ] |> map toString |> join " " |> viewBox
            , width (game.windowWidth |> toString)
            , onClick
                (case game.playerSelection of
                    Just x ->
                        Deselect

                    Nothing ->
                        NoOp
                )
            ]
            (Tabletop.view tabletop [] :: movementArea)
