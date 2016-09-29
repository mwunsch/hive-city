module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Html.Events exposing (on, onClick)
import Json.Decode as Json exposing ((:=))
import List exposing (map)
import Maybe exposing (andThen)
import Model exposing (Model)
import Mouse
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (posX, posY, Tabletop)
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
    , windowWidth : Int
    , windowScale : Float
    }


init : ( GameState, Cmd Msg )
init =
    ( { fighter =
            Model.averageFighter ( 50, 25 )
      , playerSelection = Nothing
      , windowWidth = 1000
      , windowScale = 10
      }
    , Task.perform (\_ -> NoOp) Resize Window.width
    )



-- UPDATE


type Msg
    = Select Model
    | Click Mouse.Position
    | Resize Int
    | NoOp


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        Select fighter ->
            ( { game | playerSelection = Just fighter }, Cmd.none )

        Click { x, y } ->
            let
                scale =
                    game.windowScale

                moveFighter =
                    case game.playerSelection of
                        Just fighter ->
                            Model.move fighter
                                ( round <| (toFloat x) / scale
                                , round <| (toFloat y) / scale
                                )

                        Nothing ->
                            game.fighter
            in
                ( { game
                    | fighter = moveFighter
                    , playerSelection = game.playerSelection `andThen` \_ -> Just moveFighter
                  }
                , Cmd.none
                )

        Resize w ->
            ( { game
                | windowWidth = w
                , windowScale = (toFloat w) / 100
              }
            , Cmd.none
            )

        NoOp ->
            ( game, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GameState -> Sub Msg
subscriptions game =
    Window.resizes (\size -> Resize size.width)



-- VIEW


onClickWithCoords : (Mouse.Position -> msg) -> Html.Attribute msg
onClickWithCoords message =
    on "click" <|
        Json.map message <|
            Json.object2 Mouse.Position
                ("clientX" := Json.int)
                ("clientY" := Json.int)


view : GameState -> Html Msg
view game =
    let
        fighter =
            text'
                [ fontSize "1"
                , fontFamily "monospace"
                , textAnchor "middle"
                , fill color
                , x
                    (game.fighter.position |> posX |> toString)
                , y
                    (game.fighter.position |> posY |> toString)
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
            [ viewBox
                ([ 0, 0, tabletop.width, tabletop.height ] |> map toString |> join " ")
            , width
                (game.windowWidth |> toString)
            , onClickWithCoords Click
            ]
            (Tabletop.view tabletop [] :: movementArea)
