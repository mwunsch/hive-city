module Main exposing (..)

import Model exposing (Model)
import Tabletop exposing (posX, posY, Tabletop)
import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Mouse


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
    }


init : ( GameState, Cmd Msg )
init =
    ( { fighter =
            Model.averageFighter ( 50, 25 )
      , playerSelection = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select Model
    | Deselect
    | Click Mouse.Position  -- SVG Coordinate transformation. Dang.
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

        NoOp ->
            ( game, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GameState -> Sub Msg
subscriptions game =
    Sub.none



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
                , x (game.fighter.position |> posX |> toString)
                , y (game.fighter.position |> posY |> toString)
                , onClick (Select game.fighter)
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
            [ viewBox "0 0 100 100"
            , onClick
                (case game.playerSelection of
                    Just x ->
                        Deselect

                    Nothing ->
                        NoOp
                )
            ]
            (Tabletop.view tabletop [] :: movementArea)
