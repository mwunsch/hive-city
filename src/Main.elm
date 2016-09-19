module Main exposing (..)

import Model exposing (Model)
import Tabletop exposing (posX, posY, Tabletop)
import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
            Model.averageFighter ( 50, 50 )
      , playerSelection = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select Model


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg game =
    case msg of
        Select fighter ->
            ( { game | playerSelection = Just fighter }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GameState -> Sub Msg
subscriptions game =
    Sub.none



-- VIEW


view : GameState -> Html Msg
view game =
    let
        fighter =
            "@"

        color =
            case game.playerSelection of
                Nothing ->
                    "black"

                Just x ->
                    "red"

        tabletop = Tabletop 100 50
    in
        svg [ viewBox "0 0 100 100", width "100%" ]
            [ Tabletop.view tabletop [] ]
