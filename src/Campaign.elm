module Campaign exposing (..)

import Game exposing (Game)
import Html exposing (Html)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Campaign Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Campaign =
    { game : Game
    }


init : ( Campaign, Cmd Msg )
init =
    { game = Game.init } ! [ Random.generate Begin Game.generator ]



-- UPDATE


type Msg
    = Begin Game
    | NoOp


update : Msg -> Campaign -> ( Campaign, Cmd Msg )
update msg campaign =
    let
        noop =
            ( campaign, Cmd.none )
    in
        case msg of
            Begin newGame ->
                ( { campaign | game = newGame }
                , Cmd.none
                )

            NoOp ->
                noop



-- SUBSCRIPTIONS


subscriptions : Campaign -> Sub Msg
subscriptions campaign =
    Sub.none



-- VIEW


view : Campaign -> Svg Msg
view campaign =
    let
        game =
            campaign.game
    in
        svg
            [ viewBox
                ([ 0, 0, game.tabletop.width, game.tabletop.height ]
                    |> List.map toString
                    |> String.join " "
                )
            ]
        <|
            Game.view game (always NoOp)
