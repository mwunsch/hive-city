module Campaign exposing (..)

import Game exposing (Game)
import Html exposing (Html)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Window


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
    , window : Window.Size
    }


init : ( Campaign, Cmd Msg )
init =
    { game = Game.init
    , window = { width = 640, height = 480 }
    }
        ! [ Task.perform Resize Window.size
          , Random.generate Begin Game.generator
          ]



-- UPDATE


type Msg
    = Begin Game
    | Resize Window.Size
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

            Resize ({ width, height } as size) ->
                ( { campaign | window = size }
                , Cmd.none
                )

            NoOp ->
                noop



-- SUBSCRIPTIONS


subscriptions : Campaign -> Sub Msg
subscriptions campaign =
    Sub.batch [ Window.resizes Resize ]



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
            , width (campaign.window.width |> toString)
            , height (campaign.window.height |> toString)
            ]
        <|
            Game.view game (always NoOp)
