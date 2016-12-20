module Campaign exposing (..)

import Game exposing (Game)
import Html exposing (Html)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes
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
    , window = { width = 1334, height = 750 }
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

        top =
            svg
                [ x "0"
                , y "0"
                , height "10%"
                ]
                [ rect [ x "0", y "0", height "100%", width "100%", fill "green" ] [] ]

        bottom =
            svg
                [ x "0"
                , y "90%"
                , height "10%"
                ]
                [ rect [ height "100%", width "100%", fill "purple" ] [] ]

        gameplay =
            Game.view game (always NoOp)
                |> svg
                    [ viewBox
                        ([ 0, 0, game.tabletop.width, game.tabletop.height ]
                            |> List.map toString
                            |> String.join " "
                        )
                    , height "80%"
                    , y "10%"
                    ]
    in
        [ top, gameplay, bottom ]
            |> svg
                [ width (campaign.window.width |> toString)
                , height (campaign.window.height |> toString)
                , x "0"
                , y "0"
                ]
            |> List.repeat 1
            |> Html.div
                [ Html.Attributes.style [ ( "background-color", "black" ) ] ]
