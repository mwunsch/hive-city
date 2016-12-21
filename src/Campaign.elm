module Campaign exposing (..)

import Game exposing (Game)
import Html exposing (Html)
import Model exposing (Model)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Utilities exposing (textNode)
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
    | Select Model
    | Resize Window.Size
    | NoOp


update : Msg -> Campaign -> ( Campaign, Cmd Msg )
update msg campaign =
    let
        noop =
            ( campaign, Cmd.none )

        playerOne =
            Game.player1 campaign.game
    in
        case msg of
            Begin newGame ->
                ( { campaign | game = newGame }
                , Cmd.none
                )

            Select model ->
                ( { campaign | game = Game.selectFriendlyModel campaign.game model }
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


css : Html msg
css =
    """
#canvas {
  display: flex;
  flex-direction: column;
  background-color: black;
  justify-content: center;
  align-items: center;
  height: 100vh;
}

#messaging {
  background-color: green;
  min-height: 10%;
  width: 100%;
  flex-grow: 2;
}

#controls {
  background-color: purple;
  min-height: 10%;
  width: 100%;
  flex-grow: 2;
}
    """
        |> textNode
        |> Html.node "style" [ type_ "text/css" ]


view : Campaign -> Html Msg
view campaign =
    let
        game =
            campaign.game

        top =
            Html.div [ id "messaging" ] []

        bottom =
            Html.div [ id "controls" ] []

        gameplay =
            Game.view game Select
                |> svg
                    [ width "100%"
                    , viewBox
                        ([ 0, 0, game.tabletop.width, game.tabletop.height ]
                            |> List.map toString
                            |> String.join " "
                        )
                    , id "gameplay"
                    ]
    in
        [ top, gameplay, bottom ]
            |> Html.div [ id "canvas" ]
            |> List.repeat 1
            |> (::) css
            |> Html.main_ []
