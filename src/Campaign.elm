module Campaign exposing (..)

import Game exposing (Game)
import Gang exposing (Gang)
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model)
import Mouse
import Player exposing (Player)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop
import Task
import Utilities exposing (textNode, onEventWithPosition)
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
    | Hover Mouse.Position
    | Resize Window.Size
    | NoOp


update : Msg -> Campaign -> ( Campaign, Cmd Msg )
update msg campaign =
    let
        noop =
            ( campaign, Cmd.none )

        activePlayer =
            Game.activePlayer campaign.game
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

            Hover ({ x, y } as mouse) ->
                let
                    pos : Tabletop.Position
                    pos =
                        tabletopPositionFromMousePosition mouse campaign

                    pivot : Gang
                    pivot =
                        Player.updateSelectedGangMember activePlayer
                            (\model -> { model | bearing = Tabletop.angle model.position pos })
                in
                    ( { campaign
                        | game =
                            campaign.game
                                |> Game.mapActivePlayer (\p -> { p | movementIntention = pos, gang = pivot })
                      }
                    , Cmd.none
                    )

            Resize ({ width, height } as size) ->
                ( { campaign | window = size }
                , Cmd.none
                )

            NoOp ->
                noop


tabletopPositionFromMousePosition : Mouse.Position -> Campaign -> Tabletop.Position
tabletopPositionFromMousePosition { x, y } { game, window } =
    let
        tableHeight =
            (toFloat window.height) * 0.8

        tableWidth =
            tableHeight * (Tabletop.aspectRatio game.tabletop)

        xOffset =
            ((toFloat window.width) - tableWidth) / 2

        yOffset =
            ((toFloat window.height) - tableHeight) / 2

        scale =
            tableWidth / (toFloat game.tabletop.width)
    in
        ( x, y )
            |> Tuple.mapFirst (\x -> ((toFloat x) - xOffset) / scale)
            |> Tuple.mapSecond (\y -> ((toFloat y) - yOffset) / scale)
            |> Debug.log "x,y"



-- SUBSCRIPTIONS


subscriptions : Campaign -> Sub Msg
subscriptions campaign =
    let
        activePlayer =
            Game.activePlayer campaign.game
    in
        Sub.batch
            [ Window.resizes Resize
            , case activePlayer.selection of
                Just _ ->
                    Mouse.moves Hover

                Nothing ->
                    Sub.none
            ]



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
  min-height: 10%;
  width: 100%;
  flex-grow: 2;
}

#controls {
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

        activePlayer =
            Game.activePlayer game

        top =
            Html.div
                [ id "messaging"
                , Html.Attributes.style [ ( "background-color", activePlayer.color ) ]
                ]
                []

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
