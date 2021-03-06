module Campaign exposing (..)

import Action exposing (Action, Failure)
import Animation
import Dice exposing (Dice, oneD6)
import Game exposing (Game)
import Gang exposing (Gang)
import Html exposing (Html)
import Html.Attributes
import Keyboard exposing (KeyCode)
import Model exposing (Model)
import Mouse
import Player exposing (Player, Instruction(..), DiceRoll)
import Projectile
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop
import Task
import Turn
import Utilities exposing (textNode, onEventWithPosition, onClickWithoutPropagation)
import View.Controls as Controls exposing (ActionKey)
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
    , rolling : Maybe DiceRoll
    }


init : ( Campaign, Cmd Msg )
init =
    { game = Game.init
    , window = { width = 1334, height = 750 }
    , rolling = Nothing
    }
        ! [ Task.perform Resize Window.size
          , Random.generate Begin Game.generator
          ]



-- UPDATE


type Msg
    = Begin Game
    | Select Model
    | Command Action
    | Roll Instruction
    | Complete (Result Failure Instruction)
    | Advance
    | Hover Mouse.Position
    | Click Mouse.Position
    | Transition Animation.Msg
    | Travel Animation.Msg
    | KeyPress KeyCode
    | Resize Window.Size
    | NoOp


update : Msg -> Campaign -> ( Campaign, Cmd Msg )
update msg campaign =
    let
        noop =
            ( campaign, Cmd.none )

        activePlayer =
            Game.activePlayer campaign.game

        enemyPlayer =
            Game.enemyPlayer campaign.game

        currentTurn =
            Game.turn campaign.game
    in
        case msg of
            Begin newGame ->
                ( { campaign | game = newGame }
                , Cmd.none
                )

            Select model ->
                ( { campaign
                    | game =
                        if Gang.member activePlayer.gang model then
                            Game.selectFriendlyModel campaign.game model
                        else
                            Game.mapActivePlayer (\p -> { p | target = Just model.id }) campaign.game
                  }
                , Cmd.none
                )

            Command action ->
                case action of
                    Action.Shoot weapon ->
                        let
                            maybeTarget : Maybe Model
                            maybeTarget =
                                Player.getSelectedGangMember activePlayer
                                    |> Maybe.andThen
                                        (Model.withinShootingRange (Gang.toList enemyPlayer.gang) weapon
                                            >> List.head
                                        )
                        in
                            ( { campaign
                                | game =
                                    campaign.game
                                        |> Game.mapActivePlayer
                                            (\player ->
                                                { player
                                                    | action = action
                                                    , target = Maybe.map (.id) maybeTarget
                                                }
                                            )
                                , rolling =
                                    Player.getSelectedGangMember activePlayer
                                        |> Maybe.map2 (\target attacker -> ( 1, 6, Player.Shooting attacker target weapon )) maybeTarget
                              }
                            , Cmd.none
                            )

                    _ ->
                        ( { campaign
                            | game = Game.mapActivePlayer (\p -> { p | action = action }) campaign.game
                          }
                        , Cmd.none
                        )

            Roll instruction ->
                let
                    dice : Dice
                    dice =
                        campaign.rolling
                            |> Maybe.map (\( num, val, _ ) -> num |> Dice.d val)
                            |> Maybe.withDefault (oneD6)
                in
                    Player.execute instruction dice activePlayer
                        |> Tuple.mapFirst
                            (\p ->
                                { campaign
                                    | rolling = Nothing
                                    , game = Game.mapActivePlayer (always p) campaign.game
                                }
                            )
                        |> Tuple.mapSecond (Cmd.map Complete)

            Complete result ->
                let
                    updateGame =
                        case result of
                            Ok (Shooting attacker target weapon) ->
                                campaign.game
                                    |> (\g ->
                                            { g | projectile = Projectile.travel attacker.position target.position g.projectile }
                                       )

                            _ ->
                                campaign.game
                in
                    ( { campaign | game = Game.mapActivePlayer (Player.await) updateGame }
                    , Cmd.none
                    )

            Advance ->
                ( { campaign | game = Game.advanceTurn campaign.game }
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

            Click ({ x, y } as mouse) ->
                let
                    pos : Tabletop.Position
                    pos =
                        tabletopPositionFromMousePosition mouse campaign
                in
                    case Player.getSelectedGangMember activePlayer of
                        Just fighter ->
                            case activePlayer.action of
                                Action.Move ->
                                    ( campaign
                                    , Task.perform Roll (Task.succeed <| Player.Moving fighter pos)
                                    )

                                Action.Run ->
                                    ( campaign
                                    , Task.perform Roll (Task.succeed <| Player.Running fighter pos)
                                    )

                                _ ->
                                    ( { campaign | game = Game.mapActivePlayer (Player.deselectAll) campaign.game }
                                    , Cmd.none
                                    )

                        Nothing ->
                            noop

            Transition animation ->
                let
                    moveModels : Player -> Player
                    moveModels player =
                        { player
                            | gang =
                                player.gang
                                    |> Gang.map (\_ model -> { model | transition = Animation.update animation model.transition })
                        }
                in
                    ( { campaign
                        | game =
                            campaign.game
                                |> Game.mapActivePlayer (moveModels)
                                |> Game.mapEnemyPlayer (moveModels)
                      }
                    , Cmd.none
                    )

            Travel animation ->
                ( { campaign
                    | game =
                        campaign.game
                            |> (\g ->
                                    { g
                                        | projectile =
                                            g.projectile
                                                |> (\p -> { p | style = Animation.update animation p.style })
                                    }
                               )
                  }
                , Cmd.none
                )

            KeyPress key ->
                let
                    actions =
                        Controls.availableActions activePlayer (Turn.phase currentTurn)

                    takeAction : Controls.ActionKey -> Cmd Msg
                    takeAction =
                        Controls.takeAction actions
                            >> Task.attempt (Result.map Command >> Result.withDefault NoOp)
                in
                    case key of
                        27 ->
                            -- ESCAPE
                            ( { campaign
                                | game =
                                    campaign.game
                                        |> Game.mapActivePlayer (Player.deselectAll)
                              }
                            , Cmd.none
                            )

                        101 ->
                            ( campaign
                            , takeAction Controls.E
                            )

                        113 ->
                            ( campaign
                            , takeAction Controls.Q
                            )

                        114 ->
                            ( campaign
                            , takeAction Controls.R
                            )

                        119 ->
                            ( campaign
                            , takeAction Controls.W
                            )

                        _ ->
                            noop

            Resize ({ width, height } as size) ->
                ( { campaign | window = size }
                , Cmd.none
                )

            NoOp ->
                noop


tabletopPositionFromMousePosition : Mouse.Position -> Campaign -> Tabletop.Position
tabletopPositionFromMousePosition { x, y } { game, window } =
    let
        gameHeight =
            if window.height < window.width then
                (toFloat window.height) * 0.8
            else
                (toFloat window.width) / (Tabletop.aspectRatio game.tabletop)

        gameWidth =
            gameHeight * (Tabletop.aspectRatio game.tabletop)

        xOffset =
            ((toFloat window.width) - gameWidth) / 2

        yOffset =
            ((toFloat window.height) - gameHeight) / 2

        scale =
            gameWidth / (toFloat game.tabletop.width)
    in
        ( x, y )
            |> Tuple.mapFirst (\x -> ((toFloat x) - xOffset) / scale)
            |> Tuple.mapSecond (\y -> ((toFloat y) - yOffset) / scale)



-- SUBSCRIPTIONS


subscriptions : Campaign -> Sub Msg
subscriptions campaign =
    let
        activePlayer =
            Game.activePlayer campaign.game

        enemyPlayer =
            Game.enemyPlayer campaign.game

        currentPhase =
            Game.turn campaign.game |> Turn.phase
    in
        Sub.batch
            [ Window.resizes Resize
            , Keyboard.presses KeyPress
            , Mouse.clicks Click
            , ((Gang.toList activePlayer.gang) ++ (Gang.toList enemyPlayer.gang))
                |> List.map (.transition)
                |> Animation.subscription Transition
            , Animation.subscription Travel [ campaign.game.projectile.style ]
            , case currentPhase of
                Turn.Movement ->
                    activePlayer.selection
                        |> Maybe.map (\_ -> Mouse.moves Hover)
                        |> Maybe.withDefault Sub.none

                _ ->
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
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
  font-family: monospace;
  color: #fff;
}

#controls {
  min-height: 10%;
  width: 100%;
  flex-grow: 2;
  font-family: monospace;
  display: flex;
  justify-content: space-between;
  align-items: stretch;
  color: #fff;
}

#controls > * {
  min-width: 33%;
}

#controls table {
  width: 80%;
  margin: 5%;
}

#player-commands {
  display: flex;
  justify-content: space-around;
  align-items: center;
}

.command {
  padding: 1em;
  line-height: 1;
  font-family: monospace;
  border-radius: 10%;
  cursor: pointer;
  text-align: center;
  display: block;
}

.command:disabled {
  cursor: not-allowed;
}

.command > span {
  display: block;
}

.command .command-symbol {
  font-size: 1.5em;
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

        enemyPlayer =
            Game.enemyPlayer game

        top =
            Html.div
                [ id "messaging"
                , Html.Attributes.style [ ( "background-color", activePlayer.color ) ]
                ]
                [ Game.turn game
                    |> (\turn ->
                            (Turn.round turn |> toString) ++ " - " ++ (Turn.phase turn |> toString)
                       )
                    |> textNode
                    |> Html.h2 []
                , Html.button
                    [ id "phase-advance"
                    , onClickWithoutPropagation Advance
                    ]
                    [ Html.text "Next Phase →" ]
                ]

        selectedFighterProfile =
            Html.div
                [ id "selected-profile"
                , Html.Attributes.style [ ( "background-color", activePlayer.color ) ]
                ]
                [ Player.getSelectedGangMember activePlayer
                    |> Maybe.map Model.viewProfile
                    |> Maybe.withDefault (Html.table [] [])
                ]

        targetFighterProfile =
            Html.div
                [ id "target-profile"
                , Html.Attributes.style [ ( "background-color", enemyPlayer.color ) ]
                ]
                [ activePlayer.target
                    |> Maybe.andThen (\id -> Gang.get id enemyPlayer.gang)
                    |> Maybe.map Model.viewProfile
                    |> Maybe.withDefault (Html.table [] [])
                ]

        bottom =
            Html.div [ id "controls" ]
                [ selectedFighterProfile
                , campaign.rolling
                    |> Maybe.map (\( num, val, instruction ) -> Controls.viewDice num val <| Roll instruction)
                    |> Maybe.withDefault (Controls.view activePlayer (Game.turn game |> Turn.phase) Command)
                , targetFighterProfile
                ]

        definitions =
            defs []
                [ Svg.clipPath [ id "clip-off-table" ]
                    [ rect
                        [ width (game.tabletop.width |> toString)
                        , height (game.tabletop.height |> toString)
                        ]
                        []
                    ]
                , Tabletop.viewMarker
                ]

        gameplay =
            Game.view game Select
                |> (::) definitions
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
