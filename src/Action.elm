module Action exposing (..)

{-| The `Action` module describes the Action a Player is allowed to
take during a Phase and is responsible for drawing the user controls
for those actions.

-}

import List
import Model exposing (Model)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Inch, posX, posY, transformTranslate)
import Turn exposing (Turn, Phase(..))
import Utilities exposing (onClickWithoutPropagation, textNode)
import Weapons exposing (Weapon, autogun)


type Action
    = Await
    | Cancel
    | Move
    | Charge
    | Run
    | Hide
    | Shoot Weapon
    | Fight


type Failure
    = FailedToMove
    | MissedShot Model Model Weapon


select : Phase -> Model -> List Action
select phase fighter =
    case phase of
        Movement ->
            [ Move, Run ]

        Shooting ->
            fighter.equipment
                |> List.filter Weapons.isRanged
                |> List.map Shoot

        HandToHand ->
            [ Cancel ]

        Recovery ->
            [ Cancel ]


canModelTakeAction : Model -> Action -> Bool
canModelTakeAction model action =
    case action of
        Move ->
            model.remainingMove > 0

        Run ->
            Model.canRun model

        _ ->
            True


symbol : Action -> String
symbol action =
    case action of
        Move ->
            "M"

        Run ->
            "R"

        Shoot weapon ->
            (Weapons.profile weapon) |> .icon

        _ ->
            "?"


viewSelection : Model -> Svg msg
viewSelection { position, remainingMove } =
    circle
        [ r (remainingMove |> toString)
        , fill "white"
        , opacity "0.15"
        , cx (position |> posX |> toString)
        , cy (position |> posY |> toString)
        ]
        []


viewTarget : Model -> Svg msg
viewTarget { position } =
    circle
        [ r "1"
        , fill "red"
        , opacity "0.15"
        , cx (position |> posX |> toString)
        , cy (position |> posY |> toString)
        ]
        []


viewControls : Phase -> Model -> (Action -> msg) -> Svg msg
viewControls phase fighter message =
    select phase fighter
        |> List.map
            (\action ->
                viewControl action (canModelTakeAction fighter action) (message action)
            )
        |> text_
            [ fontSize "2"
            , fontFamily "monospace"
            , alignmentBaseline "middle"
            , textAnchor "start"
            ]


viewControl : Action -> Bool -> msg -> Svg msg
viewControl action canAct message =
    let
        cursor =
            if canAct then
                "pointer"
            else
                "no-drop"

        color =
            if canAct then
                "white"
            else
                "grey"
    in
        tspan
            [ onClickWithoutPropagation message
            , Svg.Attributes.cursor cursor
            , class "control"
            , fill color
            , dx "1.5"
            ]
            (symbol action |> textNode)


emptyView : Svg msg
emptyView =
    g [] []


unimplementedView : Action -> Model -> Svg msg
unimplementedView action model =
    g [ transformTranslate model.position ]
        [ circle
            [ r (Tabletop.millimeter 20 |> toString)
            , fill "red"
            , opacity "0.15"
            ]
            []
        ]
