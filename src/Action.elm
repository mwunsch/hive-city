module Action exposing (..)

{-| The `Action` module describes the Action a Player is allowed to
take during a Phase and is responsible for drawing the user controls
for those actions.

-}

import List
import Model exposing (Model)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Inch, posX, posY, transformTranslate)
import Turn exposing (Turn, Phase(..))
import Utilities exposing (onClickWithoutPropagation, textNode)


type Action
    = Await
    | Cancel
    | Move
    | Charge
    | Run
    | Hide
    | Shoot
    | Fight


select : Phase -> List Action
select phase =
    case phase of
        Movement ->
            -- [ Charge, Run, Hide, Cancel,
            [ Move ]

        Shooting ->
            -- [ Cancel,
            [ Shoot ]

        HandToHand ->
            [ Cancel ]

        Recovery ->
            [ Cancel ]


canModelTakeAction : Model -> Action -> Bool
canModelTakeAction model action =
    case action of
        Move ->
            model.remainingMove > 0

        _ ->
            True


symbol : Action -> String
symbol action =
    case action of
        Move ->
            "M"

        Run ->
            "R"

        Shoot ->
            "S"

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


viewControls : Phase -> Model -> (Action -> msg) -> List (Svg msg)
viewControls phase fighter message =
    select phase
        |> List.map
            (\action ->
                viewControl action (canModelTakeAction fighter action) (message action)
            )


viewControl : Action -> Bool -> msg -> Svg msg
viewControl action isEnabled message =
    let
        cursor =
            if isEnabled then
                "pointer"
            else
                "no-drop"

        color =
            if isEnabled then
                "white"
            else
                "grey"
    in
        g
            [ onClickWithoutPropagation message
            , Svg.Attributes.cursor cursor
            , class "control"
            ]
            [ text'
                [ fontSize "2"
                , fontFamily "monospace"
                , fill color
                , alignmentBaseline "middle"
                , textAnchor "start"
                ]
                (symbol action |> textNode)
            ]


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
