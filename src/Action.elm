module Action exposing (..)

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


select : Phase -> Model -> List Action
select phase fighter =
    case phase of
        Movement ->
            -- [ Charge, Run, Hide, Cancel,
            [ Move ]

        Shooting ->
            [ Shoot, Cancel ]

        HandToHand ->
            [ Cancel ]

        Recovery ->
            [ Cancel ]


symbol : Action -> String
symbol action =
    case action of
        Move ->
            String.fromChar '🚶'

        Run ->
            String.fromChar '🏃'

        _ ->
            String.fromChar '🔜'


view : Action -> Phase -> Model -> (Action -> msg) -> Svg msg
view action phase model msg =
    case action of
        _ ->
            g [ transformTranslate model.position ]
                [ circle
                    [ r (Tabletop.millimeter 20 |> toString)
                    , fill "red"
                    , opacity "0.15"
                    ]
                    []
                ]


viewControl : Action -> msg -> Svg msg
viewControl action msg =
    g
        [ transform "translate(0,1.15)"
        , onClickWithoutPropagation msg
        , Svg.Attributes.cursor "pointer"
        ]
        [ circle
            [ r (Tabletop.millimeter 12 |> toString)
            , fill "white"
            , opacity "0.75"
            ]
            []
        , text'
            [ fontSize (Tabletop.millimeter 15 |> toString)
            , textAnchor "middle"
            , alignmentBaseline "middle"
            ]
            (symbol action |> textNode)
        ]


{-| TODO: Just drawing a circle for now until can come up with better HUD.
-}
viewSelection : Phase -> Model -> (Action -> msg) -> Svg msg
viewSelection phase model msg =
    g [ transformTranslate model.position ] <|
        circle
            [ r "1.25"
            , fill "white"
            , opacity "0.15"
            ]
            []
            :: List.map (\action -> viewControl action (msg action)) (select phase model)


emptyView : Svg msg
emptyView =
    g [] []


{-| TODO: Make the below `view` fn better.
The actions should form a circle around the fighter character
-}
viewAll : Phase -> Model -> Svg msg
viewAll phase fighter =
    select phase fighter
        |> List.map symbol
        |> (String.join " " >> text >> List.repeat 1)
        |> text'
            [ fontSize (Tabletop.millimeter 20 |> toString)
            , x (fighter.position |> posX |> (flip (-)) (Tabletop.millimeter 20) |> toString)
            , y (fighter.position |> posY |> (+) (Tabletop.millimeter 40) |> toString)
            ]
