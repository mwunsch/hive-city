module Action exposing (..)

import List
import Model exposing (Model)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Inch, posX, posY, transformTranslate)
import Turn exposing (Turn, Phase(..))


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
            [ Move, Charge, Run, Hide, Cancel ]

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


view : Action -> Phase -> Model -> Svg msg
view action phase model =
    case action of
        Await ->
            viewSelection phase model

        _ ->
            g [ transformTranslate model.position ]
                [ circle
                    [ r (Tabletop.millimeter 25 |> toString)
                    , fill "red"
                    , opacity "0.15"
                    ]
                    []
                ]


{-| TODO: Just drawing a circle for now until can come up with better HUD.
-}
viewSelection : Phase -> Model -> Svg msg
viewSelection phase { position } =
    g [ transformTranslate position ]
        [ circle
            [ r (Tabletop.millimeter 35 |> toString)
            , fill "white"
            , opacity "0.15"
            ]
            []
        ]


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
