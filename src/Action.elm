module Action exposing (..)

import List
import Model exposing (Model)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Inch, posX, posY)
import Turn exposing (Turn, Phase(..))


type Action
    = Await
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
            [ Await, Move, Charge, Run, Hide ]

        Shooting ->
            [ Await, Shoot ]

        HandToHand ->
            [ Fight ]

        Recovery ->
            [ Await ]


symbol : Action -> String
symbol action =
    case action of
        Move ->
            String.fromChar '🚶'

        Run ->
            String.fromChar '🏃'

        _ ->
            String.fromChar '🔜'


{-| TODO: Just drawing a circle for now until can come up with better HUD.
-}
viewSelection : Phase -> Model -> Svg msg
viewSelection phase { position } =
    let
        translate : Tabletop.Position -> String
        translate ( x, y ) =
            String.concat
                [ "translate"
                , "("
                , (String.join "," [ (toString x), (toString y) ])
                , ")"
                ]
    in
        g [ transform (translate position) ]
            [ circle
                [ r (Tabletop.millimeter 35 |> toString)
                , fill "white"
                , opacity "0.15"
                ]
                []
            ]


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
