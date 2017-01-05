module View.Controls exposing (..)

import Action exposing (Action)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Player exposing (Player)
import Task exposing (Task)
import Turn exposing (Phase)
import Utilities exposing (onClickWithoutPropagation)


type ActionKey
    = Q
    | W
    | E
    | R


keysForActions : List Action -> List ( ActionKey, Maybe Action )
keysForActions actions =
    [ Q, W, E, R ]
        |> List.foldl
            (\key ( results, acts ) ->
                ( ( key, List.head acts ) :: results, (List.drop 1 acts) )
            )
            ( [], actions )
        |> Tuple.first
        |> List.reverse


availableActions : Player -> Phase -> List ( ActionKey, Maybe Action )
availableActions player phase =
    Player.getSelectedGangMember player
        |> Maybe.map (Action.select phase)
        |> Maybe.withDefault ([])
        |> keysForActions


takeAction : List ( ActionKey, Maybe Action ) -> ActionKey -> Task ActionKey Action
takeAction actions key =
    let
        actionDict =
            List.map (Tuple.mapFirst (toString)) actions
                |> Dict.fromList
    in
        Dict.get (toString key) actionDict
            |> Maybe.andThen (identity)
            |> Maybe.map (Task.succeed)
            |> Maybe.withDefault (Task.fail key)


viewControl : String -> Maybe Action -> (Action -> msg) -> Html msg
viewControl key maybeAction msg =
    case maybeAction of
        Just action ->
            button
                [ class "command"
                , disabled False
                , title (toString action)
                , onClickWithoutPropagation (msg action)
                ]
                [ span [ class "command-symbol" ] [ text <| Action.symbol action ]
                , span [ class "command-hotkey" ] [ text key ]
                ]

        Nothing ->
            button
                [ class "command"
                , disabled True
                ]
                [ span [ class "command-symbol" ] [ text "X" ]
                , span [ class "command-hotkey" ] [ text key ]
                ]


viewDice : Int -> Int -> msg -> Html msg
viewDice num val msg =
    let
        diceStr =
            (toString num) ++ "D" ++ (toString val)
    in
        Html.div [ id "player-commands" ]
            [ button
                [ class "command diceroll"
                , title diceStr
                , onClickWithoutPropagation msg
                ]
                [ span [ class "command-symbol" ] [ text "🎲" ]
                , span [ class "command-hotkey" ] [ text diceStr ]
                ]
            ]


view : Player -> Phase -> (Action -> msg) -> Html msg
view player phase msg =
    availableActions player phase
        |> List.map (\( key, action ) -> viewControl (toString key) action msg)
        |> Html.div [ id "player-commands" ]
