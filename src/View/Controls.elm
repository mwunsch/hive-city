module View.Controls exposing (..)

import Action exposing (Action)
import Html exposing (..)
import Html.Attributes exposing (..)
import Utilities exposing (onClickWithoutPropagation)


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


view : List Action -> (Action -> msg) -> Html msg
view actions msg =
    [ "q", "w", "e", "r" ]
        |> List.foldl
            (\key ( results, actions ) ->
                ( ( key, List.head actions ) :: results, (List.drop 1 actions) )
            )
            ( [], actions )
        |> Tuple.first
        |> List.reverse
        |> List.map (\( key, action ) -> viewControl key action msg)
        |> Html.div [ id "player-commands" ]
