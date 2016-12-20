module Game exposing (..)

import Gang
import Model exposing (Model)
import Player exposing (Player)
import Random exposing (Generator)
import Svg exposing (Svg, g)
import Tabletop exposing (Tabletop)
import Turn exposing (Turn)


type alias Game =
    { players : ( Player, Player )
    , tabletop : Tabletop
    , turn : ( Turn, Player )
    }


init : Game
init =
    let
        table =
            Tabletop.by 6 4

        players =
            ( Player.init table
            , Player.init table |> \player -> { player | color = "red" }
            )
    in
        { players = players
        , tabletop = table
        , turn = ( Turn.init, Tuple.first players )
        }


player1 : Game -> Player
player1 =
    .players >> Tuple.first


player2 : Game -> Player
player2 =
    .players >> Tuple.second


generator : Generator Game
generator =
    let
        game =
            init
    in
        game.players
            |> Tuple.mapFirst (\player -> Gang.positionedGenerator game.tabletop |> Random.map (\gang -> { player | gang = gang }))
            |> Tuple.mapSecond (\player -> Gang.positionedGenerator game.tabletop |> Random.map (\gang -> { player | gang = gang }))
            |> uncurry (Random.map2 (\player1 player2 -> { game | players = ( player1, player2 ) }))


view : Game -> (Model -> msg) -> List (Svg msg)
view game msg =
    [ Tabletop.view game.tabletop
    , Player.gangView (player1 game) msg
    , Player.gangView (player2 game) msg
    ]
