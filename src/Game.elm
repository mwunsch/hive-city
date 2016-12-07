module Game exposing (..)

import Player exposing (Player)
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
            , Player.init table
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
