module Game exposing (..)

import Player exposing (Player)
import Tabletop exposing (Tabletop)


type alias Game =
    { players : ( Player, Player )
    , tabletop : Tabletop
    }


player1 : Game -> Player
player1 =
    .players >> Tuple.first


player2 : Game -> Player
player2 =
    .players >> Tuple.second
