module Game exposing (..)

import Gang
import Player exposing (Player)
import Random exposing (Generator)
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


type Msg
    = Begin Game


update : Msg -> Game -> Game
update msg game =
    case msg of
        Begin newGame ->
            newGame
