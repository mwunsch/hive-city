module Game exposing (..)

import Gang
import Model exposing (Model)
import Player exposing (Player)
import Projectile exposing (Projectile)
import Random exposing (Generator)
import Svg exposing (Svg)
import Tabletop exposing (Tabletop)
import Turn exposing (Turn)


type alias Game =
    { players : ( Player, Player )
    , tabletop : Tabletop
    , turn : ( Turn, ActivePlayer )
    , projectile : Projectile
    }


type ActivePlayer
    = PlayerOne
    | PlayerTwo


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
        , turn = ( Turn.init, PlayerOne )
        , projectile = Projectile.init
        }


turn : Game -> Turn
turn =
    .turn >> Tuple.first


advanceTurn : Game -> Game
advanceTurn game =
    let
        currentPlayer =
            game.turn |> Tuple.second

        nextPhase =
            turn game |> Turn.advance

        nextPhasePlayer =
            case Turn.phase nextPhase of
                Turn.Movement ->
                    case currentPlayer of
                        PlayerOne ->
                            PlayerTwo

                        PlayerTwo ->
                            PlayerOne

                _ ->
                    currentPlayer
    in
        { game | turn = ( nextPhase, nextPhasePlayer ) }


activePlayer : Game -> Player
activePlayer game =
    case Tuple.second game.turn of
        PlayerOne ->
            Tuple.first game.players

        PlayerTwo ->
            Tuple.second game.players


enemyPlayer : Game -> Player
enemyPlayer game =
    case Tuple.second game.turn of
        PlayerOne ->
            Tuple.second game.players

        PlayerTwo ->
            Tuple.first game.players


mapActivePlayer : (Player -> Player) -> Game -> Game
mapActivePlayer transform game =
    { game
        | players =
            case Tuple.second game.turn of
                PlayerOne ->
                    Tuple.mapFirst transform game.players

                PlayerTwo ->
                    Tuple.mapSecond transform game.players
    }


mapEnemyPlayer : (Player -> Player) -> Game -> Game
mapEnemyPlayer transform game =
    { game
        | players =
            case Tuple.second game.turn of
                PlayerOne ->
                    Tuple.mapSecond transform game.players

                PlayerTwo ->
                    Tuple.mapFirst transform game.players
    }


playerOne : Game -> Player
playerOne =
    .players >> Tuple.first


playerTwo : Game -> Player
playerTwo =
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


selectFriendlyModel : Game -> Model -> Game
selectFriendlyModel game model =
    mapActivePlayer (flip Player.selectModel <| model.id) game


view : Game -> (Model -> msg) -> List (Svg msg)
view game msg =
    [ Tabletop.view game.tabletop
    , Player.view (activePlayer game) (turn game |> Turn.phase)
    , Projectile.view game.projectile
    , Player.gangView (playerOne game) msg
    , Player.gangView (playerTwo game) msg
    ]
