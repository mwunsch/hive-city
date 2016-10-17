module Player exposing (..)

import Gang exposing (Gang)
import Maybe exposing (andThen)
import Model exposing (Model)
import Tabletop exposing (Tabletop, Position)


type alias Player =
    { gang : Gang
    , selection : Maybe Model.Id
    , movementIntention : Position
    , credits : Int
    }


init : Tabletop -> Player
init table =
    { gang = Gang.empty
    , selection = Nothing
    , movementIntention = Tabletop.center table
    , credits = 1000
    }


selectModel : Player -> Model.Id -> Player
selectModel player id =
    { player
        | gang =
            .gang (deselectAll player)
                |> Gang.update id (Maybe.map (\f -> { f | selected = True }))
        , selection = Just id
    }


deselectAll : Player -> Player
deselectAll player =
    { player
        | gang =
            player.gang
                |> Gang.map (\k v -> { v | selected = False })
        , selection = Nothing
    }


getSelectedGangMember : Player -> Maybe Model
getSelectedGangMember player =
    player.selection `andThen` (flip Gang.get) player.gang


{-| From the rulebook:

> You have 1000 Guilder credits to spend on recruiting and arming your
  gang within the following guidelines.

+ Minimum 3 Fighers: A gang must have at least three models.
+ Leader: your gang must have one leader. Not more. Not less!
+ Gangers: You can include as many gangers as you can afford.
+ Heavies: A gang can have up to two heavies but no more.
+ Juves: No more than half the gang can be made up of juves.
+ Knifes: All fighters are assumed to have a knife even if the model doesn't have one.

-}
recruit : Model -> Player -> Maybe Player
recruit model player =
    let
        cost =
            Model.cost model

        remainingCredits =
            player.credits - cost
    in
        if remainingCredits >= 0 then
            Just
                { player
                    | credits = remainingCredits
                    , gang = Gang.add model player.gang
                }
        else
            Nothing
