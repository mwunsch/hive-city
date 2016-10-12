module Model exposing (..)

{-| The "Model" is not a traditional Elm Architecture model, but an
individual fighter in a gang. "Model" refers to the physical token
used in the tabletop game.

From the rulebook:

> Each model fighter is defined by a set of characteristics namely:
> Move, Weapon Skill, Ballistic Skill, Strength, Toughness, Wounds,
> Initiative, Attacks and Leadership. Each characteristic is assigned a
> value of (usually) between 1 and 10. The higher value your model has
> for any characteristic the better – for example, a model with a
> Strength of 6 is stronger than a model with a Strength of
> 2.

-}

import Html exposing (Html, table, th, td, tr, colgroup, col)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Position, Inch, posX, posY)


type alias Model =
    { profile : Characteristics
    , position : Position
    , hidden : Bool
    , pinned : Bool
    , injury : Maybe Injury
    , remainingMove : Float
    , selected : Bool
    , id : Id
    , fighterType : FighterType
    }


type alias Id =
    Int


type Injury
    = FleshWound
    | Down
    | OutOfAction


type FighterType
    = Leader
    | Ganger
    | Heavy
    | Juve


{-| From the rulebook:

> Move (M). A model’s move shows the number of inches it can move in a
> turn under normal circumstances. The usual number is 4 as almost all
> fighters move 4" per turn, although they can move faster when they
> are running or charging, as you’ll see later.

> Weapon Skill (WS). Weapon Skill is a measure of close combat ability
> – how good the person is at hand-to-hand fighting. A good swordsman
> or a skilled knife fighter would have a high value compared to a
> green juve, for example. An average value is 3.

> Ballistic Skill (BS). Ballistic Skill shows how good a shot the
> individual is. When you shoot a gun the chance of hitting a target
> depends upon your Ballistic Skill. An average score is 3 and a
> dead-eye shot might have a Ballistic Skill of 4, 5 or even higher.

> Strength (S). Strength simply shows how strong a person is! Strength
> is especially important when you fight hand-to-hand combat because
> the stronger you are the harder you can hit or thrust. A Strength
> value of 3 is about average.

> Toughness (T). Toughness is a measure of how easily an individual
> can withstand a hit from a weapon or a blow from a club, hand weapon
> or fist. The tougher you are the harder you are to wound or kill. An
> average value is 3.

> Wounds (W). A model’s Wounds value shows how many times a fighter
> can be wounded before he collapses and goes down dead or
> incapacitated. Most individuals have a Wound value of only 1 but
> tough old gang leaders and veteran gangers might have a value of 2
> or more.

> Initiative (I). The Initiative value represents alertness and
> reaction speed. It determines a fighter’s chance of dodging a sudden
> blow or gathering his wits while shells and bombs explode around
> him. An average Initiative value is 3.

> Attacks (A). The Attacks value indicates the number of ‘Attack dice’
> rolled when the model fights in hand- to-hand combat. Most
> individuals roll only one dice and so have an Attacks value of 1,
> but experienced fighters might have a higher value and roll
> correspondingly more dice. The more dice you roll the more chance
> you have of beating your opponent!

> Leadership (Ld). Leadership represents raw courage and
> self-control. The higher a person’s Leadership the more likely he is
> to remain steadfast in combat while others run off or are slain
> around him. An average Leadership value is 7.

-}
type alias Characteristics =
    { move : Inch
    , weaponSkill : Int
    , ballisticSkill : Int
    , strength : Int
    , toughness : Int
    , wounds : Int
    , initiative : Int
    , attacks : Int
    , leadership : Int
    }


averageFighterProfile : Characteristics
averageFighterProfile =
    { move = 4
    , weaponSkill = 3
    , ballisticSkill = 3
    , strength = 3
    , toughness = 3
    , wounds = 1
    , initiative = 3
    , attacks = 1
    , leadership = 7
    }


averageFighter : Position -> Model
averageFighter pos =
    { profile = averageFighterProfile
    , position = pos
    , hidden = False
    , pinned = False
    , injury = Nothing
    , remainingMove = (toFloat averageFighterProfile.move)
    , selected = False
    , id = 1
    , fighterType = Ganger
    }


type alias MovementError =
    ( String, Model )


attemptMove : Model -> Position -> Result MovementError Model
attemptMove model pos =
    let
        distance =
            Tabletop.distance model.position pos

        allowableDistance =
            model.remainingMove - distance

        maxPosition =
            Tabletop.positionFromDirection model.position pos model.remainingMove
    in
        if allowableDistance > 0 then
            Ok
                { model
                    | position = pos
                    , remainingMove = allowableDistance
                }
        else
            Err
                ( "Can't move there"
                , { model
                    | position = maxPosition
                    , remainingMove = 0
                  }
                )



view : Model -> msg -> Svg msg
view model msg =
    text'
        [ fontSize "0.985"
        , fontFamily "monospace"
        , textAnchor "middle"
        , fill <|
            if model.selected then
                "white"
            else
                "black"
        , x
            (model.position |> posX |> toString)
        , y
            (model.position |> posY |> toString)
        , onClick msg
        , Svg.Attributes.cursor "pointer"
        ]
        [ text "@" ]


viewProfile : Model -> Html msg
viewProfile model =
    let
        ( columns, values ) =
            List.unzip
                [ ( "M", model.profile.move )
                , ( "WS", model.profile.weaponSkill )
                , ( "BS", model.profile.ballisticSkill )
                , ( "S", model.profile.strength )
                , ( "T", model.profile.toughness )
                , ( "W", model.profile.wounds )
                , ( "A", model.profile.attacks )
                , ( "Ld", model.profile.leadership )
                ]

    in
        table
            []
            [ colgroup [] [ col [] [] ]
            , tr [] <|
                List.map (toString >> Html.text >> List.repeat 1 >> th []) columns
            , tr [] <|
                List.map (toString >> Html.text >> List.repeat 1 >> td []) values
            ]
