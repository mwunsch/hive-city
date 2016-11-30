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

import Dice exposing (Die)
import Html exposing (Html, table, th, td, tr, colgroup, col)
import List
import Random exposing (Generator)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tabletop exposing (Position, Inch, posX, posY)
import Utilities exposing (textNode, onClickWithoutPropagation)
import Uuid exposing (Uuid, uuid)
import Weapons exposing (Weapon, Shot(..), knife, autogun, autopistol)


type alias Model =
    { profile : Characteristics
    , position : Position
    , hidden : Bool
    , pinned : Bool
    , injury : Maybe Injury
    , remainingMove : Inch
    , selected : Bool
    , id : Id
    , fighterType : FighterType
    , name : String
    , equipment : List Weapon
    , bearing : Float
    }


type alias Id =
    Uuid


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


ganger : Characteristics
ganger =
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


leader : Characteristics
leader =
    { ganger
        | weaponSkill = 4
        , ballisticSkill = 4
        , initiative = 4
        , leadership = 8
    }


heavy : Characteristics
heavy =
    ganger


juve : Characteristics
juve =
    { ganger
        | weaponSkill = 2
        , ballisticSkill = 2
        , leadership = 6
    }


init : FighterType -> Model
init role =
    let
        template =
            case role of
                Ganger ->
                    ganger

                Leader ->
                    leader

                Heavy ->
                    heavy

                Juve ->
                    juve
    in
        { profile = template
        , position = Tabletop.offTable
        , hidden = False
        , pinned = False
        , injury = Nothing
        , remainingMove = template.move
        , selected = False
        , id = Uuid.scheme
        , fighterType = role
        , name = "Mac McMadd"
        , equipment = [ knife ]
        , bearing = 0
        }


{-| At some point, weapon cost will have to be modeled.
-}
equip : Weapon -> Model -> Model
equip weapon model =
    { model | equipment = weapon :: model.equipment }


cost : Model -> Int
cost model =
    case model.fighterType of
        Leader ->
            120

        Ganger ->
            50

        Heavy ->
            60

        Juve ->
            25


generator : FighterType -> Generator Model
generator role =
    let
        initial =
            init role
    in
        uuid
            |> Random.map (\id -> { initial | id = (Debug.log "id" id) })
            |> Random.map (equip autogun)
            |> Random.map (equip autopistol)


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


canRun : Model -> Bool
canRun model =
    model.remainingMove == model.profile.move


{-| A Model can run twice their total movement in the Movement phase,
but lose their abilit to shoot in the shooting phase.

TODO:

> If a running model approaches within 8" of an enemy that he can see
  he must stop immediately.

-}
run : Model -> Position -> Model
run model pos =
    let
        distance =
            Tabletop.distance model.position pos

        maxPosition =
            Tabletop.positionFromDirection model.position pos (model.remainingMove * 2)
    in
        if (canRun model) then
            { model | position = maxPosition, remainingMove = 0 }
        else
            model


{-| Resolving whether a shot hits a target. The shooter's ballistic
skill determines if it's a hit.

TODO: Measure the distance between the shooter and the target and
determine how range modifiers affect the outcome.

-}
shoot : Model -> Weapon -> Model -> List Die -> Shot
shoot shooter weapon target dice =
    let
        roll =
            List.sum dice

        bs =
            shooter.profile.ballisticSkill
    in
        if roll <= 1 then
            Miss
        else if bs + roll > 6 then
            Hit
        else
            Miss


{-| A Model is within shooting range if they are:

+ Not the shooter
+ Within the maximum range of the weapon
+ They are within a ninety degree arc of sight of the shooter

The result is sorted by distance to the shooter.

-}
withinShootingRange : List Model -> Weapon -> Model -> List Model
withinShootingRange models weapon fighter =
    let
        range =
            Weapons.maxRange weapon

        arc =
            Tabletop.ninetyDegreeArc fighter.position fighter.bearing range
    in
        models
            |> List.filter ((/=) fighter)
            |> List.filter (\{ position } -> Tabletop.isWithinDistance range fighter.position position)
            |> List.filter (\{ position } -> Tabletop.isWithinArc position arc)
            |> List.sortBy (\{ position } -> Tabletop.distance fighter.position position)


view : Model -> msg -> Svg msg
view model msg =
    let
        modelX =
            model.position |> posX |> toString

        modelY =
            model.position |> posY |> toString

        bearingDegrees =
            model.bearing |> (*) 180 |> (flip (/)) pi

        rotation =
            "rotate("
                ++ String.join " " [ toString bearingDegrees, modelX, modelY ]
                ++ ")"
    in
        text_
            [ fontSize (Tabletop.millimeter 25 |> toString)
            , fontFamily "monospace"
            , textAnchor "middle"
            , alignmentBaseline "middle"
            , fill <|
                if model.selected then
                    "white"
                else
                    "black"
            , x modelX
            , y modelY
            , onClickWithoutPropagation msg
            , Svg.Attributes.cursor "pointer"
            , transform rotation
            ]
            [ text (model.fighterType |> toString |> String.left 1) ]


viewProfile : Model -> Html msg
viewProfile model =
    let
        ( columns, values ) =
            List.unzip
                [ ( "M", round model.profile.move )
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
            [ Svg.Attributes.style "border-collapse: collapse; line-height: 0;" ]
            [ colgroup [] [ col [] [] ]
            , tr [] <|
                th [] [ Html.text (model.fighterType |> toString) ]
                    :: List.map (textNode >> th []) columns
            , tr [] <|
                td [] [ Html.text model.name ]
                    :: List.map (toString >> textNode >> td []) values
            ]
