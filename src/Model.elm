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


type alias Inch =
    Int


type alias Position =
    { x : Int
    , y : Int
    , z : Int
    }


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


type alias Model =
    { profile : Characteristics
    , position : Position
    , hidden : Bool
    , pinned : Bool
    , injury : Maybe Injury
    }


move : Model -> Position -> Model
move model pos =
    { model | position = pos }


type Injury
    = FleshWound
    | Down
    | OutOfAction
