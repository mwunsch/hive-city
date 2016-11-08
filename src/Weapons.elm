module Weapons exposing (..)

import Tabletop exposing (Inch)
import Either exposing (Either(..))


type Weapon
    = Combat Profile
    | Pistol Profile
    | Basic Profile
    | Special Profile
    | Heavy Profile
    | Grenade Profile


type alias Profile =
    { range : Range
    , toHit : ( Maybe Modifier, Maybe Modifier )
    , strength : Either Modifier Int
    , damage : Int
    , saveModifier : Modifier
    , ammoRoll : Maybe Int
    }


type Range
    = Close
    | Template
    | Ranged ShortRange LongRange


type alias ShortRange =
    ( Inch, Inch )


type alias LongRange =
    ( Inch, Inch )


{-| For some attributes, like Strength or To Hit, they are a modifier
applied to either the wielders strength or to dice rolls.

-}
type alias Modifier =
    Int -> Int


asUser : Modifier
asUser =
    (+) 0


knife : Weapon
knife =
    Combat
        { range = Close
        , toHit = ( Nothing, Nothing )
        , strength = Left asUser
        , damage = 1
        , saveModifier = asUser
        , ammoRoll = Nothing
        }
