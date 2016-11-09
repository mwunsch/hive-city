module Weapons exposing (..)

import List
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
    , icon : String
    }


profile : Weapon -> Profile
profile weapon =
    case weapon of
        Combat profile ->
            profile

        Pistol profile ->
            profile

        Basic profile ->
            profile

        Special profile ->
            profile

        Heavy profile ->
            profile

        Grenade profile ->
            profile


type Range
    = Close
    | Template
    | Ranged ShortRange LongRange


type alias ShortRange =
    ( Inch, Inch )


type alias LongRange =
    ( Inch, Inch )


isRanged : Weapon -> Bool
isRanged weapon =
    case weapon of
        Combat _ ->
            False

        _ ->
            True


{-| For some attributes, like Strength or To Hit, they are a modifier
applied to either the wielders strength or to dice rolls.

-}
type alias Modifier =
    Int -> Int


asUser : Modifier
asUser =
    (+) 0


plusOne : Modifier
plusOne =
    (+) 1


plusTwo : Modifier
plusTwo =
    (+) 2



-- WEAPONS


knife : Weapon
knife =
    Combat
        { range = Close
        , toHit = ( Nothing, Nothing )
        , strength = Left asUser
        , damage = 1
        , saveModifier = asUser
        , ammoRoll = Nothing
        , icon = "\x1f5e1"
        }


autopistol : Weapon
autopistol =
    Pistol
        { range = Ranged ( 0, 8 ) ( 8, 16 )
        , toHit = ( Just plusTwo, Nothing )
        , strength = Right 3
        , damage = 1
        , saveModifier = asUser
        , ammoRoll = Just 4
        , icon = "\x1f52b"
        }


autogun : Weapon
autogun =
    Basic
        { range = Ranged ( 0, 12 ) ( 12, 24 )
        , toHit = ( Just plusOne, Nothing )
        , strength = Right 3
        , damage = 1
        , saveModifier = asUser
        , ammoRoll = Just 4
        , icon = "\x1f529"
        }
