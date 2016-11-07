module Weapons exposing (..)

import Tabletop exposing (Inch)


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
    , strength : Int
    , damage : Int
    , saveModifier : Maybe Modifier
    , ammoRoll : Maybe Int
    }


type Range
    = Close
    | Ranged Short Long


type alias Short =
    ( Inch, Inch )


type alias Long =
    ( Inch, Inch )


type alias Modifier =
    Int -> Int


knife : Weapon
knife =
    Combat
        { range = Close
        , toHit = ( Nothing, Nothing )
        , strength = 1
        , damage = 1
        , saveModifier = Nothing
        , ammoRoll = Nothing
        }
