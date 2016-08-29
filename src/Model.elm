module Model exposing (..)

type alias Inch = Int

type alias Model =
    { move : Inch
    , weaponSkill : Int
    , ballisticSkill: Int
    , strength: Int
    , toughness: Int
    , wounds: Int
    , initiative: Int
    , attacks: Int
    , leadership: Int
    }
