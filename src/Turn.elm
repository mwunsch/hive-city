module Turn exposing (..)


type Phase
    = Movement
    | Shooting
    | HandToHand
    | Recovery


type alias Turn =
    { round : Int
    , phase : Phase
    }


init : Turn
init =
    { round = 1
    , phase = Movement
    }


advance : Turn -> Turn
advance turn =
    case turn.phase of
        Movement ->
            { turn | phase = Shooting }

        Shooting ->
            { turn | phase = HandToHand }

        HandToHand ->
            { turn | phase = Recovery }

        Recovery ->
            { turn
                | round = turn.round + 1
                , phase = Movement
            }
