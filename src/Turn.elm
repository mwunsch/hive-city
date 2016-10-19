module Turn exposing (Turn, Phase(..), init, round, advance, phase)


type Phase
    = Movement
    | Shooting
    | HandToHand
    | Recovery


type Turn
    = Turn Int Phase


init : Turn
init =
    Turn 1 Movement


round : Turn -> Int
round (Turn num _) =
    num


phase : Turn -> Phase
phase (Turn _ p) =
    p


advance : Turn -> Turn
advance (Turn round phase) =
    case phase of
        Movement ->
            Turn round Shooting

        Shooting ->
            Turn round HandToHand

        HandToHand ->
            Turn round Recovery

        Recovery ->
            Turn (round + 1) Movement
