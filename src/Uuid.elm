module Uuid exposing (Uuid, uuid, scheme)

import Random exposing (Generator, andThen)
import String
import Array exposing (Array)
import Maybe exposing (withDefault)


type alias Uuid =
    String


uuid : Generator Uuid
uuid =
    let
        hexgen =
            Random.int 0 15

        listgen : Generator (List a)
        listgen =
            Random.map (always []) Random.bool

        numToHex : Int -> Char
        numToHex =
            (flip Array.get) hexTable >> withDefault 'x'
    in
        [ 8, 4, 4, 4, 12 ]
            |> List.map (\num -> Random.list num hexgen)
            |> List.foldr (Random.map2 (::)) listgen
            |> Random.map (List.map (List.map numToHex >> String.fromList))
            |> Random.map (String.join "-")


scheme : Uuid
scheme =
    "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"


hexTable : Array Char
hexTable =
    Array.fromList [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' ]
