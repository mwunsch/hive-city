module Projectile exposing (..)

import Animation exposing (px, moveTo, lineTo, path)
import Svg exposing (Svg)
import Svg.Attributes as Attr exposing (stroke, class)
import Tabletop exposing (Position, Inch, offTable, posX, posY)


type alias Projectile =
    { style : Animation.State
    , color : String
    }


init : Projectile
init =
    { style =
        Animation.style
            [ path
                [ moveTo (posX offTable) (posY offTable)
                , lineTo (posX offTable) (posY offTable)
                ]
            ]
    , color = "green"
    }


travel : Position -> Position -> Projectile -> Projectile
travel ( fromX, fromY ) ( toX, toY ) projectile =
    { projectile
        | style =
            projectile.style
                |> Animation.interrupt
                    [ Animation.set
                        [ path
                            [ moveTo fromX fromY
                            , lineTo fromX fromY
                            ]
                        ]
                    , Animation.to
                        [ path
                            [ moveTo fromX fromY
                            , lineTo toX toY
                            ]
                        ]
                    , Animation.to
                        [ path
                            [ moveTo toX toY
                            , lineTo toX toY
                            ]
                        ]
                    ]
    }


view : Projectile -> Svg msg
view projectile =
    Svg.path (Animation.render projectile.style ++ [ Attr.strokeWidth "0.35px"
                                                   , stroke projectile.color
                                                   , class "laser" ]) []
