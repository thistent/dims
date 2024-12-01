module Samples exposing (..)

import Html exposing (Html)
import Shared exposing (..)


helloPony : List (Html Msg)
helloPony =
    [ ln "01"
    , Html.span [ sty "color" "#ff6688" ] [ Html.text "actor" ]
    , Html.text " "
    , Html.span [ sty "color" "#0099cc" ] [ Html.text "Main\n" ]
    , ln "02"
    , Html.text "    "
    , Html.span [ sty "color" "#ffaa66" ] [ Html.text "new" ]
    , Html.text " "
    , Html.span [ sty "color" "#ffeeaa" ] [ Html.text "create" ]
    , Html.span [ sty "color" "#444444" ] [ Html.text "(" ]
    , Html.text " "
    , Html.span [ sty "color" "#ddeeff" ] [ Html.text "env" ]
    , Html.text " "
    , Html.span [ sty "color" "#444444" ] [ Html.text ":" ]
    , Html.text " "
    , Html.span [ sty "color" "#0099cc" ] [ Html.text "Env" ]
    , Html.text " "
    , Html.span [ sty "color" "#444444" ] [ Html.text ")" ]
    , Html.text " "
    , Html.span [ sty "color" "#444444" ] [ Html.text "=>\n" ]
    , ln "03"
    , Html.text "        "
    , Html.span [ sty "color" "#ddeeff" ] [ Html.text "env" ]
    , Html.span [ sty "color" "#444444" ] [ Html.text "." ]
    , Html.span [ sty "color" "#8866dd" ] [ Html.text "out" ]
    , Html.span [ sty "color" "#444444" ] [ Html.text "." ]
    , Html.span [ sty "color" "#ffeeaa" ] [ Html.text "print" ]
    , Html.span [ sty "color" "#444444" ] [ Html.text "(" ]
    , Html.text " "
    , Html.span [ sty "color" "#006622" ] [ Html.text "\"" ]
    , Html.span [ sty "color" "#44aa00" ] [ Html.text "Hello, World!" ]

    --, Html.span [ sty "color" "#aa4400" ] [ Html.text <| String.repeat 200 "." ]
    , Html.span [ sty "color" "#bbff77" ] [ Html.text "\\n" ]
    , Html.span [ sty "color" "#006622" ] [ Html.text "\"" ]
    , Html.text " "
    , Html.span [ sty "color" "#444444" ] [ Html.text ")" ]
    ]
