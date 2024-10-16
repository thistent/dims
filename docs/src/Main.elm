module Main exposing (main)

-- import Html exposing (Html, div, text)

import VirtualDom as Vd


s : String -> String -> Vd.Attribute msg
s =
    Vd.style


main : Vd.Node ()
main =
    Vd.node "body"
        [ s "background-color" "#000000"
        , s "color" "#ffffff"
        , s "width" "100vw"
        , s "height" "100vh"
        , s "font-family" "monospace"
        , s "font-size" "1.5vmax"
        , s "line-height" "2.5vmax"
        , s "padding" "2vmax"
        ]
        [ Vd.text "Dims: A Platform for Decentralized Research & Collaboration"
        , Vd.node "div"
            [ s "font-size" "1vmax"
            , s "padding-top" "0.5vmax"
            , s "color" "#c8c8c8"
            ]
            [ Vd.text "- Documentation about this project will go here."
            ]
        ]
