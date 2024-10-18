module Main exposing (main)

-- import Html exposing (Html, div, text)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr


s : String -> String -> Html.Attribute msg
s =
    Attr.style


main : Program () () ()
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subs
        , view =
            \() ->
                { title = "Dims: A Platform for Decentralized Research & Collaboration"
                , body = [ view () ]
                }
        }


init () =
    ( (), Cmd.none )


update () () =
    ( (), Cmd.none )


subs () =
    Sub.none


view () =
    Html.node "body"
        [ s "background-color" "#000000"
        , s "color" "#ffffff"
        , s "width" "100vw"
        , s "height" "100vh"
        , s "font-family" "monospace"
        , s "font-size" "1.5vmax"
        , s "line-height" "2.5vmax"
        ]
        [ Html.div
            [ s "padding" "2vmax" ]
            [ Html.text "Dims: A Platform for Decentralized Research & Collaboration"
            , Html.div
                [ s "font-size" "1vmax"
                , s "padding-top" "0.5vmax"
                , s "color" "#c8c8c8"
                ]
                [ Html.text "- Documentation about this project will go here."
                ]
            ]
        ]
