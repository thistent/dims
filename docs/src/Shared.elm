module Shared exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Http


type alias Model =
    { content : String
    , name : String
    , env : Ast
    }


type Msg
    = RequestDoc String
    | ReceiveDoc String (Result Http.Error String)


type Ast
    = DocText String
    | Delimiter String
    | CodeText String


sty : String -> String -> Html.Attribute Msg
sty =
    Attr.style


ln : String -> Html Msg
ln numStr =
    Html.span
        [ sty "user-select" "none"
        , sty "color" "#555555"
        , sty "padding-left" "12px"
        ]
        [ Html.text <| numStr
        , Html.span [ sty "color" "#333333" ] [ Html.text "┊" ]
        ]
