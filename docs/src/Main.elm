module Main exposing (..)

-- import Html.Attributes exposing (id, style)

import Browser
import Color
import Diml
import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Math as Math exposing (MathExpr(..))
import Html exposing (Html)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Filters.Attributes exposing (..)
import TypedSvg.Types as Types exposing (EdgeMode(..), FontWeight(..), Paint(..), px)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subs
        , update = update
        , view = view
        }



-- Model --


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init () =
    ( 0, Cmd.none )


type Msg
    = NoOp


type alias ColorScheme =
    { fg : El.Color
    , bg : El.Color
    , link : El.Color
    , math : Math.ColorScheme
    }



-- Subs --


subs : Model -> Sub Msg
subs model =
    Sub.batch []



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View --


f : Float
f =
    42


m : Float
m =
    f * 0.5


view : Model -> Html Msg
view model =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Font.color newspaper.fg --<| El.rgb 0.8 0.8 0.8

        --, Font.family [ Font.serif ]
        , getExtFont "Dosis"

        --, getExtFont "JetBrains"
        --, getExtFont "Roboto"
        , Font.size 18
        , Bg.color newspaper.bg --<| El.rgb 0 0 0
        ]
    <|
        El.row
            [ El.width El.fill
            , El.height El.fill
            ]
            [ El.column
                [ El.alignTop
                , El.height El.fill
                , newspaper.fg |> Math.changeAlpha 0.1 |> Bg.color
                , Border.shadow
                    { offset = ( 2.0, 0 )
                    , size = 4.0
                    , blur = 10
                    , color = newspaper.fg |> Math.changeAlpha 0.2
                    }
                , El.padding <| round f
                ]
                [ el
                    [ Font.size <| round <| f
                    , Font.bold
                    ]
                  <|
                    El.text "DIMS"
                , El.text "Distributed Idea Mapping System"
                ]
            , El.textColumn
                [ El.width El.fill
                , El.height El.fill
                , El.padding <| round f
                ]
                [ el [ El.width El.fill ] <|
                    el
                        [ El.centerY
                        , Font.bold
                        , Font.size <| round f
                        ]
                    <|
                        El.text "Hello, World! "
                , El.paragraph []
                    [ El.text "Welcome to the "
                    , el [ Font.bold ] <| El.text "DIMS"
                    , El.text " page!"
                    ]
                , el [ El.height <| El.px <| round f ] El.none
                , El.paragraph []
                    [ El.text "I'm currently working on a math expression AST"
                    , El.text " and a renderer that displays the sorts of equations"
                    , El.text " that I need for fitting together the type theories"
                    , El.text " of "
                    , el [ Font.bold ] <| El.text "Plutus Core"
                    , El.text " and other languages I want to target with my language, "
                    , el [ Font.bold ] <| El.text "Poplar"
                    , El.text "."
                    ]
                , el [ El.height <| El.px <| round f ] El.none
                , El.paragraph []
                    [ El.text "Here are some"
                    , el [ Font.bold ] <| El.text " nonsensical"
                    , El.text " equations that I'm using for"
                    , El.text " typesetting purposes:"
                    ]
                , el [ El.height <| El.px <| round f ] El.none
                , El.row
                    [ El.centerX
                    , El.spacing <| round <| f * 0.3
                    ]
                    [ Math.render (m * 0.8)
                        newspaper.math
                        (ReductionRule "do-something"
                            (Infix
                                (ReductionRule "this-one"
                                    (Exp (Var "b")
                                        (Var "c")
                                    )
                                    (Infix (Var "x") (Op "+") (Const "1"))
                                )
                                (Var Math.arrow)
                                (ReductionRule "another-rule"
                                    (Sub (Var "d")
                                        (Var "e")
                                    )
                                    (Infix (Var "y") (Op "+") (Const "2"))
                                )
                            )
                            (Infix
                                (Var "a"
                                    |> OfType (TyVar "T")
                                    |> OfKind
                                        (Pars
                                            (Infix
                                                (Pars
                                                    (Infix
                                                        (KVar Math.star)
                                                        (Var Math.arrow)
                                                        (KVar Math.star)
                                                    )
                                                )
                                                (Var Math.arrow)
                                                (KVar Math.star)
                                            )
                                        )
                                )
                                (Op "+")
                                (Lam (Var "x")
                                    (Pars
                                        (Infix
                                            (Sub
                                                (Exp (Const "3") (Var "a"))
                                                (Var "b")
                                            )
                                            (Sub (Op Math.equiv) (Op "Ty"))
                                            (Exp
                                                (Sub (Const "3") (Var "a"))
                                                (Var "b")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ]
                , Math.render (m * 1) newspaper.math <|
                    Pars
                        (Lam
                            (OfKind
                                (Pars
                                    (Infix (KVar Math.star)
                                        (Op Math.arrow)
                                        (KVar Math.star)
                                    )
                                )
                                (Var "x")
                            )
                            (Pars
                                (Lam
                                    (OfKind
                                        (Pars
                                            (Infix (KVar Math.star)
                                                (Op Math.arrow)
                                                (KVar Math.star)
                                            )
                                        )
                                        (Var "y")
                                    )
                                    (Var "y")
                                )
                            )
                        )
                , el [ El.height <| El.px <| round f ] El.none
                , El.paragraph []
                    [ El.text "Ultimately, the goal is to build a zettlekasten-like"
                    , El.text " mind-mapping experience with a simple flavor of Markdown,"
                    , El.text " as well as some built-in tools for working with ASTs"
                    , El.text " for things like building math equations, or mapping out"
                    , El.text " timelines for a project."
                    ]
                , el [ El.height <| El.px <| round f ] El.none
                , El.paragraph []
                    [ El.text "Currently, I'm just formatting this page by hand,"
                    , El.text " but eventually, documentation like this,"
                    , El.paragraph []
                        [ El.text " or other formats like"
                        , latex 18.0
                        , El.text ","
                        ]
                    , El.text " should be automatically generated from any subset of"
                    , El.text " someone's \"second brain\" that can be managed by a"
                    , El.text " high-performance backend running privately,"
                    , El.text " in a way that links nodes together,"
                    , El.text " or granting people public access."
                    , El.text " Credentials could be based on linking a wallet address,"
                    , El.text " or an Ada Handle."
                    ]
                ]
            ]



-- Color Schemes --


black : El.Color
black =
    El.rgb 0 0 0


white : El.Color
white =
    El.rgb 1 1 1


newspaper : ColorScheme
newspaper =
    { fg = El.rgb 0.2 0.2 0.2 --0.2 0.15 0
    , bg = El.rgb 0.8 0.8 0.8 --0.88 0.86 0.84
    , link = El.rgb 0 0.3 0.6 -- 0.7 0.1 0
    , math =
        { const = black
        , var = El.rgb 0 0.2 0.6
        , tyVar = El.rgb 0 0.6 0.6
        , kVar = El.rgb 0 0.6 0.3
        , infix = El.rgb 0.4 0.4 0.4
        , op = El.rgb 0.4 0.4 0.4
        , exp = black
        , frac = El.rgb 0.6 0.6 0.6
        , ofType = black
        , ofKind = black
        , pars = El.rgb 0.3 0.3 0.3
        , lam = El.rgb 0.8 0.3 0
        , reductionRule = black
        , replace = black
        }
    }



-- Other --


getExtFont : String -> Attribute Msg
getExtFont extFont =
    Font.family
        [ Font.external
            { name = extFont
            , url = "https://fonts.googleapis.com/css?family=" ++ extFont
            }
        ]


edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


corners =
    { topLeft = 0, bottomLeft = 0, bottomRight = 0, topRight = 0 }


latex : Float -> Element Msg
latex size =
    el
        [ El.inFront <|
            El.row
                [ getExtFont "Merriweather"
                , El.width El.shrink
                , El.height El.shrink
                , El.moveUp <| size * 0.8
                , El.moveRight <| size * 0.2
                ]
                [ el [] <| El.text " L"
                , el
                    [ El.moveUp <| size * 0.2
                    , El.moveLeft <| size * 0.35
                    , Font.size <| round <| size * 0.85
                    ]
                  <|
                    El.text " A"
                , el [ El.moveLeft <| size * 0.45 ] <| El.text " T"
                , el
                    [ El.moveLeft <| size * 0.65
                    , El.moveDown <| size * 0.25
                    ]
                  <|
                    El.text " E"
                , el
                    [ El.moveLeft <| size * 0.8
                    , El.moveDown <| size * 0.1
                    ]
                  <|
                    El.text " X"
                ]
        , El.width <| El.px <| round <| size * 2.8
        ]
        El.none



-- Not Yet Needed --
{--
svgImage : Element msg
svgImage =
    El.html <|
        svg
            [ viewBox 0 0 600 200
            ]
            [ rect
                [ x <| px 0
                , y <| px 0
                , width <| px 400
                , height <| px 400
                , fill <| Paint <| Color.rgba 0.0 0.0 0.5 1
                ]
                []
            , text_
                [ x (px 75)
                , y (px 75)
                , width <| px 350
                , height <| px 350
                , fontFamily [ "Merriweather", "serif" ]

                --, getExtFont "FiraCode"
                , fontSize (px 4.9)
                , fontWeight FontWeightBold
                , fill <| Paint <| Color.rgb 0.5 0.5 0.5
                , edgeMode EdgeModeWrap
                ]
                [ text "The documentation for the Distributed Idea Mapping System (DIMS) will go here!" ]
            ]
-}
