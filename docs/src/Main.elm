module Main exposing (..)

-- import Html.Attributes exposing (id, style)

import Browser
import Color
import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Math as Math exposing (MathExpr(..))
import Html exposing (Html)
import Markup
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
    , heading : HeadingColor
    , math : Math.ColorScheme
    }


type alias HeadingColor =
    { h1 : El.Color
    , h2 : El.Color
    , h3 : El.Color
    , h4 : El.Color
    , h5 : El.Color
    , h6 : El.Color
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
                    , El.text " equations that I'm using for"
                    , El.text " typesetting purposes:"
                    ]
                , el [ El.height <| El.px <| round f ] El.none
                , El.row
                    [ El.centerX
                    , El.spacing <| round <| f * 1.5
                    ]
                    [ tyvarRender
                    , tyallRender
                    , tyfixRender
                    ]
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
    , heading =
        { h1 = El.rgb 0.2 0.1 0.3
        , h2 = El.rgb 0 0.3 0.5
        , h3 = El.rgb 0 0.5 0.6
        , h4 = El.rgb 0.3 0.6 0
        , h5 = El.rgb 0.8 0.5 0
        , h6 = El.rgb 0.75 0.1 0
        }
    , math =
        { const = black
        , var = El.rgb 0 0.3 0.9
        , tyVar = El.rgb 0 0.6 0.9
        , kVar = El.rgb 0 0.6 0.3
        , op = El.rgb 0.4 0.4 0.4
        , exp = black
        , frac = El.rgb 0.6 0.6 0.6
        , ofType = black
        , ofKind = black
        , pars = El.rgb 0.3 0.3 0.3
        , lam = El.rgb 0.7 0.4 0.7
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


mdTextOne =
    """
# Hello, World!

Welcome to the **DIMS** page!

I'm currently working on a math expression AST and a rendering engine for working with the type theory of Plutus Core, and other languages that will be part of the research and development of my language, **Poplar**.

here are some equations from the Plutus Core Spec. that I'm using for formatting and typesetting purposes:
"""


tyvarRender =
    Math.render 18.0 newspaper.math <|
        Math.reductionRule "tyvar"
            (Math.var Math.alpha
                |> Math.ofKind (Math.kindNamed "K")
                |> Math.pars
                |> Math.elementOf Math.context
            )
            (Math.var Math.alpha
                |> Math.ofKind (Math.kindNamed "K")
                |> Math.impliedBy Math.context
            )


tyallRender =
    Math.render 18.0 newspaper.math <|
        Math.reductionRule "tyall"
            (Math.typeNamed "A"
                |> Math.ofKind Math.kindStar
                |> Math.impliedBy
                    (Math.context
                        |> Math.contains
                            (Math.var Math.alpha
                                |> Math.ofKind (Math.kindNamed "K")
                            )
                    )
            )
            ((Math.pars
                (Math.forAll (Math.var Math.alpha)
                    (Math.kindNamed "K")
                    (Math.typeNamed "A")
                )
                |> Math.ofKind Math.kindStar
             )
                |> Math.impliedBy Math.context
            )


tyfixRender =
    Math.render 18.0 newspaper.math <|
        Math.reductionRule "tyfix"
            (Math.exprList
                [ Math.typeNamed "B"
                    |> Math.ofKind (Math.kindNamed "K")
                    |> Math.impliedBy Math.context
                , Math.typeNamed "A"
                    |> Math.ofKind
                        (Math.fun
                            (Math.fun
                                (Math.kindNamed "K")
                                Math.kindStar
                            )
                            (Math.fun (Math.kindNamed "K") Math.kindStar)
                        )
                    |> Math.impliedBy Math.context
                ]
            )
            ((Math.ifix (Math.typeNamed "A") (Math.typeNamed "B")
                |> Math.ofKind Math.kindStar
             )
                |> Math.impliedBy Math.context
            )


mdTextTwo =
    """
Ultimately, the goal is to build a zettlekasten-like mind-mapping system with a simplified flavor of Markdown, as well as the ablility to work with various forms of mathematical equations. 

## Distributed Collaboration

### Managing Projects 

"""



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
