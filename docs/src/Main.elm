port module Main exposing (..)

--import Svg.Styled as Svg
--import Svg.Styled.Attributes as Svg

import AlgGraph as Ag
import Array exposing (Array)
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Nav
import Color exposing (Color)
import Color.Blending as Blend
import Color.Manipulate as Manip
import Css
import Curve
import Dict
import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Element.Math as Math exposing (MathExpr)
import Element.Region as Region
import Force exposing (State)
import Graph exposing (Graph)
import Html exposing (Html)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Styled as HtmlS
import Json.Decode as Decode
import Json.Encode as Encode
import Markup
import Path
import Return exposing (Return, return)
import Set
import Shape
import SubPath exposing (SubPath)
import Task
import Time
import TypedSvg as Ts
import TypedSvg.Attributes as Ta
import TypedSvg.Core as Tc exposing (Svg)
import TypedSvg.Filters.Attributes as Tf
import TypedSvg.Types as Tt
import Url



-- Ports --


port saveState : Encode.Value -> Cmd msg


port clearState : () -> Cmd msg



-- Model --


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , time : Time.Posix
    , mousePos : XY
    , theme : ColorScheme
    , viewport : Maybe Dom.Element
    , menu : Visibility
    , graph : Graph NodeId Entity ()
    , drag : Maybe Drag
    , forceSim : Maybe (Force.State NodeId)
    , textBox : String
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | ToggleTheme
    | ToggleMenu
    | Tick Time.Posix
    | DragStart NodeId XY
    | DragAt XY
    | DragEnd XY
    | GetViewport (Maybe Dom.Element)
    | UpdateText String
    | FocusHere
    | NoOp


type Theme
    = LightMode
    | DarkMode


type Visibility
    = Open
    | Closed


type alias ColorScheme =
    { label : Theme
    , fg : El.Color
    , bg : El.Color
    , settingsBar : El.Color
    , link : El.Color
    , alt : El.Color
    , h1 : El.Color
    , h2 : El.Color
    , h3 : El.Color
    , h4 : El.Color
    , h5 : El.Color
    , h6 : El.Color
    , math : Math.ColorScheme
    }



-- Graph Types --


type alias XY =
    { x : Float, y : Float }


type alias Drag =
    { start : XY
    , current : XY
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


type alias NodeId =
    Int



{-
   type alias NodeVal =
       { name : String
       , value : Maybe Int
       }
-}


type alias ElementContext =
    { height : Float
    , width : Float
    , x : Float
    , y : Float
    }


emptyEntity : NodeId -> String -> Entity
emptyEntity id val =
    Force.entity id val



-- Constants --


f : Float
f =
    20


s : Float
s =
    1.5



-- Main Function --


main : Program ( Int, Maybe String ) Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Initial State --


init :
    ( Int, Maybe String )
    -> Url.Url
    -> Nav.Key
    -> Return Msg Model
init ( time, initSaveState ) url key =
    return
        { key = key
        , url = url
        , time = Time.millisToPosix time
        , mousePos = { x = 0, y = 0 }
        , theme = lightMode
        , viewport = Nothing
        , menu = Closed
        , graph = graph
        , drag = Nothing
        , forceSim = Nothing
        , textBox = ""
        }
    <|
        getViewportTask "svg-image-1"


{-| The forces that act on nodes and links.
FIXME: Get real initial graph state, and don't render if empty
-}
graph : Graph NodeId Entity ()
graph =
    stickManGraph
        |> Graph.map
            (\id maybeData ->
                case maybeData of
                    Just data ->
                        Just <| emptyEntity id data

                    Nothing ->
                        --Nothing
                        Just <| emptyEntity id "Nothing"
            )



-- Update --


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    Return.singleton model

                Browser.External href ->
                    return model <|
                        Nav.load href

        UrlChanged url ->
            Return.singleton model

        ToggleTheme ->
            Return.singleton
                { model
                    | theme =
                        case model.theme.label of
                            LightMode ->
                                darkMode

                            DarkMode ->
                                lightMode
                }

        ToggleMenu ->
            Return.singleton
                { model
                    | menu =
                        case model.menu of
                            Open ->
                                Closed

                            Closed ->
                                Open
                }

        Tick time ->
            return
                (updateSim { model | time = time })
            <|
                getViewportTask "svg-image-1"

        DragStart index xy ->
            Return.singleton
                { model
                    | mousePos = xy
                    , drag = Just <| Drag xy xy index
                }

        DragAt xy ->
            case model.drag of
                Just { start, index } ->
                    Return.singleton
                        { model
                            | drag = Just <| Drag start xy index
                            , graph =
                                Graph.update index
                                    (Maybe.map <| updateNode xy)
                                    model.graph
                            , forceSim =
                                case model.forceSim of
                                    Just fSim ->
                                        Just <|
                                            Force.reheat fSim

                                    Nothing ->
                                        Nothing
                        }

                Nothing ->
                    Return.singleton
                        { model | drag = Nothing }

        DragEnd xy ->
            case model.drag of
                Just { start, index } ->
                    Return.singleton
                        { model
                            | mousePos = xy
                            , drag = Nothing
                            , graph =
                                Graph.update index
                                    (Maybe.map (updateNode xy))
                                    model.graph
                        }

                Nothing ->
                    Return.singleton
                        { model
                            | drag = Nothing
                        }

        GetViewport mp ->
            case mp of
                Just sv ->
                    Return.singleton
                        { model
                            | viewport = mp
                        }
                        |> Return.map
                            maybeStartSimulation

                Nothing ->
                    Return.singleton
                        { model | viewport = mp }

        UpdateText txt ->
            Return.singleton
                { model | textBox = txt }

        FocusHere ->
            return model focusHere

        NoOp ->
            Return.singleton model


focusHere : Cmd Msg
focusHere =
    Task.attempt (\_ -> NoOp) (Dom.focus "textBox")


updateSim : Model -> Model
updateSim model =
    case model.forceSim of
        Just forceSim ->
            let
                ( newState, list ) =
                    Graph.nodes model.graph
                        |> List.map
                            Tuple.second
                        |> List.filterMap (Maybe.map identity)
                        |> Force.tick forceSim
            in
            case model.drag of
                Nothing ->
                    { model
                        | graph =
                            List.foldr
                                (\node gr ->
                                    Graph.insertData
                                        node.id
                                        node
                                        gr
                                )
                                model.graph
                                list
                        , forceSim = Just newState
                    }

                Just { current, index } ->
                    { model
                        | drag = model.drag
                        , graph =
                            Graph.update index
                                (Maybe.map (updateNode current))
                                (List.foldr
                                    (\node gr ->
                                        Graph.insertData
                                            node.id
                                            node
                                            gr
                                    )
                                    model.graph
                                    list
                                )
                        , forceSim = Just newState
                    }

        Nothing ->
            --getViewportTask
            model


getViewportTask : String -> Cmd Msg
getViewportTask id =
    Task.attempt
        (\task ->
            case task of
                Ok value ->
                    GetViewport (Just value)

                Err _ ->
                    GetViewport Nothing
        )
    <|
        Dom.getElement id


maybeStartSimulation : Model -> Model
maybeStartSimulation model =
    case model.viewport of
        Just vp ->
            let
                toLinkRec ( a, b ) =
                    { source = a
                    , target = b
                    , distance = 75
                    , strength = Nothing
                    }

                forces =
                    [ Graph.edges graph
                        |> List.map toLinkRec
                        |> Force.customLinks 1
                    , Graph.nodes graph
                        |> List.map Tuple.first
                        |> Force.manyBodyStrength -100
                    , Force.center
                        (vp.element.width / 2)
                        (vp.element.height / 2)
                    ]
            in
            { model
                | forceSim =
                    Just <| Force.simulation forces
            }

        Nothing ->
            model


updateNode : XY -> Entity -> Entity
updateNode xy nodeData =
    { nodeData | x = xy.x, y = xy.y }


tupToXY : ( Float, Float ) -> XY
tupToXY ( x, y ) =
    { x = x, y = y }



-- Subs --


subs : Model -> Sub Msg
subs model =
    {- Sub.batch
       [ Events.onAnimationFrame Tick
       ]
    -}
    case model.drag of
        Nothing ->
            case model.forceSim of
                Nothing ->
                    Sub.none

                Just sim ->
                    if Force.isCompleted sim then
                        Sub.none

                    else
                        Events.onAnimationFrame Tick

        Just _ ->
            Events.onAnimationFrame Tick



-- View --


view : Model -> Browser.Document Msg
view model =
    (\body -> { title = "DIMS", body = [ body ] }) <|
        El.layout
            ([ El.width El.fill

             --, getExtFont "Courier Prime"
             --, getExtFont "Spline Sans Mono"
             --, getExtFont "Jetbrains Mono"
             --, getExtFont "Dosis"
             , getExtFont "Karma"
             , Bg.color model.theme.bg
             , Font.color model.theme.fg
             , Font.size <| round f
             , Font.medium
             , Font.letterSpacing 0.5 --1.25
             , Font.wordSpacing 1.5
             , El.spacing <| round <| f * s
             , El.inFront <|
                case model.menu of
                    Open ->
                        El.row
                            [ El.width El.fill
                            , El.paddingEach
                                { edges
                                    | top = round <| f * 0.5
                                    , bottom = round <| f * 0.5
                                    , left = round <| f * 0.5
                                    , right = round <| f
                                }
                            , Bg.color model.theme.settingsBar
                            ]
                            [ El.row
                                [ El.alignLeft
                                , El.spacing <| round <| f * 0.6
                                ]
                                [ el
                                    [ El.paddingEach
                                        { edges | right = round <| f * 0.5 }
                                    ]
                                  <|
                                    button model.theme.link
                                        (Just ToggleMenu)
                                        "Menu"
                                , El.row []
                                    [ El.text "Mode: "
                                    , button model.theme.link
                                        (Just ToggleTheme)
                                      <|
                                        case model.theme.label of
                                            LightMode ->
                                                "Switch to Dark Mode"

                                            DarkMode ->
                                                "Switch to Light Mode"
                                    ]
                                ]
                            , el
                                [ El.alignRight
                                , Font.color <|
                                    mix 0.5 model.theme.fg model.theme.link
                                ]
                              <|
                                El.text "Powered by the: "
                            , button model.theme.link
                                ((Browser.External >> LinkClicked >> Just)
                                    "https://github.com/thistent/dims"
                                )
                                "DIMS"
                            ]

                    Closed ->
                        el
                            [ El.padding <| round <| f * 0.5
                            ]
                        <|
                            button model.theme.link (Just ToggleMenu) "Menu"
             ]
                ++ (case model.viewport of
                        Nothing ->
                            []

                        Just vp ->
                            [ El.htmlAttribute <|
                                Mouse.onMove (.pagePos >> tupToXY >> adjustCoords vp >> DragAt)
                            , El.htmlAttribute <|
                                Mouse.onUp (.pagePos >> tupToXY >> adjustCoords vp >> DragEnd)
                            ]
                   )
            )
        <|
            El.column
                [ El.width El.fill
                ]
                [ El.textColumn
                    [ El.width <| El.px <| round <| f * 38
                    , El.height El.fill
                    , El.centerX
                    , El.paddingXY 0 (round <| f * s * 3.0)
                    , El.spacing <| round <| f * 1.5
                    ]
                    [ h1 model.theme
                        "A heading with some very long title text so that I can see if wrapping works properly"
                    , h2 model.theme
                        --"Heading Level Two"
                        "A heading with some very long title text so that I can see if wrapping works properly"
                    , h3 model.theme
                        --"Heading Level Three"
                        "A heading with some very long title text so that I can see if wrapping works properly"
                    , Input.multiline
                        [ Font.color (mix 0.1 model.theme.bg model.theme.fg)
                        , Border.widthEach
                            { edges | top = 2, bottom = 2 }
                        , Border.color <| changeAlpha 0.05 model.theme.link
                        , Bg.color (mix 0.015 model.theme.link model.theme.bg)
                        , El.spacing <| round <| f * 1.0
                        , El.paddingEach
                            { edges
                                | top = round <| f * 0.5
                                , bottom = round <| f * 0.25
                                , left = round <| f * 0.25
                                , right = round <| f * 0.25
                            }
                        , Event.onClick FocusHere
                        , El.htmlAttribute <| Html.Attributes.id "textBox"
                        ]
                        { onChange = UpdateText
                        , text = model.textBox
                        , placeholder =
                            Just <|
                                Input.placeholder
                                    [ Font.color <| changeAlpha 0.25 model.theme.link
                                    ]
                                <|
                                    El.text "Type some text!"
                        , label = Input.labelHidden "Add Text"
                        , spellcheck = False
                        }
                    , h4 model.theme
                        --"Heading Level Four"
                        "A heading with some very long title text so that I can see if wrapping works properly"
                    , svgImage model 1
                    , button model.theme.link
                        (Just ToggleMenu)
                        "Menu"
                    , h5 model.theme
                        --"Heading Level Five"
                        "A heading with some very long title text so that I can see if wrapping works properly"
                    , svgImage model 2
                    , h6 model.theme
                        --"Heading Level Six"
                        "A heading with some very long title text so that I can see if wrapping works properly"
                    , El.paragraph
                        [ El.spacing <| round <| f * 1.0
                        ]
                        [ El.text <|
                            "I'm currently working on a math expression AST and a renderer that displays the sorts of equations that I need for fitting together the type theories of "
                        , button
                            model.theme.link
                            Nothing
                            "Plutus Core"
                        , El.text <|
                            " and other languages I want to target with my language, "
                        , button
                            model.theme.link
                            Nothing
                            "Poplar"
                        , El.text "."
                        ]
                    , El.paragraph
                        [ El.spacing <| round <| f * 1.0
                        ]
                        [ El.text "Here are some equations that I'm using for typesetting purposes:"
                        ]
                    , El.textColumn
                        [ El.width El.fill
                        , Border.widthEach { edges | top = 2, bottom = 2 }
                        , Border.color <| changeAlpha 0.05 model.theme.link
                        , Bg.color (mix 0.02 model.theme.link model.theme.bg)
                        , El.padding <| round <| f * s
                        , El.spacing <| round <| f * s
                        ]
                        [ el [ El.width El.fill ] <|
                            tyvarRender f model.theme.math
                        , el [ El.width El.fill ] <|
                            tyallRender f model.theme.math
                        , el [ El.width El.fill ] <|
                            tyfixRender f model.theme.math
                        ]
                    , El.paragraph
                        [ El.spacing <| round <| f * 1.0
                        ]
                        [ El.text <|
                            "Ultimately, the goal is to build a zettlekasten-like mind-mapping experience with a simple flavor of Markdown, as well as some built-in tools for working with ASTs for things like building math equations, or mapping out timelines for a project."
                        ]
                    , El.paragraph
                        [ El.spacing <| round <| f * 1.0
                        ]
                        [ El.text <|
                            "Currently, I'm just formatting this page by hand, but eventually, documentation like this,"
                        , El.paragraph
                            [ El.spacing <| round <| f * 1.0
                            ]
                            [ El.text " or other formats like"
                            , el
                                [--Font.color model.theme.link
                                ]
                              <|
                                latex f
                            , El.text ","
                            ]
                        , El.text <|
                            " should be automatically generated from any subset of someone's \"second brain\" that can be managed by a high-performance backend running privately, in a way that links nodes together, or granting people public access. Credentials could be based on linking a wallet address, or an Ada Handle."
                        ]
                    ]
                ]


spacer : Element Msg
spacer =
    el [ El.height <| El.px <| round <| f * 1.5 ] El.none



-- Colors --


black : El.Color
black =
    El.rgb 0 0 0


white : El.Color
white =
    El.rgb 1 1 1


mix w a b =
    colorMap2 (\x y -> Manip.weightedMix x y w) a b


lightMode : ColorScheme
lightMode =
    palFun
        LightMode
        (El.rgb255 0x00 0x8B 0x8B)
        (El.rgb255 0x52 0x84 0x18)


darkMode : ColorScheme
darkMode =
    palFun
        DarkMode
        (El.rgb255 0x00 0x8B 0x8B)
        (El.rgb255 0x52 0x84 0x18)



{-
   grey =
       El.rgb 0.5 0.5 0.5


   lightGrey =
       El.rgb 0.65 0.65 0.65


   darkGrey =
       El.rgb 0.35 0.35 0.35


   pink =
       El.rgb 0.9 0.1 0.4


   berry =
       El.rgb 0.6 0 0.2
-}


palFun : Theme -> El.Color -> El.Color -> ColorScheme
palFun label color1 color2 =
    let
        dark =
            mix 0.12 color1 black

        light =
            mix 0.12 color1 white

        ( fg, bg ) =
            case label of
                LightMode ->
                    ( dark, light )

                DarkMode ->
                    ( light, dark )

        hC : Float -> El.Color
        hC i =
            if i == 1 then
                color1

            else
                color1
                    |> mix (i * 0.175) color2
                    |> mix ((i - 1) * 0.05) fg
    in
    { label = label
    , fg = fg
    , bg = bg
    , settingsBar =
        changeAlpha 0.5 <|
            mix 0.2 color1 bg
    , link = color1
    , alt = color2
    , h1 = hC 1
    , h2 = hC 2
    , h3 = hC 3
    , h4 = hC 4
    , h5 = hC 5
    , h6 = hC 6
    , math =
        { const = hC 1
        , var = hC 2
        , tyVar = hC 3
        , kVar = hC 4
        , op = hC 1
        , exp = color1
        , frac = mix 0.5 color1 bg
        , ofType = color1
        , ofKind = color1
        , pars = mix 0.6 color1 bg
        , lam = color1
        , reductionRule = mix 0.5 color1 fg
        , replace = color1
        }
    }


elToColor : El.Color -> Color
elToColor =
    El.toRgb >> Color.fromRgba


colorToEl : Color -> El.Color
colorToEl =
    Color.toRgba >> El.fromRgb


colorMap : (Color -> Color) -> El.Color -> El.Color
colorMap fun =
    elToColor >> fun >> colorToEl


colorMap2 : (Color -> Color -> Color) -> El.Color -> El.Color -> El.Color
colorMap2 fun a b =
    fun (elToColor a) (elToColor b)
        |> colorToEl



-- Document Rendering Components --


header :
    Int
    -> Float
    -> El.Color
    -> String
    -> Element Msg
header level size color str =
    El.column []
        [ spacer
        , El.row
            [ {- Bg.color <| changeAlpha 0.15 color
                 , Border.shadow
                     { offset = ( 4, 4 )
                     , size = -0.5
                     , blur = 5
                     , color = changeAlpha 0.25 black
                     }
                     ,
              -}
              El.width El.fill
            , Border.widthEach
                { edges
                    | bottom = 2
                }
            , Border.color <| changeAlpha 0.5 color
            ]
            [ El.paragraph
                [ El.width El.fill
                , El.spacing <| round <| f * size * 0.15 * toFloat level
                , Font.size <| round <| f * size
                , Font.semiBold
                , Font.color color
                , Font.wordSpacing <| f * 0.5
                , Region.heading level
                , El.paddingEach
                    { edges
                        | {- top = round <| f * size * 0.5
                             ,
                          -}
                          bottom = round <| f * size * 0.25

                        --, left = round <| f * size * 0.5
                    }
                ]
                [ El.text str ]
            , el
                [ El.alignBottom
                , El.alignRight
                , Font.bold
                , Font.color <| changeAlpha 0.5 color
                , El.paddingEach
                    { edges
                        | left = round f
                        , right = round <| f * 0.5
                    }
                ]
              <|
                El.text <|
                    "h"
                        ++ String.fromInt level
            ]
        ]


h1 : ColorScheme -> String -> Element Msg
h1 theme =
    header 1 2.5 theme.h1


h2 : ColorScheme -> String -> Element Msg
h2 theme =
    header 2 2.2 theme.h2


h3 : ColorScheme -> String -> Element Msg
h3 theme =
    header 3 1.9 theme.h3


h4 : ColorScheme -> String -> Element Msg
h4 theme =
    header 4 1.6 theme.h4


h5 : ColorScheme -> String -> Element Msg
h5 theme =
    header 5 1.3 theme.h5


h6 : ColorScheme -> String -> Element Msg
h6 theme =
    header 6 1.0 theme.h6



-- Other --


button : El.Color -> Maybe Msg -> String -> Element Msg
button color msg label =
    el [ El.paddingXY (round <| f * 0.1) 0 ] <|
        Input.button
            [ Font.bold
            , Font.color color
            , Font.size <| round <| f * 0.9
            , Bg.color <| changeAlpha 0.1 color
            , Border.width 2 --Each { edges | bottom = 1 }
            , Border.dashed
            , Border.color <| changeAlpha 0.5 color
            , Border.rounded 5
            , El.paddingEach
                { edges
                    | top = round <| f * 0.25
                    , bottom = round <| f * 0.05
                    , left = round <| f * 0.35
                    , right = round <| f * 0.35
                }
            ]
            { onPress = msg
            , label =
                El.text label
            }


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


changeAlpha : Float -> El.Color -> El.Color
changeAlpha a color =
    let
        rgbColor =
            color |> El.toRgb
    in
    { rgbColor | alpha = a } |> El.fromRgb


latex : Float -> Element Msg
latex size =
    el
        [ El.inFront <|
            El.row
                [ getExtFont "Merriweather"
                , El.width El.shrink
                , El.height El.shrink
                , El.moveUp <| size * 0.9
                , El.moveRight <| size * 0.4
                , Font.letterSpacing 0
                ]
                [ el [] <| El.text " L"
                , el
                    [ El.moveUp <| size * 0.05
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
        , El.width <| El.px <| round <| size * 3.0
        ]
        El.none


mdTextOne =
    """
# Hello, World!

Welcome to the **DIMS** page!

I'm currently working on a math expression AST and a rendering engine for working with the type theory of Plutus Core, and other languages that will be part of the research and development of my language, **Poplar**.

here are some equations from the Plutus Core Spec. that I'm using for formatting and typesetting purposes:
"""


tyvarRender fs mathTheme =
    Math.render fs mathTheme <|
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


tyallRender fs mathTheme =
    Math.render fs mathTheme <|
        Math.reductionRule "tyall"
            (Math.typeNamed "A"
                |> Math.ofKind Math.kindStar
                |> Math.impliedBy
                    (Math.var Math.alpha
                        |> Math.ofKind (Math.kindNamed "K")
                        |> Math.containedBy Math.context
                    )
            )
            (Math.forAll
                (Math.var Math.alpha)
                (Math.kindNamed "K")
                (Math.typeNamed "A")
                |> Math.pars
                |> Math.ofKind Math.kindStar
                |> Math.impliedBy Math.context
            )


tyfixRender fs mathTheme =
    Math.render fs mathTheme <|
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


svgImage : Model -> Int -> Element Msg
svgImage model i =
    el
        [ El.width El.fill
        , El.height <| El.px <| round <| f * 24
        , El.htmlAttribute <| id <| "svg-image-" ++ String.fromInt i

        --, Border.widthEach
        --    { edges | top = 1, bottom = 1 }
        --, Border.color (mix 0.2 model.theme.fg model.theme.bg)
        --, Bg.color (mix 0.02 model.theme.link model.theme.bg)
        , Border.widthEach
            { edges | top = 2, bottom = 2 }
        , Border.color <| changeAlpha 0.05 model.theme.link
        , Bg.color (mix 0.015 model.theme.link model.theme.bg)
        ]
    <|
        El.html <|
            case model.viewport of
                Nothing ->
                    Ts.svg
                        [ Ta.width <| Tt.percent 100
                        , Ta.height <| Tt.percent 100
                        ]
                    <|
                        [ Ts.text_
                            [ Ta.x (Tt.px <| f * 0.25)
                            , Ta.y (Tt.px <| f * 24 - f * 0.75)
                            , Ta.fontFamily []
                            , Ta.fontSize <| Tt.px <| f
                            , Ta.alignmentBaseline Tt.AlignmentBaseline
                            , Ta.letterSpacing "1.5"
                            , Ta.fill <|
                                Tt.Paint <|
                                    elToColor <|
                                        changeAlpha 0.2 model.theme.link
                            ]
                            [ Tc.text "Graph View" ]
                        ]

                Just vp ->
                    Ts.svg
                        [ Ta.viewBox
                            0
                            0
                            vp.element.width
                            vp.element.height
                        ]
                    <|
                        [ Graph.edges model.graph
                            |> List.map (linkElement model.theme model.graph)
                            |> Ts.g [ Ta.class [ "links" ] ]
                        , Graph.nodes model.graph
                            |> List.map
                                (nodeElement model.theme
                                    vp
                                )
                            |> Ts.g [ Ta.class [ "nodes" ] ]
                        ]



--linkElement : Graph Entity () -> Edge () -> Svg msg
--linkElement gr edge =


linkElement : ColorScheme -> Graph NodeId Entity () -> ( NodeId, NodeId ) -> Svg Msg
linkElement theme gr ( sourceId, targetId ) =
    let
        source : Entity
        source =
            let
                maybeData =
                    Graph.getData sourceId gr
            in
            case maybeData of
                Just data ->
                    data

                Nothing ->
                    emptyEntity sourceId ""

        target : Entity
        target =
            let
                maybeData =
                    Graph.getData targetId gr
            in
            case maybeData of
                Just data ->
                    data

                Nothing ->
                    emptyEntity targetId ""

        relAng : Float
        relAng =
            atan2 (target.y - source.y) (target.x - source.x)
    in
    Ts.line
        [ Ta.strokeWidth <| Tt.px 2
        , Ta.stroke <| Tt.Paint <| elToColor <| mix 0.5 theme.link theme.bg
        , Ta.x1 <| Tt.px source.x
        , Ta.y1 <| Tt.px source.y
        , Ta.x2 <| Tt.px target.x
        , Ta.y2 <| Tt.px target.y
        ]
        []


nodeElement : ColorScheme -> Dom.Element -> ( NodeId, Maybe Entity ) -> Svg Msg
nodeElement theme svgPos ( nodeId, maybeEntity ) =
    let
        node : Entity
        node =
            case maybeEntity of
                Just data ->
                    data

                Nothing ->
                    emptyEntity nodeId ""
    in
    Ts.g
        [ {- onMouseDown svgPos node.id
             ,
          -}
          Touch.onStart (touchCoordinates svgPos >> DragStart node.id)
        , Touch.onMove (touchCoordinates svgPos >> DragAt)
        , Touch.onEnd (touchCoordinates svgPos >> DragEnd)
        , Mouse.onDown (.offsetPos >> tupToXY >> DragStart node.id)
        ]
        [ Ts.circle
            [ Ta.cx <| Tt.Px node.x
            , Ta.cy <| Tt.Px node.y
            , Ta.r <| Tt.Px <| f * 1.2
            , Ta.fill <| Tt.Paint <| elToColor <| mix 0.25 theme.link theme.bg
            , Ta.stroke <| Tt.Paint <| elToColor <| mix 0.5 theme.link theme.bg
            , Ta.strokeWidth <| Tt.px 2
            ]
            []
        , Ts.title
            [ Ta.fill <| Tt.Paint <| elToColor <| theme.fg ]
            [ Tc.text <| String.fromInt node.id ]
        ]


adjustCoords : Dom.Element -> XY -> XY
adjustCoords vp pos =
    { pos
        | x = pos.x - vp.element.x
        , y = pos.y - vp.element.y
    }


touchCoordinates : Dom.Element -> Touch.Event -> XY
touchCoordinates svgPos touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map
            .pagePos
        --.screenPos
        --.clientPos
        |> Maybe.withDefault ( 0, 0 )
        |> tupToXY
        |> adjustCoords svgPos


onMouseDown : Dom.Element -> NodeId -> Html.Attribute Msg
onMouseDown svgPos nodeId =
    Mouse.onDown (.pagePos >> tupToXY >> DragStart nodeId)


stickManGraph : Graph NodeId String ()
stickManGraph =
    Graph.empty
        |> Graph.insert 0
        |> Graph.insertEdge 0 1
        |> Graph.insertEdge 1 2
        |> Graph.insertEdge 1 3
        |> Graph.insertEdge 1 4
        |> Graph.insertEdge 4 5
        |> Graph.insertEdge 4 6
