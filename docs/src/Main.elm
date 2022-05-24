port module Main exposing (..)

import AlgGraph as Ag
import Array exposing (Array)
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Nav
import Color exposing (Color)
import Color.Blending as Blend
import Color.Manipulate as Manip
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
    , delta : Float
    , mousePos : XY
    , theme : ColorScheme
    , viewport : Maybe Viewport
    , menu : Visibility
    , graph : Graph NodeId Entity ()
    , drag : Maybe Drag
    , forceSim : Maybe (Force.State NodeId)
    }


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | ToggleTheme
    | ToggleMenu
    | Tick Float
    | DragStart NodeId XY
    | DragAt XY
    | DragEnd XY
    | GetViewport (Maybe Viewport)
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
        , delta = 0
        , mousePos = { x = 0, y = 0 }
        , theme = lightMode
        , viewport = Nothing
        , menu = Closed
        , graph = graph
        , drag = Nothing
        , forceSim = Nothing
        }
        getViewportTask



{- The forces that act on nodes and links. -}
{- FIXME: Get real initial graph state, and don't render if empty -}


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

        Tick delta ->
            if model.delta < 50 then
                Return.singleton
                    (updateSim { model | delta = model.delta + delta })

            else
                return
                    (updateSim { model | delta = 0 })
                    getViewportTask

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

                                    --fSim
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

        NoOp ->
            Return.singleton model


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


getViewportTask : Cmd Msg
getViewportTask =
    Task.attempt
        (\task ->
            case task of
                Ok value ->
                    GetViewport (Just value)

                Err _ ->
                    GetViewport Nothing
        )
    <|
        Dom.getViewport


maybeStartSimulation : Model -> Model
maybeStartSimulation model =
    case model.viewport of
        Just vp ->
            let
                toLinkRec ( a, b ) =
                    { source = a
                    , target = b
                    , distance = 150
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
                        (vp.scene.width / 2)
                        (vp.scene.height / 2)
                    ]

                {- [ Force.links <|
                       Graph.edges graph
                   , Force.manyBody <|
                       List.map Tuple.first <|
                           Graph.nodes <|
                               graph
                   , Force.center (vp.viewport.width / 2)
                       (vp.viewport.height / 2)
                   ]
                -}
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
    {- case model.drag of
       Nothing ->
           case model.forceSim of
               Nothing ->
                   Sub.none

               Just sim ->
                   if Force.isCompleted sim then
                       Sub.none

                   else
                       Events.onAnimationFrameDelta Tick

       Just _ ->
    -}
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        , Events.onMouseMove
            (Decode.map
                (.offsetPos >> tupToXY >> DragAt)
                Mouse.eventDecoder
            )
        , Events.onMouseUp
            (Decode.map
                (.offsetPos >> tupToXY >> DragEnd)
                Mouse.eventDecoder
            )
        , Events.onAnimationFrameDelta Tick
        ]



-- View --


view : Model -> Browser.Document Msg
view model =
    (\body -> { title = "DIMS", body = [ body ] }) <|
        El.layout
            [ El.width El.fill
            , getExtFont "Dosis"
            , Bg.color model.theme.bg
            , Font.color model.theme.fg
            , Font.size <| round f
            , Font.medium
            , Font.letterSpacing 1.25
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

                            --<| changeAlpha 0.05 model.theme.fg
                            --, Font.color white
                            ]
                            [ El.row
                                [ El.alignLeft
                                , El.spacing <| round <| f * 0.6
                                ]
                                [ el
                                    [ El.paddingEach
                                        { edges | right = round <| f * 0.5 }

                                    --, Font.size <| round <| f * 1.8
                                    --, Font.bold
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

                            {- El.link
                               [ Font.bold
                               , Font.color model.theme.link
                               ]
                               { url = "https://github.com/thistent/dims"
                               , label =
                                   El.text "DIMS"

                               --El.text "Distributed Idea Mapping System"
                               }
                            -}
                            ]

                    Closed ->
                        el
                            [ El.padding <| round <| f * 0.5

                            --, Bg.color model.theme.settingsBar
                            ]
                        <|
                            button model.theme.link (Just ToggleMenu) "Menu"
            ]
        <|
            El.column
                [ El.width El.fill
                , El.inFront <|
                    svgImage model
                ]
                [ El.textColumn
                    [ El.width <| El.px <| round <| f * 40
                    , El.height El.fill
                    , El.centerX
                    , El.paddingXY 0 (round <| f * s * 3.0)
                    , El.spacing <| round <| f * 2.0
                    ]
                    [ h1 model.theme
                        "Heading Level One: With some very long title text so that I can see if wrapping works properly"

                    --, spacer
                    , h2 model.theme
                        "Heading Level Two"

                    --, spacer
                    , h3 model.theme
                        "Heading Level Three"
                    , Input.multiline
                        [ Bg.color (mix 0.02 model.theme.link model.theme.bg)
                        , Font.color model.theme.fg
                        , Border.color <| changeAlpha 0.1 model.theme.link
                        ]
                        { onChange = \_ -> NoOp
                        , text = ""
                        , placeholder =
                            Just <|
                                Input.placeholder
                                    [ Font.color <|
                                        changeAlpha 0.4 model.theme.link
                                    ]
                                <|
                                    El.text "Type some text!"
                        , label = Input.labelHidden "Add Text"
                        , spellcheck = False
                        }
                    , h4 model.theme
                        "Heading Level Four"
                    , h5 model.theme
                        "Heading Level Five"
                    , h6 model.theme
                        "Heading Level Six"
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
                        , Border.widthEach { edges | top = 1, bottom = 1 }
                        , Border.color model.theme.math.frac
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

                            --, el [ Font.bold ] <| El.text "LATEX"
                            , El.text ","
                            ]
                        , El.text <|
                            " should be automatically generated from any subset of someone's \"second brain\" that can be managed by a high-performance backend running privately, in a way that links nodes together, or granting people public access. Credentials could be based on linking a wallet address, or an Ada Handle."
                        ]
                    ]
                ]


spacer : Element Msg
spacer =
    el [ El.height <| El.px <| round <| f ] El.none



-- Colors --


black : El.Color
black =
    El.rgb 0 0 0


white : El.Color
white =
    El.rgb 1 1 1


mix w a b =
    colorMap2 (\x y -> Manip.weightedMix x y w) a b



{-
   lightMode : ColorScheme
   lightMode =
       palFun
           LightMode
           (mix 0.5 black pal.aqua4)
           (mix 0.7 white pal.aqua4)
           --black
           pal.aqua4


   darkMode : ColorScheme
   darkMode =
       palFun
           DarkMode
           (mix 0.6 white pal.aqua4)
           (mix 0.85 black pal.aqua4)
           pal.aqua5
-}


lightMode : ColorScheme
lightMode =
    palFun
        LightMode
        (mix 0.8 black testColor)
        (mix 0.7 white testColor)
        (colorMap (Manip.saturate 0.25) (mix 0.1 black testColor))
        (colorMap (Manip.saturate 0.25) (mix 0.1 black testBgColor))


testColor =
    --El.rgb255 0x31 0x64 0x4E
    El.rgb255 0x1E 0x76 0x84


testBgColor =
    El.rgb255 0xCB 0x4B 0x60


darkMode : ColorScheme
darkMode =
    palFun
        DarkMode
        (mix 0.7 white testColor)
        (mix 0.8 black testColor)
        (colorMap (Manip.saturate 0.25) (mix 0.4 white testColor))
        (colorMap (Manip.saturate 0.25) (mix 0.4 white testBgColor))


pink =
    El.rgb 0.9 0.1 0.4


berry =
    El.rgb 0.6 0 0.2


getSecondary : El.Color -> El.Color
getSecondary color =
    color
        |> colorMap (Manip.rotateHue 25 >> Manip.saturate 2.0)


palFun : Theme -> El.Color -> El.Color -> El.Color -> El.Color -> ColorScheme
palFun label fg bg hi alt =
    let
        dark n =
            colorMap (Manip.darken n)

        hC : Float -> El.Color
        hC i =
            if i == 1 then
                hi

            else
                hi
                    |> mix (i * 0.175) alt
                    |> mix ((i - 1) * 0.125) bg

        --|> colorMap (Manip.saturate (i * 0.9))
    in
    { label = label
    , fg = fg

    -- mix 0.5 pal.aqua5 black
    , bg = bg

    -- mix 0.5 pal.teal5 white
    , settingsBar =
        changeAlpha 0.5 <|
            mix 0.2 hi bg
    , link = hi
    , alt = alt

    -- mix 0.5 pal.aqua2 (El.rgb 0 0.75 1)
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
        , exp = hi
        , frac = mix 0.5 hi bg
        , ofType = hi
        , ofKind = hi
        , pars = mix 0.6 hi bg
        , lam = hi
        , reductionRule = mix 0.5 hi fg
        , replace = hi
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
    El.row
        [ El.width El.fill
        , Border.widthEach { edges | bottom = 1 }
        , Border.color <| changeAlpha 0.4 color
        ]
        [ El.paragraph
            [ El.width El.fill
            , Font.size <| round <| f * size
            , Font.semiBold
            , Font.color color

            --, Font.justify
            , Region.heading level
            ]
            [ El.text str ]
        , el
            [ El.alignBottom
            , El.alignRight
            , Font.bold
            , Font.color <| changeAlpha 0.5 color
            , El.paddingEach { edges | left = round f }
            ]
          <|
            El.text <|
                "level-"
                    ++ String.fromInt level
        ]


h1 : ColorScheme -> String -> Element Msg
h1 theme =
    header 1 3.0 theme.h1


h2 : ColorScheme -> String -> Element Msg
h2 theme =
    header 2 2.6 theme.h2


h3 : ColorScheme -> String -> Element Msg
h3 theme =
    header 3 2.2 theme.h3


h4 : ColorScheme -> String -> Element Msg
h4 theme =
    header 4 1.8 theme.h4


h5 : ColorScheme -> String -> Element Msg
h5 theme =
    header 5 1.4 theme.h5


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
            , El.paddingXY (round <| f * 0.35) (round <| f * 0.25)
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
                , El.moveUp <| size * 0.8
                , El.moveRight <| size * 0.2
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


svgImage : Model -> Element Msg
svgImage model =
    el
        [ El.width <| El.fill
        , El.height <| El.fill
        , Font.color model.theme.fg
        , Border.width 1
        , Border.color <| changeAlpha 0.1 model.theme.link
        , El.htmlAttribute <| id "svgNode"
        , El.alpha 0.25
        ]
    <|
        case model.viewport of
            Nothing ->
                el
                    [ El.alignLeft
                    , El.alignBottom
                    , Font.color <|
                        changeAlpha 0.4 model.theme.link
                    ]
                <|
                    el [ El.padding <| round <| f / 3 ] <|
                        El.text "Loading Graph..."

            Just vp ->
                El.html <|
                    Ts.svg
                        [ Ta.viewBox
                            0
                            0
                            vp.scene.width
                            vp.scene.height

                        --,  Ta.width <| Tt.px vp.viewport.width
                        --, Ta.height <| Tt.px vp.viewport.height
                        --, Ta.preserveAspectRatio
                        --    (Tt.Align Tt.ScaleMid Tt.ScaleMid)
                        --    Tt.Meet
                        ]
                        [ {- Ts.rect
                             [ Ts.x <| Ts.px 0
                             , Ts.y <| Ts.px 0
                             , Ts.width <| Ts.percent 100
                             , Ts.height <| Ts.percent 100
                             , Ts.fill <|
                                 Ts.Paint <|
                                     Color.rgba 1 0 0 0.2
                             ]
                             []
                             ,
                          -}
                          Graph.edges model.graph
                            |> List.map (linkElement model.theme model.graph)
                            |> Ts.g [ Ta.class [ "links" ] ]
                        , Graph.nodes model.graph
                            |> List.map
                                (nodeElement model.theme
                                    vp.viewport
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
        , Ta.stroke <| Tt.Paint <| elToColor theme.link
        , Ta.x1 <| Tt.px source.x
        , Ta.y1 <| Tt.px source.y
        , Ta.x2 <| Tt.px target.x
        , Ta.y2 <| Tt.px target.y
        ]
        []



--nodeElement : { a | id : NodeId, label : { b | x : Float, y : Float, value : String } } -> Svg Msg
--nodeElement node =
{-
   nodeElement : Dom.Element -> ( NodeId, Maybe Entity ) -> Svg Msg
   nodeElement svgPos ( nodeId, maybeEntity ) =
       Ts.circle
           [ Ts.r 2.5
           , Ts.fill <| Ts.Paint Color.black
           , Ts.stroke <| Ts.Paint <| Color.rgba 0 0 0 0
           , Ts.strokeWidth 7
           , onMouseDown nodeId
           , Ts.cx node.label.x
           , Ts.cy node.label.y
           ]
           [ title [] [ Ts.text node.label.value ] ]

      linkElement : Graph NodeId Entity () -> ( NodeId, NodeId ) -> Svg Msg
      linkElement gr ( sourceId, targetId ) =
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
                          emptyEntity sourceId <| NodeVal "" [] []

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
                          emptyEntity targetId <| NodeVal "" [] []

              arrowPath : SubPath
              arrowPath =
                  SubPath.close <|
                      Curve.linear
                          [ ( 0, 0 )
                          , ( -10, 5 )
                          , ( -5, 0 )
                          , ( -10, -5 )
                          ]

              relAng : Float
              relAng =
                  atan2 (target.y - source.y) (target.x - source.x)

              genPath : NodeId -> Shape.Arc -> Svg Msg
              genPath index arc =
                  let
                      ( xCent, yCent ) =
                          Shape.centroid
                              { arc
                                  | innerRadius = 10
                                  , outerRadius = 20
                              }

                      absMid : ( Float, Float )
                      absMid =
                          ( (source.x + target.x) / 2 + 1.75 * xCent
                          , (source.y + target.y) / 2 + 1.75 * yCent
                          )

                      pathAttribs : List (Ts.Attribute Msg)
                      pathAttribs =
                          [ List.drop index smallPieNums
                              |> List.head
                              |> Maybe.withDefault 0
                              |> (\x -> 10 * x / List.foldl (+) 0 smallPieNums)
                              |> Ts.Px
                              |> Ts.strokeWidth
                          , Color.rgba 0 0 0 0
                              |> Ts.Paint
                              |> Ts.fill
                          , Color.black
                              |> Ts.Paint
                              |> Ts.stroke
                          ]
                  in
                  Ts.g []
                      [ SubPath.element
                          (Shape.catmullRomCurveOpen 1
                              [ ( source.x, source.y )
                              , ( source.x + xCent, source.y + yCent )
                              , absMid
                              , ( target.x + xCent, target.y + yCent )
                              , ( target.x, target.y )
                              ]
                          )
                          pathAttribs
                      , SubPath.element
                          (SubPath.rotate relAng <|
                              SubPath.translate absMid <|
                                  arrowPath
                          )
                          pathAttribs
                      ]
          in
          Ts.g [] <|
              List.indexedMap genPath [ 2, 3, 5 ]
-}


nodeElement : ColorScheme -> ElementContext -> ( NodeId, Maybe Entity ) -> Svg Msg
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
        [ onMouseDown node.id
        , Touch.onStart (touchCoordinates svgPos >> DragStart node.id)
        , Touch.onMove (touchCoordinates svgPos >> DragAt)
        , Touch.onEnd (touchCoordinates svgPos >> DragEnd)
        ]
        [ Ts.circle
            [ Ta.cx <| Tt.Px node.x
            , Ta.cy <| Tt.Px node.y
            , Ta.r <| Tt.Px 30
            , Ta.fill <| Tt.Paint <| elToColor <| mix 0.5 theme.link theme.bg
            , Ta.stroke <| Tt.Paint <| elToColor theme.link
            , Ta.strokeWidth <| Tt.px 2
            ]
            []
        , Ts.title
            [ Ta.fill <| Tt.Paint <| elToColor <| theme.fg
            ]
            [ Tc.text <| String.fromInt node.id ]
        ]


touchCoordinates : ElementContext -> Touch.Event -> XY
touchCoordinates svgPos touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map
            -- FIXME: Which should be used here?
            --.clientPos
            .pagePos
        --.screenPos
        |> Maybe.withDefault ( 0, 0 )
        |> (\( x, y ) ->
                { x = x - svgPos.x, y = y - svgPos.y }
           )


onMouseDown : NodeId -> Html.Attribute Msg
onMouseDown nodeId =
    Mouse.onDown (.offsetPos >> tupToXY >> DragStart nodeId)


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
