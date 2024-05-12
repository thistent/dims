module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Color as C
import Color.Manipulate as CM
import Delay exposing (Delay, Timer)
import Ease
import El exposing (Color, Element, el)
import El.Background as Bg
import El.Border as Border
import El.Font as Font
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Http
import Json.Decode as Jd exposing (Decoder)
import Markdown.Block as Md
import Markdown.Html as MdHtml
import Markdown.Parser as Md
import Markdown.Renderer as Md
import Pane exposing (Pane, Size, SplitRenderer)
import Task
import TypedSvg as Ts
import TypedSvg.Attributes as Ta
import TypedSvg.Attributes.InPx as Tpx
import TypedSvg.Core as Tc exposing (Svg)
import TypedSvg.Filters.Attributes as Tf
import TypedSvg.Types as Tt



-- Types --


type alias Model =
    { project : Delay Project
    , window : Delay Dom.Viewport
    }


type Msg
    = Tick Float
    | WindowSize Dom.Viewport
    | GotDoc (Result Http.Error Project)


type alias Style a =
    { size : a, color : El.Color }


type alias Project =
    { title : String
    , link : String
    , ada : String
    , problem : String
    , solution : String
    }



-- Constants --


kerning : Float
kerning =
    1.2



-- Main --


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subs
        , view =
            \model ->
                { title = "Dims"
                , body =
                    [ model.window
                        |> Delay.switch
                            loadingPage
                            (view model)
                    ]
                }
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { project = Delay.wait 300 Nothing
      , window = Delay.wait 2000 Nothing
      }
    , Cmd.batch
        [ Task.perform WindowSize Dom.getViewport
        , Http.get
            { url = "notes/dims.json"
            , expect = Http.expectJson GotDoc projectDecoder
            }
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model
                | window = Delay.tick delta model.window
                , project = Delay.tick delta model.project
              }
            , Cmd.none
            )

        WindowSize viewport ->
            ( { model | window = Delay.update model.window viewport }
            , Cmd.none
            )

        GotDoc res ->
            case res of
                Ok p ->
                    ( { model | project = Delay.update model.project p }
                    , Cmd.none
                    )

                Err e ->
                    ( model
                    , Cmd.none
                    )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        ]



-- View --


view : Model -> Dom.Viewport -> Html Msg
view model vp =
    let
        size : Size
        size =
            { width = vp.viewport.width
            , height = vp.viewport.height
            }
    in
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Font.color pal.white
        , Font.size 18
        , Bg.color pal.black
        , El.inFront <|
            El.column
                [ El.alignRight
                , El.height El.fill
                ]
                [ el
                    [ Border.rounded 10
                    , Bg.color <| addAlpha 0.75 <| pal.black
                    , Font.size 20
                    , fontSpacing
                    , Font.color pal.gray
                    , El.alignRight
                    , El.paddingXY 15 10
                    ]
                  <|
                    el [ El.moveUp 1.5 ] <|
                        El.text "Ken Stanton"
                , el
                    [ El.alignRight
                    , El.alignBottom
                    , El.paddingXY 15 10
                    , Font.size 20
                    , fontSpacing
                    , Font.color pal.gray
                    , Bg.color <| addAlpha 0.75 <| pal.black
                    , Border.rounded 10
                    ]
                  <|
                    dims <|
                        {- styleMap toFloat <|
                           levelStyle 1
                        -}
                        { size = 50, color = pal.blue }
                ]
        ]
    <|
        Pane.render splitRenderer size <|
            Pane.vSplit 0.5
                (Pane.single <| innerView model)
                (Pane.single <| paneContents pal.blue)


splitRenderer : SplitRenderer (Element Msg)
splitRenderer =
    { h =
        \f size e1 e2 ->
            El.column
                [ El.width El.fill
                , El.height El.fill
                ]
                [ el
                    [ El.height <| El.px <| round <| f * size.height
                    , El.width El.fill

                    --, scrollStyle
                    , El.scrollbars

                    --, El.clip
                    ]
                  <|
                    e1
                , el
                    [ El.height El.fill
                    , El.width El.fill

                    --, scrollStyle
                    , El.scrollbars

                    --, El.clip
                    ]
                  <|
                    e2
                ]
    , v =
        \f size e1 e2 ->
            El.row
                [ El.width El.fill
                , El.height El.fill
                ]
                [ el
                    [ El.width <|
                        El.px <|
                            round <|
                                f
                                    * size.width
                    , El.height El.fill
                    , El.scrollbars
                    ]
                  <|
                    e1
                , el
                    [ El.height El.fill
                    , El.width El.fill
                    , El.scrollbars
                    ]
                  <|
                    e2
                ]
    }


paneContents : El.Color -> Size -> Element Msg
paneContents color size =
    el
        [ Bg.color pal.black
        , Font.color color
        , El.width El.fill
        , El.height El.fill
        ]
    <|
        el [ El.centerX, El.centerY ] <|
            El.text "( Empty )"


loadingPage : Timer -> Html Msg
loadingPage t =
    let
        size : Float
        size =
            50.0

        ease : Float
        ease =
            (2 * (t.original + t.current) / t.original)
                |> Ease.inOut Ease.inSine Ease.outSine
    in
    El.layout
        [ Bg.color pal.black
        , Font.color <| mix ease pal.black pal.white
        , Font.size <| round size
        , El.width El.fill
        , El.height El.fill
        ]
    <|
        El.wrappedRow
            [ El.centerX
            , El.centerY
            ]
            [ El.text "Welcome to "
            , dims
                { color = mix ease pal.black pal.blue
                , size = size
                }
            ]


dimsMd : String
dimsMd =
    """
## **Dims** : A decentralized platform for ideation and DAO experimentation with a built-in pegging mechanism for a Djed-like stable coin

(This is where progress on this project will live)

### Problem
Current funding mechanisms have the following problems:
 - Incentives don't encourage collaboration
 - Voting systems are concurrency bottlenecks
 - They don't encourage composable DAO experimentation

### Solution
An open-source graph-based ideation management tool featuring:
 - Asynchronous composable workflows
 - Flexible funding and wage stability strategies
 - Autonomy and donation-based incentives to work where needed
"""


renderMd : String -> Element Msg
renderMd str =
    str
        |> Md.parse
        |> Result.withDefault [ Md.Paragraph [ Md.Text "Oops!" ] ]
        |> Md.render markdownRenderer
        |> Result.withDefault [ El.text "Oops again!" ]
        |> (\stuff ->
                El.column
                    [ Font.color pal.white
                    , El.spacing 40
                    ]
                    stuff
           )


markdownRenderer : Md.Renderer (Element Msg)
markdownRenderer =
    { heading =
        \{ level, rawText, children } ->
            case level of
                Md.H1 ->
                    hd 1 children

                Md.H2 ->
                    hd 2 children

                Md.H3 ->
                    hd 3 children

                Md.H4 ->
                    hd 4 children

                Md.H5 ->
                    hd 5 children

                Md.H6 ->
                    hd 6 children
    , paragraph =
        \list ->
            textBlock list
    , blockQuote = \list -> El.paragraph [] list
    , html = MdHtml.oneOf []
    , text = \s -> el [] <| El.text s
    , codeSpan =
        \str ->
            El.text str
                |> el [ Font.family [ Font.monospace ] ]
    , strong = \list -> El.paragraph [ Font.bold ] list
    , emphasis = \list -> El.paragraph [ Font.italic ] list
    , strikethrough = \list -> El.paragraph [ Font.strike ] list
    , hardLineBreak = El.none
    , link =
        \{ title, destination } list ->
            El.link
                [ Font.color pal.blue
                , fontSpacing
                ]
                { url = destination
                , label =
                    El.paragraph [] <|
                        [ El.text "[[ "
                        , El.paragraph [] list
                        , El.text <| " ]]"
                        ]
                }
    , image = \img -> El.none
    , unorderedList =
        \items ->
            El.column [ El.width El.fill, El.spacing 15 ]
                (items
                    |> List.map
                        (\it ->
                            case it of
                                Md.ListItem _ xs ->
                                    El.paragraph [ El.width El.fill ] <| (el [ Font.family [ Font.monospace ] ] <| El.text " -> ") :: xs
                        )
                )
    , orderedList = \startIndex items -> El.none
    , codeBlock =
        \{ body, language } -> El.none
    , thematicBreak =
        hbar
    , table = \list -> El.none
    , tableHeader = \list -> El.none
    , tableBody = \list -> El.none
    , tableRow = \list -> El.none
    , tableCell = \maybeArg list -> El.none
    , tableHeaderCell = \maybeArg list -> El.none
    }


innerView : Model -> Size -> Element Msg
innerView model _ =
    let
        l =
            levelStyle 1
    in
    el
        [ Bg.color pal.black
        , Font.color pal.white
        , Font.size 18
        , El.width El.fill
        ]
    <|
        El.column
            [ El.padding 40
            , El.spacing 40
            , El.height El.fill
            , El.width <| El.maximum 1080 <| El.fill
            ]
            [ renderMd dimsMd
            , hbar

            --, Delay.payload model.project
            --    |> Maybe.map propCard
            --    |> Maybe.withDefault El.none
            , El.link
                []
                { url = "https://github.com/thistent/dims/"
                , label =
                    el
                        [ Font.color pal.blue
                        , fontSpacing
                        ]
                    <|
                        El.text "[[ View source on GitHub ]]"
                }
            ]


fontSpacing : El.Attribute Msg
fontSpacing =
    El.batch
        [ Font.letterSpacing kerning
        , Font.wordSpacing <| 4 * kerning

        --, Font.justify
        ]


textBlock : List (Element Msg) -> Element Msg
textBlock ls =
    El.paragraph
        [ fontSpacing
        , El.spacing 15
        ]
        ls


styleMap : (a -> b) -> Style a -> Style b
styleMap f sty =
    { size = f sty.size
    , color = sty.color
    }


addAlpha : Float -> El.Color -> El.Color
addAlpha alpha color =
    let
        addA c =
            { c | alpha = alpha }
    in
    color |> El.toRgb |> addA |> El.fromRgb


hd : Int -> List (Element Msg) -> Element Msg
hd lev ls =
    let
        sty =
            levelStyle lev
    in
    El.paragraph
        [ Font.size sty.size
        , Font.color sty.color
        , fontSpacing
        , El.spacingXY 0 15
        ]
        ls


levelStyle : Int -> Style Int
levelStyle lev =
    let
        level : Int
        level =
            clamp 1 6 lev

        size : Int
        size =
            18 + (7 - level) * 4
    in
    case level of
        1 ->
            Style size pal.darkRed

        2 ->
            Style size pal.redOrange

        3 ->
            Style size pal.orange

        4 ->
            Style size pal.yellow

        5 ->
            Style size pal.lightGreen

        6 ->
            Style size pal.green

        _ ->
            Style size pal.white


hbar : Element Msg
hbar =
    el
        [ El.height <| El.px 1
        , El.width El.fill
        , Border.widthEach { edges | bottom = 1 }
        , Border.color pal.gray
        ]
    <|
        El.none


propCard : Project -> Element Msg
propCard proj =
    El.column
        [ El.width El.fill
        , El.spacing 40
        ]
        [ hd 4 [ El.text proj.title ]
        , El.row
            [ El.width El.fill ]
            [ El.link
                [ Font.color pal.blue
                , fontSpacing
                ]
                { url = proj.link
                , label = El.paragraph [] [ El.text "[[ See on IdeaScale ]]" ]
                }
            , el
                [ El.alignRight
                , Font.color pal.green
                ]
              <|
                El.text proj.ada
            ]
        , El.column [ El.spacing 40 ]
            [ hd 5 [ El.text "Problem:" ]
            , textBlock
                [ El.text proj.problem ]
            ]
        , El.column [ El.spacing 40 ]
            [ hd 5 [ El.text "Solution:" ]
            , textBlock
                [ El.text proj.solution ]
            ]
        , hbar
        ]



{-
   dims : Float -> Element Msg
   dims fs =
       El.image
           [ El.height <| El.px <| round <| fs * 0.8
           , El.moveUp <| fs * 0.007
           , El.alpha 0.65
           ]
           { src = "assets/dims.svg"
           , description = "dims"
           }
-}


projectDecoder : Decoder Project
projectDecoder =
    Jd.map5 Project
        (Jd.field "title" Jd.string)
        (Jd.field "link" Jd.string)
        (Jd.field "ada" Jd.string)
        (Jd.field "problem" Jd.string)
        (Jd.field "solution" Jd.string)


edges =
    { top = 0
    , bottom = 0
    , right = 0
    , left = 0
    }



-- Projects --


malawi : Project
malawi =
    { title = "Research: Strategically Competing with Mobile Money Markets in Malawi"
    , link = "https://cardano.ideascale.com/c/idea/106578"
    , ada = "₳15,000"
    , problem = "Mobile money is commonly used in southeast Africa. Cardano hasn't yet disrupted this market. There are untapped opportunities to improve people's ability to move money as well as Cardano's reach!"
    , solution = "    I see these transactions all the time. My solution is a local survey & detailed report about how Cardano wallet providers could work to make the switch to a Cardano-based solution as easy as possible."
    }


dao : Project
dao =
    { title = "Research: Real DAOs and Optimizing Governance for Parallel Experimentation"
    , link = "https://cardano.ideascale.com/c/idea/105979"
    , ada = "₳20,000"
    , problem = "Current DAOs don't really focus much on Autonomy. In a sense, DAOs aren't that Decentralized because they pool funds together and require majority votes to allocate them. Is mob rule even Organized???"
    , solution = "Are current DAOs are really DAOs? The goal is to produce detailed research and documentation of what it might mean to autonomously organize in a truly decentralized way! Can we maximize experiments???"
    }


dreps : Project
dreps =
    { title = "Research: Unique Pseudonymous Identification of DReps through Joint Content Creation"
    , link = "https://cardano.ideascale.com/c/idea/105563"
    , ada = "₳20,000"
    , problem = "Allowing DReps to remain anonymous has value, but there are some dangers to fair governance if very large whales decide to game the system. DReps should be encouraged to consider diverse perspectives."
    , solution = "                  Research detailing how community discussions can not only help DReps broaden their perspectives, but also be used to validate DReps as unique individuals, making things like quadratic voting possible."
    }



-- Svg Images --


dims : Style Float -> Element Msg
dims sty =
    let
        scaleFactor : Float
        scaleFactor =
            0.8
    in
    el
        [ El.paddingXY 4 0
        , El.centerY
        ]
    <|
        El.html <|
            Ts.svg
                [ Ta.viewBox 0 0 79.374839 18.521
                , Ta.width <| Tt.px <| sty.size * (3.0 / 0.7) * scaleFactor
                , Ta.height <| Tt.px <| sty.size * scaleFactor
                , Ta.preserveAspectRatio
                    (Tt.Align Tt.ScaleMid Tt.ScaleMid)
                    Tt.Slice
                ]
                [ Ts.path
                    [ Ta.d dimsData
                    , Ta.fill <|
                        Tt.Paint <|
                            elToColor sty.color

                    -- elToColor color
                    , Ta.transform
                        [ Tt.Matrix
                            1.0074119
                            0
                            0
                            1.0074119
                            10.866723
                            -26.487442
                        ]
                    ]
                    []
                ]


poplar : Float -> El.Color -> Element Msg
poplar f color =
    let
        scaleFactor : Float
        scaleFactor =
            0.98

        w : Float
        w =
            f * (2.0 / 0.7) * scaleFactor

        h : Float
        h =
            f * scaleFactor
    in
    el
        [ El.paddingXY 4 0
        , El.moveDown 3
        ]
    <|
        El.html <|
            Ts.svg
                [ Ta.viewBox 0 0 52.917 18.521
                , Ta.width <| Tt.px w
                , Ta.height <| Tt.px h
                , Ta.preserveAspectRatio
                    (Tt.Align Tt.ScaleMid Tt.ScaleMid)
                    Tt.Slice
                ]
                [ {- Ts.rect
                     [ Ta.x (Tt.px 0)
                     , Ta.y (Tt.px 0)
                     , Ta.width (Tt.px w)
                     , Ta.height (Tt.px h)
                     , Ta.fill <|
                         Tt.Paint <|
                             C.rgb 0 0.3 0.6
                     ]
                     []
                     ,
                  -}
                  Ts.path
                    [ Ta.d poplarData
                    , Ta.fill <|
                        Tt.Paint <|
                            elToColor color
                    , Ta.transform
                        [ Tt.Translate 0 0 ]

                    -- elToColor color
                    {- , Ta.transform
                       [ Tt.Matrix
                           1.0074119
                           0
                           0
                           1.0074119
                           10.866723
                           -26.487442
                           ]
                    -}
                    ]
                    []
                ]


elToColor : Color -> C.Color
elToColor =
    El.toRgb >> C.fromRgba


colorToEl : C.Color -> Color
colorToEl =
    C.toRgba >> El.fromRgb



-- Color Scheme --
{- pal =
   { black = El.rgb255 0x09 0x0A 0x0D
   , dark0 = El.rgb255 0x20 0x1A 0x23 -- #201a23
   , red1 = El.rgb255 0x33 0x21 0x29 -- #332129
   , blue1 = El.rgb255 0x29 0x32 0x47 -- #293247
   , red2 = El.rgb255 0x5B 0x2A 0x32 -- #5b2a32
   , brown3 = El.rgb255 0x7A 0x41 0x3B -- #7a413b
   , brown4 = El.rgb255 0xA3 0x67 0x49 -- #a36749
   , blue2 = El.rgb255 0x2B 0x46 0x5D -- #2b465d
   , green3 = El.rgb255 0x4D 0x68 0x3F -- #4d683f
   , blue4 = El.rgb255 0x44 0x71 0x75 -- #447175
   , pink5 = El.rgb255 0xA5 0x7A 0x80 -- #a57a80
   , green5 = El.rgb255 0x8B 0xA3 0x5C -- #8ba35c
   , brown6 = El.rgb255 0xC5 0x96 0x60 -- #c59660
   , blue6 = El.rgb255 0x97 0xBA 0x9E -- #97ba9e
   , tan7 = El.rgb255 0xCE 0xC0 0x98 -- #cec098
   , yellow7 = El.rgb255 0xEB 0xDA 0x89 -- #ebda89
   , light7 = El.rgb255 0xF1 0xEA 0xC3 -- #f1eac3
   }
-}


pal =
    let
        bl =
            -- #0c0e13
            El.rgb255 0x0C 0x0E 0x13

        wh =
            -- #c0d3d3
            El.rgb255 0xC0 0xD3 0xD3
    in
    { black =
        bl
    , white =
        wh
    , green =
        -- #18b800
        El.rgb255 0x18 0xB8 0x00
    , blue =
        -- #229ddf
        El.rgb255 0x22 0x9D 0xDF
    , cursorDark =
        -- #005566
        El.rgb255 0x00 0x55 0x66
    , cursorLight =
        -- #00f094
        El.rgb255 0x00 0xF0 0x94
    , lightGreen =
        -- #abd600
        El.rgb255 0xAB 0xD6 0x00
    , gray =
        mix 0.4 bl wh
    , cyan =
        -- #0eeef8
        El.rgb255 0x0E 0xEE 0xF8
    , yellow =
        -- #fed167
        El.rgb255 0xFE 0xD1 0x67
    , orange =
        -- #ff9d47
        El.rgb255 0xFF 0x9D 0x47
    , pink =
        -- #ff6873
        El.rgb255 0xFF 0x68 0x73
    , redOrange =
        -- #dd581d
        El.rgb255 0xDD 0x58 0x1D
    , darkRed =
        -- #880011
        El.rgb255 0x88 0x00 0x11
    , berry =
        -- #9f33dd
        El.rgb255 0x9F 0x33 0xDD
    }


mix n a b =
    map2ColorFun (\x y -> CM.weightedMix y x n) a b


map2ColorFun : (C.Color -> C.Color -> C.Color) -> Color -> Color -> Color
map2ColorFun f c1 c2 =
    let
        cc1 =
            c1 |> El.toRgb >> C.fromRgba

        cc2 =
            c2 |> El.toRgb >> C.fromRgba
    in
    f cc1 cc2 |> C.toRgba >> El.fromRgb


scrollStyle : El.Attribute Msg
scrollStyle =
    El.batch
        [ style "overflow" "scroll"
        , style "scrollbar-color" "red orange"
        , style "scrollbar-width" "thin"
        ]


style : String -> String -> El.Attribute Msg
style s t =
    El.htmlAttribute <| Attr.style s t



{-

   oldPal =
       { -- #090a0d
         black = El.rgb255 0x09 0x0A 0x0D
       , -- #161d32
         night = El.rgb255 0x16 0x1D 0x32
       , -- #253d60
         dusk = El.rgb255 0x25 0x3D 0x60
       , -- #2e7ba8
         day = El.rgb255 0x2E 0x7B 0xA8
       , -- #51f7e3
         cyan = El.rgb255 0x51 0xF7 0xE3
       , -- #76ad7d
         moss = El.rgb255 0x76 0xAD 0x7D
       , -- #c5ed7f
         lime = El.rgb255 0xC5 0xED 0x7F
       , -- #421e2c
         grape = El.rgb255 0x42 0x1E 0x2C
       , -- #7d2d51
         berry = El.rgb255 0x7D 0x2D 0x51
       , -- #a76dbd
         plum = El.rgb255 0xA7 0x6D 0xBD
       , -- #ed7acb
         pink = El.rgb255 0xED 0x7A 0xCB
       , -- #c42840
         red = El.rgb255 0x42 0x28 0x40
       , -- #d66300
         rust = El.rgb255 0xD6 0x63 0x00
       , -- #efae40
         lemon = El.rgb255 0xEF 0xAE 0x40
       , -- #ffe1a3
         sand = El.rgb255 0xFF 0xE1 0xA3
       , -- #fff3d6
         white = El.rgb255 0xFF 0xF3 0xD6
       }
-}
-- Dims Path Data --


dimsData : String
dimsData =
    "m 1.8226194,30.630173 2e-6,4.65e-4 c -0.02689,-1.18e-4 -0.05387,6.18e-4 -0.08083,0.0021 -0.01098,5.43e-4 -0.02208,0.0019 -0.03318,0.0029 -0.04499,-5.71e-4 -0.08985,-2.13e-4 -0.134565,8.83e-4 -0.83853399,0.0054 -1.47805999,0.925932 -1.28513999,1.715114 0.01007,0.04641 0.02281,0.09186 0.03782,0.136311 -0.09678,0.153373 -0.22416,0.287313 -0.334575,0.432316 -0.07144,0.08878 -0.137745,0.183398 -0.191609,0.284222 -0.06021,0.103607 -0.106904,0.214142 -0.1279,0.333545 -0.01038,0.04653 -0.01976,0.0952 -0.03085,0.143076 -0.0059,0.02294 -0.01202,0.04578 -0.01802,0.06873 -0.02078,0.06865 -0.05052,0.133305 -0.101272,0.186276 -0.03395,0.03204 -0.07467,0.05189 -0.1184863,0.06159 -0.0048,9.4e-4 -0.0093,0.0018 -0.01402,0.0028 -0.09989,0.01695 -0.211134,-0.01868 -0.281538,-0.09164 -0.01841,-0.02557 -0.03678,-0.05105 -0.05537,-0.07645 -0.0406,-0.05869 -0.08069,-0.118117 -0.12064401,-0.177189 -0.08596,-0.128258 -0.163458,-0.271029 -0.197581,-0.422696 -0.01045,-0.05213 -0.01588,-0.105335 -0.01451,-0.159745 7.64e-4,-0.04273 0.0057,-0.08502 0.01361,-0.126669 0.180704,-0.2104 0.29984401,-0.471273 0.31858301,-0.756505 0.07661,-0.762987 -0.55836701,-1.563142 -1.35101301,-1.540437 -0.05255,-0.0027 -0.104896,-1.42e-4 -0.157455,4.89e-4 -0.729076,0.0058 -1.322186,0.696447 -1.308897,1.408647 -0.0306,0.757612 0.665401,1.435983 1.419192,1.403744 0.118199,-0.0016 0.235458,-0.01948 0.34886,-0.05109 0.01498,0.0043 0.03002,0.0091 0.04537,0.0148 0.06508,0.02739 0.122055,0.06719 0.173713,0.114394 0.02195,0.02574 0.04202,0.05291 0.05867,0.08159 0.08545,0.125763 0.17776,0.251416 0.252128,0.384678 0.03229,0.06696 0.06297,0.13503 0.09317,0.202949 0.03385,0.130956 -0.05478,0.267054 -0.17561,0.315948 -0.04785,0.02186 -0.100717,0.02992 -0.152714,0.02571 -0.02339,-0.0044 -0.04715,-0.0092 -0.0706,-0.01379 -0.0085,-0.0025 -0.01672,-0.0049 -0.02479,-0.0084 -0.333299,-0.06278 -0.677727,-0.117047 -1.016003,-0.07107 -0.0031,5.19e-4 -0.0061,4.31e-4 -0.0094,9.11e-4 -0.02452,0.0035 -0.04889,0.0072 -0.07332,0.0119 l -4.75e-4,7e-6 c -0.159786,0.02904 -0.318806,0.07019 -0.481543,0.04267 -0.08919,-0.01727 -0.175699,-0.04443 -0.261423,-0.07486 -0.177656,-0.120377 -0.382551,-0.198108 -0.596908,-0.221451 -0.02893,-0.0075 -0.05791,-0.01438 -0.08743,-0.01983 -0.06128,-0.01071 -0.12254,-0.01247 -0.183652,-0.0086 -0.06992,6.96e-4 -0.139688,0.0089 -0.207841,0.02589 -0.08469,0.02078 -0.168194,0.04852 -0.248784,0.08208 -0.08935,0.03319 -0.173861,0.07703 -0.252835,0.129756 -0.204519,0.133574 -0.3653,0.326806 -0.469714,0.547212 -4.04e-4,7.19e-4 -8.19e-4,0.0018 -0.0014,0.0028 -0.0112,0.02325 -0.02169,0.04678 -0.03155,0.07065 -0.0048,0.01194 -0.0094,0.02382 -0.0139,0.03602 -0.01694,0.04469 -0.03176,0.09035 -0.04395,0.136571 -0.04531,0.145702 -0.06493,0.3 -0.05275,0.457595 0.01923,0.67367 0.572541,1.338552 1.274218,1.336961 0.428686,0.05009 0.885196,-0.08247 1.17051,-0.418142 0.246585,-0.260836 0.376086,-0.615976 0.377725,-0.970247 1.99e-4,-0.0018 2.16e-4,-0.0034 4.62e-4,-0.0052 0.02048,-0.115475 0.04155,-0.231174 0.07414,-0.344126 0.02428,-0.06388 0.07301,-0.112614 0.129898,-0.148054 0.03226,-0.01593 0.06495,-0.03084 0.09845,-0.04285 0.03149,-0.01089 0.06408,-0.01898 0.09664,-0.02599 0.1335,-0.02408 0.273259,-0.02249 0.406047,0.0072 l 4.68e-4,4.67e-4 4.68e-4,-3e-6 c 0.0073,0.0018 0.01429,0.0038 0.02151,0.0055 0.12205,0.03383 0.236193,0.09322 0.33089,0.176972 -0.02051,0.102635 -0.03082,0.207063 -0.0308,0.31174 0.0026,0.683059 0.44126,1.288096 1.089677,1.502881 0.01323,0.08596 0.01136,0.18029 -0.007,0.266352 -4.91e-4,0.0035 -0.0018,0.0069 -0.0029,0.01026 -0.02726,0.09468 -0.06266,0.187683 -0.109396,0.274613 -0.0419,0.06814 -0.09381,0.129705 -0.157371,0.178965 -0.10293,0.07848 -0.229568,0.113215 -0.357082,0.112334 -0.01597,-0.0034 -0.03205,-0.0052 -0.04769,-0.01014 -0.08449,-0.01787 -0.165419,-0.0467 -0.245541,-0.07818 -0.06832,-0.03387 -0.138813,-0.06277 -0.211468,-0.08578 -0.103522,-0.04924 -0.211222,-0.08731 -0.327804,-0.08538 -0.0114,-9.07e-4 -0.02307,-0.001 -0.03457,-0.0017 -0.186113,-0.01771 -0.375262,0.0093 -0.552518,0.0709 -0.166394,0.05386 -0.323237,0.136977 -0.458033,0.250018 -0.169914,0.138585 -0.303126,0.321284 -0.382407,0.525447 -0.02757,0.06834 -0.04964,0.138787 -0.06381,0.211867 -0.03784,0.183427 -0.034,0.372873 0.005,0.55506 5.51e-4,0.0034 8.07e-4,0.0089 0.0014,0.01214 0.07084,0.58373 0.570049,1.054035 1.119575,1.207071 0.510177,0.07085 1.08438,-0.09316 1.394022,-0.52678 0.259666,-0.314468 0.351908,-0.741771 0.279557,-1.136739 0.0129,-0.07437 0.03842,-0.145754 0.07268,-0.213287 0.09912,-0.172354 0.240626,-0.318006 0.3615383,-0.476791 0.07148,-0.08883 0.137732,-0.183287 0.191607,-0.284223 0.06012,-0.103525 0.106929,-0.213782 0.127902,-0.333078 0.01068,-0.04677 0.01962,-0.09546 0.03084,-0.143543 0.0059,-0.02259 0.01214,-0.04521 0.018,-0.0678 0.01824,-0.06033 0.04298,-0.117633 0.08311,-0.166598 0.10144,-0.01497 0.201184,-0.03968 0.297859,-0.07382 0.05094,0.01567 0.09802,0.04317 0.134357,0.08088 0.01814,0.02526 0.03651,0.05042 0.05491,0.07552 0.04079,0.05894 0.08048,0.118767 0.120638,0.178098 0.08599,0.128259 0.163921,0.271019 0.198045,0.422697 0.01048,0.0521 0.01588,0.105334 0.01454,0.159743 -3.64e-4,0.01512 -0.0012,0.03095 -0.0027,0.04625 -0.331654,0.296153 -0.531069,0.742058 -0.466268,1.19246 0.07742,0.715837 0.764621,1.370187 1.506168,1.260086 0.357809,-0.02249 0.702524,-0.181841 0.929278,-0.463164 0.03328,-0.03204 0.0635,-0.06598 0.09266,-0.10121 0.181198,-0.189235 0.310403,-0.427938 0.353397,-0.687958 0.07673,-0.461811 -0.116526,-0.959574 -0.491727,-1.241763 -0.288736,-0.227059 -0.661461,-0.347964 -1.028882,-0.320035 -0.07782,0.0012 -0.145218,-0.0095 -0.219719,-0.0376 -0.06511,-0.02742 -0.122512,-0.06762 -0.174178,-0.114855 -0.02195,-0.02573 -0.04202,-0.05294 -0.05869,-0.0816 -0.08539,-0.125652 -0.176856,-0.251067 -0.251188,-0.384204 -0.03229,-0.06703 -0.06341,-0.134943 -0.09363,-0.202949 -0.03391,-0.130981 0.05476,-0.267043 0.175619,-0.315927 0.04784,-0.02185 0.100715,-0.02993 0.152713,-0.0257 0.02342,0.0045 0.04666,0.0088 0.07011,0.01334 0.0084,0.0025 0.01673,0.0055 0.02481,0.0087 0.333327,0.06279 0.678177,0.117539 1.016467,0.07154 0.0031,-5.1e-4 0.006,-9.36e-4 0.0088,-0.0015 0.02628,-0.0039 0.05272,-0.0084 0.07892,-0.01328 0.103098,-0.01883 0.205932,-0.04169 0.30963,-0.04817 0.244197,0.287431 0.595633,0.478347 0.98501,0.468329 0.452327,0.04113 0.923184,-0.135256 1.1977963,-0.504668 0.0081,-0.01083 0.01548,-0.0219 0.02329,-0.03275 0.112614,-0.112353 0.204874,-0.245125 0.27257,-0.389598 0.0107,-0.02226 0.02073,-0.04501 0.03016,-0.06785 0.0049,-0.01222 0.0095,-0.02419 0.01391,-0.03648 0.103587,-0.27361 0.120053,-0.581299 0.02708,-0.85974 -0.131674,-0.414646 -0.449477,-0.773576 -0.8611963,-0.924208 -0.370301,-0.140769 -0.803611,-0.132725 -1.157179,0.05085 l -0.005,0.0028 c -0.01344,0.007 -0.02643,0.01445 -0.03965,0.02209 -0.01474,0.0085 -0.02977,0.01685 -0.04431,0.02584 -0.0011,7.41e-4 -0.0025,0.0018 -0.0045,0.0025 -0.0011,7.4e-4 -0.0027,0.0019 -0.0039,0.0029 -0.106143,0.06669 -0.202111,0.149175 -0.28472,0.24339 -0.313264,0.272025 -0.514132,0.674367 -0.497413,1.097711 10e-4,0.07406 0.0087,0.147658 0.02215,0.220476 -0.02348,0.02773 -0.05194,0.05146 -0.08295,0.07081 -0.03229,0.01587 -0.0649,0.03037 -0.09845,0.04237 -0.0313,0.01087 -0.06333,0.01946 -0.09571,0.02646 -0.133729,0.02416 -0.273501,0.02209 -0.406525,-0.0077 -1.99e-4,-4.1e-5 -8.9e-4,3e-6 -9.02e-4,3e-6 -0.0072,-0.0017 -0.01428,-0.0038 -0.02151,-0.0054 -0.140586,-0.03898 -0.270326,-0.112193 -0.372169,-0.217056 0.0086,-0.06772 0.01305,-0.135946 0.01293,-0.204226 -0.002,-0.651791 -0.401626,-1.236335 -1.008284,-1.474663 -0.03432,-0.107179 -0.04129,-0.242239 -0.01574,-0.362053 4.5e-4,-0.0035 0.0016,-0.0069 0.0028,-0.01028 0.02726,-0.09473 0.06354,-0.1881 0.110336,-0.275083 0.04192,-0.06803 0.09342,-0.129286 0.156925,-0.178481 0.07756,-0.05918 0.168502,-0.0936 0.263184,-0.106432 0.338369,0.257323 0.786395,0.373621 1.204911,0.264854 0.02954,-0.0069 0.05895,-0.01485 0.08776,-0.02364 0.07519,-0.01302 0.149516,-0.03214 0.221755,-0.05723 0.166076,-0.05387 0.321604,-0.137162 0.456173,-0.250021 0.169916,-0.138575 0.304074,-0.320833 0.383338,-0.524993 0.02756,-0.06836 0.04917,-0.139312 0.06336,-0.212325 0.0885,-0.42813 -0.04786,-0.88961 -0.338673,-1.213288 -0.187088,-0.212288 -0.436136,-0.37003 -0.70787,-0.446311 -0.06303,-0.01799 -0.127306,-0.03122 -0.19263,-0.03958 -0.0011,-1.25e-4 -0.0016,-1.4e-4 -0.0024,-3.94e-4 -0.01511,-0.0019 -0.03059,-0.003 -0.04579,-0.0045 -0.01477,-0.0016 -0.02947,-0.0033 -0.04438,-0.0041 -0.0017,-6.1e-5 -0.0033,-4.99e-4 -0.0052,-4.91e-4 -0.0045,-1.61e-4 -0.0089,2.31e-4 -0.01308,1.5e-5 -0.01744,-6.62e-4 -0.03476,-0.002 -0.05233,-0.0023 z m -4.0097593,2.232593 c -1.029344,0.183694 -1.370906,-0.957346 -0.622553,-1.533842 0.09783,-0.07536 0.229534,-0.104396 0.425788,-0.09732 0.985785,0.03552 1.222819,1.448057 0.196765,1.631162 z m 3.9125693,-1.628147 c 0.02487,-3.24e-4 0.04952,3.12e-4 0.07428,0.0023 0.111342,0.0076 0.211835,0.03237 0.31516,0.07844 0.152428,0.06829 0.283736,0.184029 0.365962,0.324949 0.05736,0.102297 0.09179,0.216508 0.09448,0.333784 0.0098,0.180082 -0.04623,0.364861 -0.153038,0.513047 -0.152637,0.187143 -0.391308,0.302327 -0.63346,0.331405 -0.0034,4.25e-4 -0.0068,3.8e-4 -0.01027,9.4e-4 -0.180933,0.0124 -0.364972,-0.0263 -0.514861,-0.12918 -0.195712,-0.132038 -0.31606999,-0.354588 -0.34105299,-0.583442 -0.0142,-0.226456 0.07321,-0.461038 0.22883299,-0.625885 0.09558,-0.08931 0.213551,-0.156853 0.339056,-0.19684 0.07779,-0.02563 0.155884,-0.0418 0.234862,-0.04933 z m -2.05757199,3.477114 c 0.02686,-3.08e-4 0.05317,5.61e-4 0.07989,0.0026 0.120194,0.0081 0.22887,0.03472 0.340419,0.08443 0.164554,0.07372 0.306251,0.198901 0.394994,0.351046 0.06191,0.110445 0.09912,0.233326 0.102058,0.359914 0.0103,0.194403 -0.04974,0.394239 -0.165052,0.554193 -0.164769,0.202022 -0.422901,0.325889 -0.684307,0.357268 -0.0039,2.66e-4 -0.0076,6.93e-4 -0.01112,0.0014 -0.195319,0.01346 -0.3932813,-0.02834 -0.5550783,-0.139386 C -1.0416249,36.14069 -1.1716909,35.90021 -1.1986659,35.653159 c -0.01539,-0.244466 0.0789,-0.497508 0.24689601,-0.675467 0.10321,-0.0964 0.230152,-0.169652 0.365651,-0.212817 0.08402,-0.02769 0.1687303,-0.045 0.2540053,-0.05313 z m 4.22542799,0.07635 c 0.15694,0.002 0.312561,0.0449 0.441038,0.135038 0.273994,0.197394 0.3989143,0.562071 0.316776,0.882073 -0.004,0.01723 -0.01,0.03408 -0.01528,0.05099 -0.05892,0.150874 -0.162113,0.285918 -0.296909,0.379859 l -4.68e-4,10e-7 c -0.01514,0.0092 -0.03071,0.01821 -0.04661,0.02634 -0.148722,0.07954 -0.31863,0.114089 -0.487425,0.117875 -0.13389,-0.0095 -0.266083,-0.0495 -0.380704,-0.117942 -0.145137,-0.08275 -0.267189,-0.211787 -0.338942,-0.360571 -0.03011,-0.07252 -0.04914,-0.149201 -0.05303,-0.227837 -0.01619,-0.175612 0.03072,-0.354302 0.126437,-0.503156 0.09265,-0.132951 0.224074,-0.241289 0.377027,-0.301655 0.112565,-0.04938 0.235093,-0.07453 0.358108,-0.08102 z m -8.2548973,0.05459 h 4.67e-4 c 0.133941,0.0095 0.265583,0.04946 0.380242,0.117943 0.145187,0.08281 0.267676,0.211705 0.339424,0.36057 0.03009,0.07245 0.04913,0.149259 0.05305,0.227838 0.01621,0.175683 -0.03108,0.354277 -0.126901,0.503155 -0.09265,0.132873 -0.223751,0.241241 -0.376572,0.301625 -0.112603,0.04942 -0.235043,0.07454 -0.358108,0.08101 -0.156873,-0.0016 -0.312611,-0.04492 -0.441036,-0.135018 -0.273991,-0.197398 -0.398919,-0.56207 -0.316773,-0.882073 0.004,-0.01716 0.0099,-0.03414 0.01526,-0.05098 0.05891,-0.150921 0.161606,-0.285422 0.296424,-0.379404 0.01544,-0.0093 0.03094,-0.01858 0.04711,-0.02678 0.148701,-0.07949 0.318668,-0.114069 0.487427,-0.117892 z m 6.1204363,3.423856 2e-6,4.72e-4 4.68e-4,-2e-6 c 0.212841,0.0043 0.424061,0.08332 0.58568,0.221026 0.0332,0.02813 0.06388,0.05936 0.09189,0.0922 0.109226,0.135524 0.176062,0.302945 0.184661,0.475525 0.0073,0.107093 -0.01013,0.214415 -0.04665,0.315541 -0.07242,0.187479 -0.213463,0.349607 -0.394321,0.449335 -0.107288,0.06119 -0.229526,0.09098 -0.352447,0.102975 -0.224475,0.01414 -0.45373,-0.056 -0.629059,-0.196587 -0.0079,-0.0067 -0.01531,-0.01331 -0.02293,-0.02002 -0.06115,-0.05512 -0.115157,-0.117551 -0.158993,-0.186392 -5.72e-4,-0.0012 -0.0017,-0.0017 -0.0023,-0.0034 -0.01,-0.01511 -0.01925,-0.03204 -0.02821,-0.0485 -0.0035,-0.0067 -0.007,-0.0138 -0.01082,-0.02099 -0.004,-0.0067 -0.007,-0.01505 -0.01081,-0.02241 -0.0303,-0.06549 -0.05246,-0.13446 -0.06511,-0.204907 -0.0012,-0.005 -0.0017,-0.01019 -0.0029,-0.0168 -0.0024,-0.01344 -0.0044,-0.0276 -0.006,-0.04158 -0.0012,-0.0086 -8.69e-4,-0.01704 -0.0014,-0.02474 -0.0011,-0.01343 -0.0018,-0.02642 -0.0026,-0.03972 -0.002,-0.11181 0.02243,-0.222582 0.06812,-0.324937 0.0056,-0.01178 0.01091,-0.02398 0.01718,-0.03559 0.0018,-0.0033 0.0039,-0.0068 0.0055,-0.01028 0.118915,-0.216711 0.335788,-0.378251 0.579851,-0.441035 0.06618,-0.01366 0.133901,-0.02082 0.201318,-0.01923 z m -4.0714383,0.02876 c 0.164929,-0.0023 0.328821,0.03793 0.465325,0.131689 0.195783,0.132047 0.316147,0.354481 0.341058,0.583437 0.01417,0.226431 -0.07274,0.460608 -0.228377,0.625419 -0.09558,0.08926 -0.213599,0.157391 -0.339054,0.197315 -0.07781,0.02563 -0.155885,0.04138 -0.234865,0.04889 -0.02495,2.62e-4 -0.04998,1.43e-4 -0.07476,-0.0014 -0.111345,-0.0082 -0.21184,-0.03282 -0.315164,-0.0789 -0.152454,-0.0683 -0.283733,-0.183979 -0.365944,-0.324971 -0.05731,-0.102218 -0.09179,-0.216565 -0.0945,-0.333782 -0.0098,-0.180105 0.04669,-0.364851 0.153501,-0.51305 0.152638,-0.187128 0.391334,-0.301848 0.63346,-0.330937 0.0034,-4.57e-4 0.0068,-8.82e-4 0.01027,-0.0014 0.01612,-0.0013 0.03286,-0.0015 0.04906,-0.0017 z m -2.331278,-11.005167 c 0.618446,-0.02746 0.903799,0.950262 0.272303,1.206293 -0.648468,0.33226 -1.234486,-0.781112 -0.563503,-1.116904 0.107577,-0.05383 0.20847,-0.08572 0.291201,-0.08938 z M 4.3415924,26.85899 c 0.118072,-0.0031 0.247496,0.03917 0.326698,0.114528 0.5433963,0.322648 0.2208113,1.214513 -0.411397,1.088883 -0.695052,-0.04791 -0.672333,-1.081713 -0.02732,-1.184553 0.03446,-0.01145 0.07266,-0.01785 0.112016,-0.01885 z m -4.46236599,0.905547 c 0.1231,-3.8e-5 0.250582,0.03773 0.352926,0.091 0.691582,0.298459 0.565204,1.444965 -0.230997,1.481625 -0.8833973,0.142136 -1.19127431,-1.279408 -0.331853,-1.526599 0.06388,-0.03208 0.136055,-0.04592 0.209921,-0.046 z m 6.20544129,3.217492 c 0.175179,-0.01052 0.359665,0.03634 0.498793,0.143706 0.722056,0.448212 0.138266,1.680732 -0.661758,1.374694 -0.782449,-0.191603 -0.72743,-1.438855 0.08869,-1.510396 0.0245,-0.0039 0.04924,-0.0065 0.07429,-0.008 z M -4.6995089,26.721577 c -0.0146,9.68e-4 -0.02926,0.0025 -0.04382,0.0042 -0.742483,0.06594 -1.2723803,0.928974 -1.001559,1.622084 0.09214,0.21432 0.231246,0.414145 0.408966,0.565998 0.355812,0.284786 0.856225,0.279953 1.251874,0.07836 l -0.0105,0.01215 c 0.0014,3.7e-4 0.0031,9.52e-4 0.0047,0.0014 0.329096,0.0878 0.799719,0.09363 1.137045,0.04059 0.480553,-0.07692 0.951879,-0.214222 1.432141,-0.292517 0.0335,0.0069 0.06911,0.01592 0.105548,0.02607 0.05207,0.370259 0.236498,0.718059 0.55427901,0.930657 0.3650733,0.267765 0.8688083,0.310892 1.2777523,0.120079 0.146107,-0.06155 0.280589,-0.150696 0.397906,-0.259337 0.06342,-0.0013 0.120454,-0.01062 0.169,-0.03074 0.45970499,0.117727 0.93837299,0.166557 1.40581499,0.241693 0.587346,0.1057 1.147911,0.347276 1.631568,0.696433 0.293411,0.204292 0.585912,0.508515 0.8490543,0.764045 -0.1969163,0.528867 -0.09929,1.179387 0.339182,1.555303 0.303293,0.240273 0.696979,0.405676 1.089404,0.329931 0.710747,-0.115058 1.286855,-0.840583 1.158791,-1.564693 -0.07019,-0.690465 -0.767905,-1.203403 -1.443234,-1.149218 -0.314187,0.0017 -0.626888,0.125162 -0.846875,0.348162 -0.131252,-0.119652 -0.264032,-0.236846 -0.402544,-0.347226 -0.4761383,-0.389271 -0.9971073,-0.728552 -1.5737363,-0.944773 -0.357216,-0.130966 -0.742227,-0.1985 -1.119476,-0.253525 -0.299903,-0.04513 -0.602721,-0.09286 -0.897145,-0.169544 0.0697,-0.181785 0.0973,-0.378111 0.07033,-0.574869 0.617773,-0.202319 1.315302,-0.443771 1.95283,-0.595972 0.07332,0.249534 0.225472,0.472472 0.457136,0.608106 0.363579,0.241535 0.855683,0.250045 1.2306913,0.02933 0.430266,-0.228918 0.707278,-0.739216 0.602972,-1.225066 -0.08465,-0.58004 -0.665488,-1.046044 -1.2511673,-0.991827 -0.484302,-0.0029 -0.902076,0.375023 -1.029238,0.828459 -0.02325,0.07031 -0.03859,0.143472 -0.0488,0.217524 -0.582923,0.130951 -1.143913,0.363968 -1.709436,0.555406 -0.109416,0.03769 -0.219878,0.0721 -0.330744,0.105164 -0.20350499,-0.434157 -0.64388899,-0.754459 -1.12555499,-0.798205 -0.07616,-0.0081 -0.153,-0.008 -0.229354,-0.0022 -0.4152863,0.02272 -0.81570331,0.256173 -1.01042131,0.628632 -0.06557,0.115889 -0.113421,0.24173 -0.14428,0.372108 -0.01548,0.0029 -0.03127,0.0058 -0.04656,0.0099 -0.71731,0.10669 -1.419525,0.353212 -2.15265,0.325132 -8e-4,-6e-5 -0.0018,-3.06e-4 -0.0024,-3.77e-4 0.04187,-0.07176 0.07628,-0.148874 0.09976,-0.230709 0.137414,-0.535217 -0.09832,-1.148483 -0.578897,-1.430934 -0.174938,-0.106305 -0.379722,-0.162778 -0.584285,-0.157409 l 2.5e-5,5.04e-4 c -0.01459,4.18e-4 -0.02933,7.45e-4 -0.04394,0.0017 z m 13.9772056,9.181007 c -0.294262,0.544645 -1.279633,0.287031 -1.174772,-0.386274 0.04819,-0.727039 1.304455,-0.657641 1.247726,0.09053 -0.0091,0.11995 -0.03358,0.222893 -0.07295,0.295747 z m -4.247495,7.929913 c -0.05797,0.102899 -0.160777,0.192194 -0.266129,0.22141 -0.5560223,0.300366 -1.1553623,-0.43467 -0.7227173,-0.912464 0.398259,-0.571647 1.2734583,-0.02092 1.0302323,0.585268 -0.0079,0.03546 -0.02203,0.07149 -0.04138,0.10579 z m 1.516219,-4.293459 c -0.06318,0.105651 -0.161139,0.195575 -0.259425,0.256001 -0.611421,0.439919 -1.530032,-0.257648 -1.152342,-0.959524 0.332015,-0.830878 1.709724,-0.364509 1.480153,0.499783 -0.0054,0.07128 -0.03053,0.14031 -0.06842,0.203722 z m -5.94895829,3.670096 c -0.08095,0.155715 -0.215995,0.289872 -0.379595,0.354055 -0.755548,0.389109 -1.51290231,-0.745055 -0.8392533,-1.274111 0.5664463,-0.572783 1.6081543,0.115338 1.2501493,0.852227 -0.0092,0.02301 -0.01974,0.04556 -0.03128,0.06786 z M 9.7940157,36.147027 c 0.0067,-0.01303 0.01293,-0.02636 0.0189,-0.03975 0.3249683,-0.670838 -0.143108,-1.568905 -0.87687,-1.692741 -0.231205,-0.03109 -0.474113,-0.01444 -0.695706,0.05999 -0.42715,0.158895 -0.680149,0.590669 -0.710522,1.033677 l -0.0051,-0.01525 c -0.0011,10e-4 -0.0024,0.0022 -0.0035,0.0033 -0.24443,0.237206 -0.49127,0.637943 -0.619107,0.954582 -0.18095,0.451779 -0.305363,0.92667 -0.484987,1.378906 -0.02315,0.02517 -0.04916,0.05111 -0.07661,0.07714 -0.344393,-0.145592 -0.737529,-0.166101 -1.083208,-0.0027 -0.417307,0.175589 -0.7131563,0.585566 -0.7596083,1.03444 -0.02228,0.156966 -0.0149,0.318147 0.01799,0.474617 -0.03144,0.0551 -0.05282,0.108776 -0.06047,0.160777 -0.337219,0.333872 -0.625081,0.719415 -0.92974,1.081809 -0.392495,0.449549 -0.887791,0.806305 -1.435857,1.041798 -0.326031,0.14673 -0.737321,0.241327 -1.09175299,0.335759 -0.352485,-0.440695 -0.960715,-0.691224 -1.508519,-0.508244 -0.3619753,0.136717 -0.70617231,0.389453 -0.84284631,0.765027 -0.266524,0.668853 0.05984,1.535902 0.74683801,1.798137 0.6283953,0.294599 1.4269653,-0.04036 1.72751029,-0.647563 0.160022,-0.27039 0.214762,-0.602109 0.136504,-0.905418 0.170067,-0.05113 0.338835,-0.104814 0.504703,-0.166919 0.578615,-0.20843 1.137382,-0.481008 1.619183,-0.864571 0.295914,-0.239145 0.551691,-0.534731 0.792753,-0.830086 0.192828,-0.234085 0.389377,-0.46934 0.6064583,-0.682511 0.120134,0.153203 0.274374,0.277766 0.457022,0.355743 -0.14389,0.633934 -0.295192,1.356397 -0.492228,1.981523 -0.2517403,-0.06534 -0.5211823,-0.04935 -0.7565833,0.07968 -0.394035,0.187786 -0.654211,0.605574 -0.657568,1.040702 -0.02471,0.486747 0.270706,0.986612 0.741102,1.146792 0.541103,0.225436 1.2393433,-0.03337 1.4937933,-0.563674 0.251373,-0.413964 0.141826,-0.966573 -0.18182,-1.308669 -0.04836,-0.05608 -0.10326,-0.106831 -0.161534,-0.153637 0.187211,-0.56736 0.275582,-1.168359 0.401957,-1.751878 0.02389,-0.113234 0.05114,-0.225674 0.07975,-0.337773 0.47706,0.04854 0.978137,-0.164663 1.263176,-0.55539 0.04608,-0.06118 0.0855,-0.127133 0.119705,-0.195651 0.193924,-0.367928 0.1994,-0.831405 -0.02006,-1.18984 -0.06572,-0.115808 -0.149089,-0.221519 -0.24508,-0.314989 0.0054,-0.01481 0.01107,-0.02981 0.01541,-0.04504 0.277077,-0.670183 0.426437,-1.39927 0.827254,-2.013768 4.64e-4,-6.55e-4 0.0013,-0.0014 0.0015,-0.0018 0.04004,0.07279 0.08852,0.14193 0.146656,0.204125 0.388528,0.392915 1.035769,0.505823 1.525028,0.238693 0.181091,-0.09544 0.334768,-0.242106 0.435281,-0.420353 l -4.46e-4,-2.38e-4 c 0.0071,-0.01272 0.01443,-0.02555 0.02113,-0.03856 z m -15.4598916,7.660494 c -0.3078603,-0.537076 0.43029,-1.238829 0.947297,-0.794936 0.592534,0.424041 -0.12838,1.455202 -0.734374,1.012773 -0.09715,-0.07094 -0.171747,-0.145986 -0.212924,-0.217834 z m -4.5017961,-7.78836 c -0.05691,-0.10349 -0.07864,-0.237917 -0.04796,-0.342852 0.03768,-0.630841 0.9782018,-0.752939 1.1563278,-0.133472 0.276011,0.639693 -0.653214,1.093364 -1.0402668,0.567237 -0.026,-0.02536 -0.04916,-0.05639 -0.0681,-0.09092 z m 2.8502088,3.550917 c -0.0565,-0.109366 -0.08132,-0.240007 -0.08089,-0.355381 -0.05174,-0.751457 1.025164,-1.164655 1.422697,-0.473823 0.5312443,0.719981 -0.591055,1.645196 -1.204677,0.994678 -0.05776,-0.04211 -0.103169,-0.09988 -0.1371,-0.16549 z m 0.01523,-6.989955 c -0.08969,-0.150843 -0.13256,-0.336315 -0.100908,-0.50918 0.06739,-0.847181 1.430396,-0.893277 1.525107,-0.04196 0.1883573,0.783237 -0.945375,1.306035 -1.383039,0.613488 -0.01468,-0.02 -0.02833,-0.04079 -0.04119,-0.06235 z m 1.156544,11.537431 c 0.0076,0.01252 0.01559,0.02487 0.02383,0.03702 0.3989343,0.629666 1.4088533,0.705035 1.9007293,0.146641 0.148243,-0.180127 0.262085,-0.39535 0.315584,-0.622906 0.09002,-0.446769 -0.14365,-0.889301 -0.504168,-1.148539 l 0.01564,0.0037 c -3.35e-4,-0.0014 -6.23e-4,-0.0032 -9.6e-4,-0.0047 -0.07282,-0.332766 -0.283353,-0.753711 -0.485112,-1.029202 -0.288635,-0.39184 -0.626702,-0.747804 -0.916423,-1.138755 -0.0092,-0.03294 -0.01754,-0.06872 -0.0252,-0.105766 0.305204,-0.215995 0.529782,-0.539325 0.573072,-0.919205 0.07064,-0.447197 -0.121921,-0.914666 -0.478955,-1.190661 -0.121674,-0.101651 -0.2625523,-0.180299 -0.4128833,-0.234765 -0.03026,-0.05575 -0.06462,-0.102207 -0.104787,-0.13611 -0.106081,-0.46253 -0.282088,-0.910336 -0.429569,-1.36022 -0.175276,-0.570461 -0.217516,-1.179401 -0.128889,-1.769298 0.04708,-0.354412 0.183389,-0.753822 0.289881,-1.104819 0.560296,-0.0674 1.0937073,-0.452345 1.2268283,-1.01435 0.07453,-0.379688 0.04108,-0.805398 -0.206115,-1.119452 -0.4280413,-0.578946 -1.3369293,-0.758416 -1.9217923,-0.312691 -0.581492,0.378861 -0.71756,1.234084 -0.359855,1.809455 0.145494,0.278475 0.398587,0.499785 0.697611,0.593088 -0.04615,0.171483 -0.08945,0.34321 -0.124062,0.516909 -0.127725,0.601602 -0.190472,1.220133 -0.118336,1.831729 0.04734,0.37751 0.163791,0.750648 0.287805,1.111156 0.09735,0.28723 0.193737,0.578238 0.260536,0.875063 -0.19351,0.02138 -0.380649,0.08684 -0.543163,0.20099 -0.462979,-0.456319 -0.997295,-0.965583 -1.424786,-1.462432 0.188172,-0.179535 0.316568,-0.416954 0.330928,-0.68502 0.04801,-0.433845 -0.169984,-0.87511 -0.538041,-1.107233 -0.400675,-0.277475 -0.981182,-0.289773 -1.3651778,0.02563 -0.476712,0.341113 -0.624648,1.070935 -0.308007,1.566613 0.219389,0.431768 0.7467858,0.629834 1.2080708,0.535012 0.07314,-0.01155 0.145205,-0.03147 0.215693,-0.05634 0.383572,0.458059 0.847811,0.849837 1.27717,1.264705 0.08365,0.07996 0.16486,0.162374 0.245062,0.245756 -0.292619,0.379892 -0.375435,0.918103 -0.193535,1.366242 0.02772,0.07139 0.063,0.139655 0.103215,0.204829 0.210531,0.358685 0.601566,0.607546 1.021845,0.609884 0.133056,0.0051 0.266829,-0.0099 0.396849,-0.04233 0.0097,0.01241 0.0195,0.02512 0.03017,0.03683 0.423612,0.588616 0.9645823,1.09972 1.2756663,1.764166 3.12e-4,7.4e-4 5.62e-4,0.0018 7.72e-4,0.0022 -0.08297,-0.0043 -0.16727,4.51e-4 -0.250764,0.01709 -0.538666,0.123191 -0.9756633,0.613804 -1.0064163,1.17039 -0.0143,0.204205 0.02938,0.412094 0.127916,0.591442 l 4.37e-4,-2.53e-4 c 0.0071,0.01273 0.01411,0.02572 0.02164,0.03828 z M 33.427483,28.656551 c 0.55228,0.0017 0.7652,0.885054 0.19162,1.086494 -0.591934,0.268652 -1.067048,-0.74831 -0.454903,-1.019157 0.09814,-0.04342 0.189403,-0.06757 0.263283,-0.06733 z m -0.08239,-0.766037 c -0.450612,0.03702 -0.929931,0.225435 -1.16997,0.629835 -0.217173,0.415434 -0.09714,0.929092 0.160402,1.297701 0.236454,0.361465 0.61546,0.661721 1.037058,0.741146 0.361751,0.08228 0.731496,-0.09943 0.96439,-0.371878 0.2912,-0.321808 0.425322,-0.769652 0.348308,-1.202062 -0.07447,-0.471141 -0.445048,-0.856688 -0.882547,-1.020703 -0.145643,-0.054 -0.302283,-0.08042 -0.457641,-0.07399 z m -18.77262,-1.597868 c -0.05634,0.03029 -0.01379,0.107817 -0.02656,0.158302 1.32e-4,6.068476 3.99e-4,12.13695 5.32e-4,18.205933 0.526844,-0.01799 1.05498,-0.0175 1.58247,-0.01614 2.124973,0.01056 4.257301,0.115729 6.375725,-0.08747 0.92546,-0.09117 1.853923,-0.241046 2.728539,-0.574875 1.04178,-0.435429 1.865208,-1.278487 2.419801,-2.24854 0.569456,-0.995966 0.861244,-2.122883 0.949793,-3.261306 0.155496,-1.65897 0.267856,-3.331742 0.124966,-4.994842 -0.129798,-1.463591 -0.441676,-2.939271 -1.107071,-4.26159 -0.406735,-0.849979 -1.125211,-1.532914 -1.952168,-1.978788 -0.947255,-0.506024 -2.019274,-0.736904 -3.076974,-0.845134 -1.259628,-0.125883 -2.529131,-0.08845 -3.795005,-0.09373 -1.398983,0.0011 -2.797964,0.0024 -4.196946,0.0036 -0.0089,-0.0019 -0.01769,-0.0068 -0.02708,-0.0052 z m 5.616483,1.201302 c 1.247537,0.0048 2.514722,0.09691 3.701801,0.507182 0.89504,0.301232 1.640509,0.95962 2.135956,1.754292 0.539128,0.865295 0.818158,1.865835 0.966974,2.867121 0.195459,1.307104 0.206834,2.633654 0.167156,3.952343 -0.0429,1.157167 -0.167438,2.324209 -0.532178,3.428475 -0.297671,0.900917 -0.809239,1.752175 -1.5601,2.345874 -0.589214,0.471837 -1.320487,0.722745 -2.053229,0.86597 -1.379131,0.288888 -2.797561,0.253179 -4.198558,0.221308 -0.885206,-0.01939 -1.771096,-0.03235 -2.656193,-5.34e-4 -0.03146,-0.938127 -0.0068,-1.877122 -0.01615,-2.815531 -5.35e-4,-4.366582 -0.0013,-8.732862 -0.0016,-13.099243 1.3485,0.04671 2.697603,-0.003 4.046003,-0.02708 z m 12.414988,5.079193 c 0.149557,3.956731 -0.600544,7.671773 0.349316,11.219149 0.06621,0.247267 0.202468,0.510873 0.458743,0.606037 0.380235,0.154959 0.837187,7.85e-4 1.099083,-0.30174 0.125584,-0.14347 0.22602,-0.36661 0.116094,-0.541088 -1.16637,-1.851313 -0.643561,-7.881151 -0.404902,-11.18992 0.04676,-0.648277 -1.643734,-0.46444 -1.618334,0.207562 z m 7.648901,3.849075 c 0.02405,-0.848 0.624465,-1.601735 1.237136,-2.195521 0.422942,-0.435669 0.929698,-0.819762 1.528089,-0.968643 0.771189,-0.206213 1.639324,-0.104221 2.308018,0.345217 0.519635,0.345663 0.855919,0.933585 0.926185,1.550177 0.08964,0.749407 0.152879,1.502376 0.16417,2.257473 0,0 -0.09264,5.749661 -0.110472,5.894207 -0.01783,0.144546 -0.04295,0.643315 0.189227,0.845393 0.374583,0.326019 1.123933,0.366938 1.213604,0.202016 0.08967,-0.164922 0.433171,-8.20533 0.433171,-8.20533 0.181687,-0.554269 0.47636,-1.068616 0.840266,-1.52306 0.38938,-0.477869 0.868747,-0.891075 1.42121,-1.168571 0.688278,-0.33925 1.517797,-0.422438 2.241513,-0.14525 0.530561,0.204203 1.006608,0.600644 1.225858,1.125621 0.941303,2.253889 0.804573,7.072993 0.376376,9.59473 0.01455,0.583369 0.907664,0.430057 1.143572,0.345356 0.157472,-0.06169 0.275257,-0.208941 0.301226,-0.369253 0.311753,-1.924827 0.463449,-6.392115 -0.113029,-9.15409 -0.161404,-0.773317 -0.490572,-1.531871 -1.104616,-2.032433 -0.56193,-0.472598 -1.282661,-0.755009 -2.01964,-0.77788 -0.744421,-0.02932 -1.49652,0.101597 -2.170458,0.427042 -0.569058,0.275635 -1.072464,0.685743 -1.474948,1.164018 -0.354765,0.422526 -0.636212,0.786558 -0.835663,1.299489 -0.0344,0.08696 -0.159805,0.08929 -0.205083,0.01176 -0.03985,-0.07373 -0.02217,-0.16279 -0.04653,-0.241909 -0.10173,-0.624693 -0.377451,-1.114526 -0.828495,-1.564418 -0.70142,-0.730635 -1.720515,-1.064532 -2.728412,-1.113349 -1.429721,-0.06925 -2.377729,0.792196 -3.037732,1.534642 -0.213336,0.23998 -0.441784,0.477877 -0.724171,0.636225 -0.200886,0.0819 -0.426147,-0.08226 -0.471024,-0.281801 -0.228487,-1.015917 -0.487287,-1.315246 -1.342826,-1.149089 -0.815065,0.158295 -0.593293,0.402085 -0.147974,1.641849 0.510879,1.422285 0.116644,6.981736 0.122744,7.347014 0.0062,0.365277 -0.118311,1.471105 0.06904,2.187409 0.06144,0.207756 0.19623,0.411832 0.439818,0.453131 0.243592,0.04129 0.560627,0.05206 0.757415,-0.142685 0.167048,-0.166402 0.212138,-0.417538 0.218371,-0.637243 z m 19.083781,-2.926842 c -0.837648,1.439186 -0.507897,2.753868 0.390321,3.801621 1.831752,2.136705 6.558667,1.120417 6.961633,3.361192 0.459899,2.557349 -2.094658,3.056933 -4.161435,2.864647 -1.047862,-0.09749 -2.424156,-0.900808 -2.959417,-1.2569 -0.204863,-0.13629 -0.458089,0.519136 -0.34705,0.832092 1.59335,2.054328 6.025855,1.9058 7.522681,0.686763 1.157166,-0.942415 1.521995,-2.724575 1.078038,-3.774017 -1.162241,-2.747343 -4.79657,-1.665292 -6.612182,-3.024223 -0.739252,-0.553309 -1.564417,-1.525119 -0.952014,-2.720359 1.356615,-2.647731 5.179761,-1.002322 6.172178,0.190092 0.20957,0.251809 0.768067,-0.625076 0.589757,-0.855663 -1.474237,-1.906451 -6.145259,-2.746441 -7.68251,-0.105245 z"


poplarData : String
poplarData =
    "m 0.00267057,7.9763769e-5 c 0,5.003841536231 -0.00617349,10.008104236231 0,15.011622236231 5.5195e-4,0.44735 1.37408343,0.400505 1.37408343,0 V 8.6569935 C 3.7441397,8.5856715 6.1604078,8.8432608 8.4735152,8.2497797 9.8905015,7.6379234 10.744068,6.0566872 10.688895,4.5383449 10.710467,3.2875403 10.373802,1.9433244 9.4372856,1.055322 8.5300887,0.21973972 7.2447318,-0.03418275 6.0514283,0.00731453 4.0353011,0.00483553 2.0187974,0.00255856 0.00267057,7.9763769e-5 Z M 32.882345,0.0011133 c -0.762327,-0.03902973 -1.339452,0.92691768 -0.946719,1.5813131 0.08717,0.1452589 0.197026,0.2756504 0.315229,0.3963618 -0.443309,0.9384049 -0.504412,2.0234751 -0.294557,3.0303333 -0.10901,-0.014793 -0.233743,0.00124 -0.329635,0.037545 -0.342155,-0.2688349 -0.652348,-0.7279517 -0.806219,-1.0473114 0.554384,-0.3681758 0.479549,-1.3149923 -0.09974,-1.6226544 -0.463383,-0.2827052 -1.163748,-0.1300188 -1.36168,0.451656 -0.331612,0.9745261 0.537612,1.3986032 1.0143,1.3462137 0.237976,0.3470547 0.295101,0.5285965 0.831075,1.1461631 -0.589423,0.4855192 -0.430706,1.5904887 0.294557,1.858301 0.158494,0.060888 0.329758,0.074568 0.497647,0.088368 0.229289,0.3571306 0.57702,0.8914529 0.519477,1.2519402 -1.322138,-0.028979 -1.480411,1.6500097 -0.339766,2.0902807 -0.0508,0.486783 -0.0369,0.871604 0.08332,1.292163 -1.095757,0.336714 -1.023492,1.811371 0.03566,2.051573 -0.02492,0.577646 -0.03757,0.873243 0.02828,1.522642 -0.566362,0.185321 -0.843765,0.502238 -0.838799,0.98384 0.0054,0.523125 0.424416,0.928542 0.947232,0.947237 1.102788,0.03943 1.267296,-1.500143 0.388596,-1.845367 -0.05868,-0.640903 -0.07966,-0.954434 -0.06484,-1.526703 0.794178,-0.13899 1.091906,-1.104952 0.738428,-1.627822 0.145675,-1.081625 0.73282,-2.025942 1.647453,-2.5481875 C 35.996002,10.427778 37.00725,9.6597836 36.898131,8.7066036 36.837063,8.1731602 36.362005,7.7693333 35.820012,7.8180612 36.421962,6.6626737 35.087074,5.9182139 34.432318,6.437312 33.932197,6.8338139 33.887707,7.5050439 34.272733,7.8892168 33.993286,8.4036454 33.482523,8.5551337 33.200147,8.7634483 32.960598,8.4685098 33.138197,8.0600622 33.276637,7.7464466 33.587437,7.0423534 33.66256,6.249657 33.554141,5.4923002 33.933026,5.2162498 34.208692,4.7730476 34.16437,4.2684025 34.087277,3.3906227 32.83029,2.9771158 32.306145,3.6820588 32.291699,3.1767173 32.443346,2.6644693 32.65858,2.1612076 33.364833,2.3402508 34.133084,1.5977035 33.928277,0.88323791 33.79898,0.4146086 33.368183,0.04427767 32.882342,0.00163007 Z m -0.0057,0.59376755 c 0.02381,-8.4668e-4 0.04786,7.051e-5 0.07183,0.003101 0.465296,0.0686348 0.561708,0.85933495 0.09663,1.00304855 C 32.643798,1.7247249 32.19094,1.193395 32.457041,0.83466172 32.551228,0.69797108 32.710013,0.60081331 32.876656,0.59488085 Z M 30.301091,2.7415415 c 0.443128,0.023231 0.516486,0.8234063 0.06253,0.9203656 -0.387868,0.096466 -0.770641,-0.4486832 -0.471807,-0.7575835 0.140989,-0.1457378 0.259872,-0.1706147 0.409277,-0.1627821 z m 3.066422,1.2496094 c 0.341228,0.22532 0.341734,0.8403186 -0.132677,0.9332686 -0.339412,0.0665 -0.668183,-0.2573939 -0.581984,-0.6302261 0.08126,-0.3514739 0.445457,-0.4808041 0.714661,-0.3030425 z m -0.223715,1.56076 c 0.125028,0.7121443 -0.0055,1.6654376 -0.366357,2.2265764 -0.110396,-0.2088179 -0.220339,-0.4180139 -0.330731,-0.6268408 0.591659,-0.3363173 0.69368,-1.2529711 0.224278,-1.7332431 0.235097,0.1303348 0.366184,0.1017549 0.47281,0.1335075 z m -1.038675,1.1299852 c -0.404825,0.164793 -0.898075,-0.3736613 -0.632524,-0.7524156 0.09465,-0.1349941 0.255699,-0.3000481 0.59358,-0.2144336 0.336273,0.085207 0.477389,0.7883703 0.03894,0.9668492 z M 35.175284,6.83427 c 0.323162,0.1177792 0.292084,0.8508531 -0.167989,0.8511976 -0.241724,0.00279 -0.447739,-0.2117327 -0.448408,-0.4386604 -8.51e-4,-0.2885761 0.272704,-0.5377989 0.616397,-0.4125372 z m -0.202858,1.4184177 c -0.249714,0.3364934 -0.22661,0.8139989 -0.08866,1.2078795 -1.367217,0.8546738 -1.529575,1.5963318 -1.802164,2.5280908 -0.07011,-0.06777 -0.192544,-0.126193 -0.381204,-0.156144 -0.11737,-0.342177 -0.118135,-0.801392 -0.08075,-1.219381 0.63922,-0.09642 1.04787,-0.8046012 0.840433,-1.4192185 0.438899,-0.3930787 0.947466,-0.5832941 1.238484,-1.0220232 0.04006,0.037956 0.185416,0.098366 0.273864,0.080796 z m 0.964531,0.177443 c 0.472798,0.042105 0.553432,0.8637969 0.08475,0.9978807 C 35.631564,9.5525546 35.160448,9.0319042 35.430522,8.6879985 35.582983,8.493859 35.728847,8.4115973 35.936957,8.4301307 Z m -3.273721,0.6320084 c 0.410079,0.1607874 0.321864,1.0118669 -0.205413,0.9807929 -0.32249,-0.019 -0.472276,-0.2800478 -0.456549,-0.5340375 0.01666,-0.2691154 0.243216,-0.6109412 0.661962,-0.4467554 z m -0.0026,3.3718849 c 0.459677,0.05151 0.505238,0.848282 0.05788,0.976177 -0.394281,0.188755 -0.878512,-0.30309 -0.607717,-0.727094 0.139459,-0.218362 0.334316,-0.273234 0.549837,-0.249083 z m -0.185683,3.548502 c 0.477777,0.05379 0.587374,0.899013 0.02259,0.914543 -0.736708,0.02025 -0.470872,-0.965012 -0.02259,-0.914543 z M 6.398179,1.0723754 C 7.9988106,1.0606958 9.396066,2.5811563 9.3117111,4.1709221 9.3819232,5.3891076 8.7913421,6.706547 7.6642581,7.2575833 6.5430587,7.7414957 5.299854,7.5203796 4.1182029,7.5681614 3.2050901,7.565833 2.2924909,7.5631263 1.3793379,7.5609276 1.378605,5.3981797 1.3774805,3.235641 1.3767529,1.0728932 3.0504854,1.0727421 4.7244456,1.0726787 6.3981779,1.0723773 Z m 19.755485,4.0282141 c -0.08098,-0.0019 -0.161988,-0.00127 -0.24288,0.00156 l 5.29e-4,-5.292e-4 c -1.006668,0.02792 -1.932072,0.5425402 -2.749718,1.100719 C 22.767444,6.3072795 22.4679,5.9445629 22.27224,5.6535308 22.124194,5.433322 21.94675,5.1136704 21.685192,5.129527 21.312327,5.152131 20.666379,5.5302484 20.90074,5.8519701 c 0.524973,0.7206614 0.729531,1.6030878 0.616503,2.4525854 -0.0047,3.7443285 -0.0083,6.1968715 -0.01344,9.9416015 -5.02e-4,0.362799 1.194197,0.355985 1.194766,0.04134 0.0036,-1.977403 0.0238,-2.704004 0.0248,-4.681411 1.152969,1.42208 3.231212,1.977905 4.937709,1.302775 1.290205,-0.497409 2.151598,-1.761939 2.422088,-3.086661 C 30.379342,10.341166 30.318329,8.739826 29.679579,7.3495665 29.304562,6.5218847 28.632033,5.8349191 27.795963,5.4742118 27.285812,5.2419315 26.720554,5.1138413 26.153677,5.100588 Z m -10.656252,0.024805 c -1.260627,-0.014343 -2.606159,0.4647207 -3.358469,1.5218845 -0.596713,0.8680719 -0.651644,1.9564055 -0.642858,2.9734886 0.01533,1.2708494 -0.09337,2.6048614 0.476976,3.7827494 0.536729,1.019676 1.676408,1.605024 2.793126,1.737894 1.26003,0.146139 2.624536,-0.06583 3.647341,-0.879541 0.885039,-0.710621 1.221304,-1.921723 1.201483,-3.023099 C 19.554548,9.8406375 19.757951,8.3822432 19.207799,7.0555266 18.692034,5.9414834 17.495262,5.2909201 16.31132,5.1636353 16.04058,5.1311823 15.773508,5.1197035 15.497412,5.1253945 Z m 22.116611,1.4316867 c -0.34556,0.3437509 0.299977,1.065906 0.824242,0.6144384 1.198667,-1.0322241 2.852898,-1.6464152 4.353983,-0.7797632 0.795675,0.4593829 0.970653,1.3989974 0.679311,2.1598141 -0.268021,0.557521 -0.931232,0.7424796 -1.50224,0.7606839 -1.362908,0.2202258 -2.808297,0.244573 -4.033367,0.9575736 -1.229855,0.748447 -1.588145,2.586692 -0.713656,3.73934 0.520843,0.735092 1.41833,1.100035 2.299614,1.155496 1.46708,0.133492 2.99258,-0.443207 4.004426,-1.53222 0.241651,0.635964 0.286818,0.887337 0.726576,1.278486 0.28169,0.250556 1.15188,-0.355139 0.846462,-0.773603 -1.014926,-1.39059 -0.0493,-5.0654111 -0.60565,-7.1417444 C 44.285549,6.2186629 43.437602,5.340784 41.785348,5.0941528 40.133094,4.8475216 38.672504,5.5041446 37.614023,6.5570812 Z m 13.85399,-1.4239367 c -0.0613,-0.0022 -0.122966,-0.00198 -0.184487,5.292e-4 C 50.212119,5.216902 49.27886,5.9088044 48.668169,6.7625318 48.413779,6.9762527 48.162352,6.6734278 48.050115,6.3945923 47.858371,5.91823 47.804255,5.6478486 47.451699,5.2008559 47.266341,4.9658498 46.063262,5.4688934 46.308092,5.8592195 c 0.470657,0.7503481 0.669365,1.8685199 0.67335,2.7182049 0.0109,2.3243506 -0.126683,4.5813496 -0.08423,6.3908816 0.0046,0.195578 1.202281,0.328147 1.215952,0 0.09148,-2.196142 -0.07941,-4.488597 0.393776,-6.4151698 0.39049,-1.0421211 1.199193,-2.0383064 2.321834,-2.2768841 0.528862,-0.1123913 1.070687,0.059692 1.570971,0.2294455 0.39778,0.1349705 0.764243,-0.8050165 0.301275,-1.0376721 C 52.31856,5.275826 51.897093,5.1485195 51.468016,5.1331593 Z M 25.787276,6.0323247 c 1.768341,-0.014626 3.316231,1.656624 3.234961,3.4137758 0.08037,1.2481065 -0.04524,2.5941005 -0.797371,3.6370205 -1.047801,1.389562 -3.318339,1.568015 -4.581656,0.376208 -0.595137,-0.536433 -0.907201,-1.332269 -0.918295,-2.126505 -0.0023,-0.983169 -0.03308,-1.973334 0.10077,-2.9497172 0.135144,-1.0053122 0.926651,-1.8325259 1.869146,-2.1492422 0.349924,-0.127335 0.720623,-0.1916246 1.092445,-0.2015399 z m -10.260925,0.013952 h 5.29e-4 c 1.064485,-0.03378 2.20241,0.5509579 2.601405,1.5761455 0.419897,1.4961968 0.256159,3.0680418 0.176218,4.5966598 -0.209412,1.132497 -1.302769,1.964512 -2.427255,2.023667 -1.046108,0.09474 -2.202289,-0.318289 -2.76677,-1.244896 -0.44896,-0.75575 -0.398802,-1.66734 -0.381374,-2.513048 0.01278,-0.8334615 -0.05001,-1.6816074 0.126608,-2.5016781 0.161099,-0.7102623 0.626379,-1.3723124 1.315688,-1.6495265 0.424243,-0.1931106 0.891317,-0.2754853 1.354963,-0.2873237 z m 27.84909,3.830808 0.01291,0.0062 c 0.279253,0.1173593 0.177295,0.4700633 0.212908,0.7048733 0.06605,0.924591 -0.05125,1.953604 -0.71159,2.658259 -0.92825,0.949372 -2.399338,1.235689 -3.657677,0.900728 -0.75976,-0.234558 -1.406646,-0.968047 -1.338425,-1.795772 0.04332,-0.9605 0.859937,-1.803571 1.808166,-1.902226 1.216111,-0.244356 2.456987,-0.329845 3.673698,-0.5720636 z"
