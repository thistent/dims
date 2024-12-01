module Main exposing (main)

-- import Html exposing (Html, div, text)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Markdown.Block as Md
import Markdown.Html as MdHtml
import Markdown.Parser as Md
import Markdown.Renderer as Md
import Parser
import Samples exposing (..)
import Shared exposing (..)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }


init () =
    ( { name = "", content = "", env = DocText "" }
    , Http.get
        { url = "notes/main.md"
        , expect =
            Http.expectString <| ReceiveDoc "main.md"
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestDoc doc ->
            ( model
            , Http.get
                { url = "notes/" ++ doc
                , expect =
                    Http.expectString <| ReceiveDoc doc
                }
            )

        ReceiveDoc doc res ->
            -- https://raw.githubusercontent.com/thistent/gimbalabs/main/src/Markup.elm
            case res of
                Ok p ->
                    ( { model
                        | content = p
                        , name = doc
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )


subs : Model -> Sub Msg
subs model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Dims: A Platform for Decentralized Research & Collaboration"
    , body =
        [ Html.node "body"
            [ sty "background-color" "#000000"
            , sty "width" "auto"
            , sty "height" "100vh"
            , sty "font-family" "sans-serif"
            , sty "line-height" "38px"
            , sty "font-size" "22px"
            , sty "color" "#667788"
            , sty "padding" "0"
            , sty "margin" "0"
            ]
            [ Html.div
                [ sty "width" "auto"
                , sty "height" "100%"
                , sty "background-color" "#000000"
                ]
                [ Html.div
                    [ sty "padding" "30px"
                    , sty "background-color" "#000000"
                    ]
                    [ -- codeblock "Pony" helloPony
                      renderMd () model.content
                    ]
                ]
            ]
        ]
    }



{-
     section "Dims: A Platform for Decentralized Research & Collaboration"
       [ par <| String.repeat 10 "This is some important text here! "
       , item "Documentation about this project will go here."
       , item "Structure of the knowledge graph."
       , codeblock "Pony" helloPony
       , item "It needs to be flexible!"
       ]
   , section "This is the Next Section"
       [ par <| String.repeat 4 "This is some important text that you might want to see here! "
       , par <| String.repeat 5 "This is some important text that you might want to see here! "
       , par <| String.repeat 5 "This is some important text that you might want to see here! "
       ]
-}


mdTokenizer : Md.Renderer (Html Msg)
mdTokenizer =
    { heading =
        {-
           { level : HeadingLevel
           , rawText : String
           , children : List view
           }
           -> view
        -}
        \{ level, children } ->
            case level of
                Md.H1 ->
                    Html.div
                        [ sty "font-size" "42px"
                        , sty "line-height" "60px"
                        , sty "color" "#ffffff"
                        , sty "background-color" "#1a1a1a"
                        , sty "padding" "0px 20px"
                        , sty "border-radius" "10px 10px 0 0"
                        , sty "border-bottom" "1px solid"
                        , sty "margin-bottom" "10px"
                        ]
                    <|
                        children

                Md.H2 ->
                    Html.div
                        [ sty "font-size" "37px"
                        , sty "line-height" "55px"
                        , sty "color" "#d8f8d2"
                        , sty "background-color" "#181818"
                        , sty "padding" "0px 20px"
                        , sty "border-radius" "10px 10px 0 0"
                        , sty "border-bottom" "1px solid"
                        , sty "margin-bottom" "10px"
                        ]
                    <|
                        children

                Md.H3 ->
                    Html.div
                        [ sty "font-size" "32px"
                        , sty "line-height" "50px"
                        , sty "color" "#a8f0c9"
                        , sty "background-color" "#161616"
                        , sty "padding" "0px 20px"
                        , sty "border-radius" "10px 10px 0 0"
                        , sty "border-bottom" "1px solid"
                        , sty "margin-bottom" "10px"
                        ]
                    <|
                        children

                Md.H4 ->
                    Html.div
                        [ sty "font-size" "27px"
                        , sty "line-height" "45px"
                        , sty "color" "#74e7dc"
                        , sty "background-color" "#141414"
                        , sty "padding" "0px 20px"
                        , sty "border-radius" "10px 10px 0 0"
                        , sty "border-bottom" "1px solid"
                        , sty "margin-bottom" "10px"
                        ]
                    <|
                        children

                Md.H5 ->
                    Html.div
                        [ sty "font-size" "22px"
                        , sty "line-height" "40px"
                        , sty "color" "#47dcef"
                        , sty "background-color" "#121212"
                        , sty "padding" "0px 20px"
                        , sty "border-radius" "10px 10px 0 0"
                        , sty "border-bottom" "1px solid"
                        , sty "margin-bottom" "10px"
                        ]
                    <|
                        children

                Md.H6 ->
                    Html.div
                        [ sty "font-size" "20px"
                        , sty "line-height" "38px"
                        , sty "color" "#27ceff"
                        , sty "background-color" "#101010"
                        , sty "padding" "0px 20px"
                        , sty "border-radius" "10px 10px 0 0"
                        , sty "border-bottom" "1px solid"
                        , sty "margin-bottom" "10px"
                        ]
                    <|
                        children
    , paragraph =
        -- List view -> view
        \vs ->
            Html.p
                []
                vs
    , blockQuote =
        -- List view -> view
        \vs -> Html.p [] vs
    , html =
        -- Renderer (List view -> view)
        MdHtml.oneOf
            []
    , text =
        -- String -> view
        \s -> Html.text s
    , codeSpan =
        -- String -> view
        \s ->
            Html.span
                [ sty "font-family" "monospace"
                , sty "background-color" "#222222"
                , sty "padding" "2px 8px"
                , sty "border-radius" "6px"
                ]
                [ Html.text s ]
    , strong =
        -- List view -> view
        \vs ->
            Html.span
                [ sty "font-weight" "bold"
                , sty "color" "#ddbb00"
                ]
                vs
    , emphasis =
        -- List view -> view
        \vs -> Html.span [ sty "font-style" "italic" ] vs
    , strikethrough =
        -- List view -> view
        \vs -> Html.span [ sty "text-decoration" "line-through" ] vs
    , hardLineBreak =
        -- view
        Html.br [] []
    , link =
        {-
           { title : Maybe String
           , destination : String
           }
           -> List view
           -> view
        -}
        \{ title, destination } vs ->
            Html.a
                [ sty "background-color" "#0088ff33"
                , Ev.onClick <| RequestDoc destination
                , sty "padding" "0px 2px 0px 2px"
                , sty "border" "2px dotted #0088ff99"
                , sty "border-radius" "5px"
                , sty "white-space" "nowrap"
                , sty "text-wrap" "wrap"
                ]
                vs
    , image =
        {-
           { alt : String
           , src : String
           , title : Maybe String
           }
           -> view
        -}
        \{ alt, src } ->
            Html.img
                [ sty "alt" alt
                , sty "src" src
                ]
                []
    , unorderedList =
        -- List (ListItem view) -> view
        \items ->
            items
                |> List.map
                    (\(Md.ListItem task children) ->
                        Html.li
                            [ sty "line-height" "27px"
                            , sty "margin-top" "10px"
                            , sty "margin-bottom" "10px"
                            ]
                            ((case task of
                                Md.NoTask ->
                                    []

                                Md.IncompleteTask ->
                                    [ Html.a
                                        [ sty "font-family" "monospace"
                                        , sty "font-weight" "bold"
                                        , sty "color" "#ff9966"
                                        , sty "background-color" "#280c00"
                                        ]
                                        [ Html.text "[ ]" ]
                                    , Html.text " "
                                    ]

                                Md.CompletedTask ->
                                    [ Html.a
                                        [ sty "font-family" "monospace"
                                        , sty "font-weight" "bold"
                                        , sty "color" "#aaff88"
                                        , sty "background-color" "#0c2000"
                                        ]
                                        [ Html.text "[X]" ]
                                    , Html.text " "
                                    ]
                             )
                                ++ children
                            )
                    )
                |> Html.ul
                    [ sty "color" "#888888"
                    , sty "margin-bottom" "10px"
                    ]
    , orderedList =
        -- Int -> List (List view) -> view
        \start items ->
            items
                |> List.map
                    (\children ->
                        Html.li
                            [ sty "line-height" "27px"
                            , sty "margin-top" "10px"
                            , sty "margin-bottom" "10px"
                            ]
                            children
                    )
                |> Html.ol
                    [ sty "color" "#888888"
                    , sty "margin-top" "10px"
                    , sty "margin-bottom" "10px"
                    , Attr.start start
                    ]
    , codeBlock =
        {- { body : String
           , language : Maybe String
           }
           -> view
        -}
        \{ body, language } ->
            codeblock (language |> Maybe.withDefault "")
                (parseCode language body
                    |> highlight
                )

    -- helloPony
    , thematicBreak =
        -- view
        Html.hr
            [ sty "margin" "20px 0px 0px 0px"
            ]
            []
    , table =
        -- List view -> view
        \vs ->
            Html.div
                [ sty "overflow" "auto"
                ]
                [ Html.table
                    [ sty "background-color" "#1a1a1a"
                    , sty "border-radius" "10px"
                    ]
                    vs
                ]
    , tableHeader =
        -- List view -> view
        \vs ->
            Html.thead
                []
                vs
    , tableBody =
        -- List view -> view
        \vs -> Html.tbody [] vs
    , tableRow =
        -- List view -> view
        \vs -> Html.tr [] vs
    , tableCell =
        -- Maybe Alignment -> List view -> view
        \align vs ->
            Html.td
                [ sty "padding" "10px"
                , sty "background-color" "#111111"
                , alignSty align
                ]
                vs
    , tableHeaderCell =
        -- Maybe Alignment -> List view -> view
        \align vs ->
            Html.th
                [ sty "padding" "10px"
                , alignSty align
                ]
                vs
    }


alignSty : Maybe Md.Alignment -> Html.Attribute Msg
alignSty align =
    case align of
        Just a ->
            case a of
                Md.AlignLeft ->
                    sty "text-align" "left"

                Md.AlignCenter ->
                    sty "text-align" "center"

                Md.AlignRight ->
                    sty "text-align" "right"

        Nothing ->
            sty "text-align" "left"


renderMd : () -> String -> Html Msg
renderMd _ mdDoc =
    mdDoc
        |> Md.parse
        |> Result.withDefault
            [ Md.Paragraph [ Md.Text "Markdown parsing error!" ] ]
        |> Md.render mdTokenizer
        |> Result.withDefault
            [ Html.text "Markdown rendering error!" ]
        |> Html.div
            [ sty "color" "#ffffff"
            , sty "background-color" "#000000"
            ]


par : String -> Html Msg
par s =
    Html.p
        [ sty "color" "#aaacaf"
        , sty "word-spacing" "2px"
        , sty "letter-spacing" "1px"
        , sty "text-align" "justify"
        ]
        [ Html.text s
        , Html.span
            [ sty "color" "#0099bb"

            --, sty "text-decoration" "underline"
            , sty "padding" "0vmax 0.1vmax"
            , sty "border" "2px dashed #0099bb55"
            ]
            [ Html.text "Link" ]
        , Html.text " "
        , Html.text s
        ]


section : String -> List (Html Msg) -> Html Msg
section s hs =
    Html.div
        [ sty "color" "#ffffff"
        , sty "padding-bottom" "12px"
        ]
    <|
        [ Html.div
            []
          <|
            [ Html.span
                [ sty "font-size" "36px"
                , sty "line-height" "54px"
                ]
                [ Html.text s ]
            ]
                ++ hs
        ]


item : String -> Html Msg
item s =
    Html.div
        [ sty "color" "#888a8f"

        --, sty "padding" "12px 0"
        ]
        [ Html.text <| "• " ++ s ]


codeblock : String -> List (Html Msg) -> Html Msg
codeblock lang code =
    Html.div
        [ sty "padding" "24px 0 24px 0"
        ]
        [ Html.div
            []
            [ Html.div
                [ sty "background-color" "#282828"
                , sty "border-radius" "8px 8px 0 0"
                , sty "color" "#ffffff"
                , sty "width" "100%"
                , sty "height" "36px"
                , sty "user-select" "none"
                , sty "margin" "0"
                ]
                [ Html.span
                    [ sty "padding" "0 12px"
                    , sty "color" "#ffffff"
                    , sty "font-weight" "bold"
                    , sty "font-size" "18px"
                    ]
                    [ Html.text lang ]
                ]
            , Html.pre
                [ sty "padding" "0 0 0 0"
                , sty "overflow" "auto"
                , sty "font-size" "18px"
                , sty "margin" "0px"
                , sty "line-height" "32px"
                , sty "background-color" "#181818"
                , sty "font-family" "monospace"
                ]
                code
            ]
        ]


parseCode : Maybe String -> String -> Ast
parseCode maybeLang code =
    case maybeLang of
        Nothing ->
            DocText code

        Just lang ->
            case lang of
                _ ->
                    DocText code


highlight : Ast -> List (Html Msg)
highlight ast =
    let
        s =
            case ast of
                DocText str ->
                    str

                Delimiter str ->
                    str

                CodeText str ->
                    str

        chopFinalNewlines a =
            if String.endsWith "\n" a then
                String.dropRight 1 a
                    |> chopFinalNewlines

            else
                a

        splitList =
            s
                |> chopFinalNewlines
                |> String.split "\n"

        numberOfLines =
            List.length splitList
                |> toFloat
    in
    splitList
        |> List.indexedMap (\i line -> ( i, line ))
        |> List.map
            (\( i, line ) ->
                [ ln <| String.padLeft (logBase 10 (numberOfLines + 1) |> ceiling) '0' (String.fromInt <| i + 1)
                , Html.span [] [ Html.text line ]
                ]
            )
        |> List.intersperse [ Html.text "\n" ]
        |> List.concat
