module Element.Math exposing (..)

-- import Html.Attributes exposing (id, style)

import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Math.Attribute as MA
import Element.Math.Internal.Model as Internal


type MathExpr
    = Const String
    | Var String
    | TyVar String
    | KVar String
    | Infix MathExpr MathExpr MathExpr
    | Exp MathExpr MathExpr
    | Sub MathExpr MathExpr
    | ExpSub MathExpr MathExpr MathExpr
    | Frac MathExpr MathExpr
    | OfType MathExpr MathExpr
    | OfKind MathExpr MathExpr
    | Pars MathExpr
    | Lam MathExpr MathExpr
    | ReductionRule String MathExpr MathExpr


type alias ColorScheme =
    { const : El.Color
    , var : El.Color
    , tyVar : El.Color
    , kVar : El.Color
    , infix : El.Color
    , exp : El.Color
    , frac : El.Color
    , ofType : El.Color
    , ofKind : El.Color
    , pars : El.Color
    , lam : El.Color
    , reductionRule : El.Color
    , replace : El.Color
    }


reduce : MathExpr -> MathExpr
reduce expr =
    case expr of
        Sub (Exp b e) s ->
            ExpSub (reduce b) (reduce e) (reduce s)

        Exp (Sub b s) e ->
            ExpSub (reduce b) (reduce e) (reduce s)

        _ ->
            expr


render : Float -> ColorScheme -> MathExpr -> Element msg
render size color expr =
    el
        [ El.centerX
        , El.centerY
        , El.width El.shrink
        , El.height El.shrink
        , Font.size <| round size
        , Font.regular
        , Font.italic
        ]
    <|
        case reduce expr of
            Const s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.const --<| El.rgb 1 0.8 0.6
                    ]
                <|
                    El.text s

            Var s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.var --<| El.rgb 0.6 0.8 1
                    , El.moveUp <| size * 0.1
                    ]
                <|
                    El.text s

            TyVar s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.tyVar --<| El.rgb 0.6 1 0.8
                    , Font.bold
                    ]
                <|
                    El.text s

            KVar s ->
                el
                    [ El.centerX
                    , El.centerY
                    , El.padding <| round <| size * 0.1
                    , Font.color color.kVar --<| El.rgb 0.8 1 0.6
                    ]
                <|
                    El.text s

            Infix a i b ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.5
                    , El.padding <| round <| size * 0.1
                    ]
                    [ el [ El.centerX, El.centerY, El.width El.shrink ] <| render size color a
                    , el
                        [ Font.size <| round <| size * 1.5
                        , Font.color color.infix --<| El.rgb 0.5 0.5 0.5

                        --, El.moveUp <| size * 0.15
                        ]
                      <|
                        render size color i
                    , el [ El.centerX, El.centerY, El.width El.shrink ] <| render size color b
                    ]

            Exp b e ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.2
                    , El.padding <| round <| size * 0.1
                    ]
                    [ render size color b
                    , el
                        [ El.alignTop
                        , El.moveUp <| size * 0.2
                        , El.moveLeft 2.0
                        ]
                      <|
                        render (size * 0.75) color e
                    ]

            Sub b s ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.2
                    , El.padding <| round <| size * 0.1
                    ]
                    [ render size color b
                    , el
                        [ El.alignBottom
                        , El.moveLeft 2.0
                        ]
                      <|
                        render (size * 0.75) color s
                    ]

            ExpSub b e s ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.2
                    , El.padding <| round <| size * 0.1
                    ]
                    [ render size color b
                    , El.column
                        [ El.moveLeft 2.0 ]
                        [ el
                            [ El.alignTop
                            , El.moveRight 1.0
                            ]
                          <|
                            render (size * 0.75) color e
                        , el
                            [ El.alignBottom
                            , El.moveLeft 2.0
                            ]
                          <|
                            render (size * 0.75) color s
                        ]
                    ]

            Frac n d ->
                render size color <| ReductionRule "" n d

            OfType t v ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.3
                    , El.padding <| round <| size * 0.1
                    ]
                    [ el [ El.centerX, El.centerY ] <| render size color v
                    , el
                        [ El.centerX
                        , El.centerY
                        , Font.size <| round <| size * 0.8
                        , Font.color color.ofType --<| El.rgb 0.5 0.5 0.5
                        , El.moveRight <| size * 0.05
                        ]
                      <|
                        El.text "∶"

                    --":"
                    , el [ El.centerX, El.centerY ] <| render size color t
                    ]

            OfKind k v ->
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacing <| round <| size * 0.25
                    , El.padding <| round <| size * 0.1
                    ]
                    [ el [ El.centerX, El.centerY ] <| render size color v
                    , el
                        [ El.centerX
                        , El.centerY
                        , Font.size <| round <| size * 0.8
                        , Font.color color.ofKind --<| El.rgb 0.5 0.5 0.5
                        ]
                      <|
                        El.text "∷"

                    --"⦂⦂", "::"
                    , el
                        [ El.centerX
                        , El.centerY
                        ]
                      <|
                        render size color k
                    ]

            Pars exp ->
                el
                    [ El.paddingEach
                        { edges
                            | left = round <| size * 0.4
                            , right = round <| size * 0.1
                            , top = round <| size * 0.2
                            , bottom = round <| size * 0.2
                        }
                    ]
                <|
                    el
                        [ El.paddingEach
                            { edges
                                | left = round <| size * 0.4
                                , right = round <| size * 0.6
                            }
                        , Border.widthEach { edges | left = round <| size * 0.1, right = round <| size * 0.1 }
                        , Border.rounded <| round <| size * 1.25
                        , color.pars |> changeAlpha 0.3 |> Border.color -- El.rgba 0.5 0.5 0.5 0.25
                        , color.pars |> changeAlpha 0.05 |> Bg.color -- El.rgba 0.5 0.5 0.5 0.1
                        ]
                    <|
                        el [ El.moveUp <| size * 0.075 ] <|
                            render (size * 0.98) color exp

            Lam v b ->
                El.row [ Font.color color.lam ]
                    [ el [ El.padding <| round <| size * 0.2 ] <| El.text lambda
                    , render size color v
                    , el [ El.padding <| round <| size * 0.2, El.moveRight <| size * 0.1 ] <| El.text "⟶"
                    , render size color b
                    ]

            ReductionRule name n d ->
                let
                    marginSize =
                        round <| size * 0.4
                in
                El.row []
                    [ El.column
                        [ El.centerX
                        , El.centerY

                        --, El.spacing <| round <| size * 0.25
                        , El.padding <| round <| size * 0.1
                        , El.moveRight <| size * 0.15
                        ]
                        [ el
                            [ El.centerX
                            , El.centerY
                            , El.paddingEach
                                { edges
                                    | left = marginSize
                                    , right = marginSize
                                }
                            , El.moveRight <| size * 0.2
                            ]
                          <|
                            render (size * 0.95) color n
                        , el
                            [ El.width El.fill
                            , El.height <| El.px 2
                            , El.centerX
                            , El.centerY
                            , El.moveLeft <| size * 0.1
                            , Bg.color color.frac --<| El.rgb 0.5 0.5 0.5
                            , El.onRight <|
                                el
                                    [ Font.unitalicized
                                    , El.moveUp <| size * 0.6
                                    , El.moveRight <| size * 0.25
                                    ]
                                <|
                                    El.text name
                            ]
                            El.none
                        , el
                            [ El.centerX
                            , El.centerY
                            , El.paddingEach
                                { edges
                                    | left = marginSize
                                    , right = marginSize
                                }
                            , El.moveLeft <| size * 0.2
                            ]
                          <|
                            render (size * 0.95) color d
                        ]
                    , el [ color.frac |> changeAlpha 0 |> Font.color ] <| El.text name
                    ]



--
-- Math Characters --


{-| The Natural Numbers
-}
nat : String
nat =
    "ℕ"


rangeRestriction : String
rangeRestriction =
    "▷"


domRestriction : String
domRestriction =
    "◁"


bigLambda : String
bigLambda =
    "Λ"


lambda : String
lambda =
    "λ"


context : String
context =
    "Γ"


dot : String
dot =
    "⦁"


star : String
star =
    --"★"
    "∗"


arrow : String
arrow =
    "⟶"


equiv : String
equiv =
    "≡"



-- Helper functions --


edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


corners =
    { topLeft = 0, bottomLeft = 0, bottomRight = 0, topRight = 0 }


changeAlpha : Float -> El.Color -> El.Color
changeAlpha alpha color =
    let
        rgbColor =
            color |> El.toRgb
    in
    { rgbColor | alpha = alpha } |> El.fromRgb
