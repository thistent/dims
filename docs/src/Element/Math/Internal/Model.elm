module Element.Math.Internal.Model exposing (..)

-- import Html.Attributes exposing (id, style)

import Element as El exposing (Attribute, Element, el)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


{-| Math Variable
-}
var : Float -> String -> Element msg
var fontSize x =
    el
        [ Font.regular
        , Font.italic
        , Font.size <| round fontSize
        ]
    <|
        El.text
            x


{-| Constant
-}
const : Float -> String -> Element msg
const fontSize x =
    el [ Font.size <| round fontSize ] <| El.text x


{-| Fraction
-}
frac : Float -> Element msg -> Element msg -> Element msg
frac fontSize n d =
    el [] <|
        El.column
            [ El.centerY
            , Font.size 24
            , El.moveDown 1.25
            , El.width El.shrink
            ]
            [ el
                [ El.paddingEach { edges | bottom = 4 }
                , El.width El.fill

                --, Border.color <| El.rgb 1 1 1
                , Border.widthEach { edges | bottom = 1 }
                ]
              <|
                el
                    [ El.centerX
                    , El.paddingEach { edges | left = 6, right = 6 }
                    ]
                <|
                    n
            , el
                [ El.paddingEach { edges | top = 2 }
                , El.width El.fill

                --, Border.color <| El.rgb 1 1 1
                , Border.widthEach { edges | top = 1 }
                ]
              <|
                el
                    [ El.centerX
                    , El.paddingEach { edges | left = 6, right = 6 }
                    , Font.regular
                    ]
                <|
                    d
            ]


{-| Exponent
-}
exp : Float -> Element msg -> Element msg -> Element msg
exp fontSize e b =
    El.row
        [ El.spacing 3
        ]
        [ el [ El.alignBottom ] b
        , el
            [ Font.size <| round <| fontSize / 4
            , El.alignTop
            , El.padding 0
            , El.height El.shrink
            ]
          <|
            e
        ]


plus : Float -> Element msg -> Element msg -> Element msg
plus fontSize a b =
    El.row
        [ Font.italic
        ]
        [ a
        , el
            [ Font.unitalicized
            , Font.size <| round fontSize
            , El.moveRight 3.0
            , El.moveDown 2.0
            ]
          <|
            El.text " + "
        , b
        ]


{-| Label Kind of an Expression or Type
-}
ofKind : Float -> Element msg -> Element msg -> Element msg
ofKind fontSize k v =
    El.row []
        [ v
        , el [ El.moveRight 2.0 ] <| El.text " :: "
        , el [ El.moveDown 2.0 ] k
        ]


{-| Label the Type of an Expression
-}
ofType : Float -> Element msg -> Element msg -> Element msg
ofType fontSize t v =
    El.row []
        [ v
        , el [ El.moveRight 1.5 ] <| El.text " : "
        , el [ El.moveDown 2.0 ] t
        ]


{-| The Natural Numbers
-}
nat : Float -> Element msg
nat fontSize =
    el [ Font.size <| round fontSize ] <| El.text "ℕ"



-- Helper functions --


edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


corners =
    { topLeft = 0, bottomLeft = 0, bottomRight = 0, topRight = 0 }
