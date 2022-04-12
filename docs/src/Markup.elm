module Markup exposing (..)

import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol)


type Title
    = Title String


fn : Int -> Int
fn x =
    x + 2
