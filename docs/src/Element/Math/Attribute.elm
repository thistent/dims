module Element.Math.Attribute exposing (..)


type Attribute
    = Size Float


size : Float -> Attribute
size f =
    Size f


testFun : { size : Float } -> Int
testFun rec =
    round rec.size
