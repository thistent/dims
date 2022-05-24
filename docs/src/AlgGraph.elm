module AlgGraph exposing (..)

{-|


# An Algebraic Graph Library with an Edge Type

More than one type of edge can be represented in the same graph as part of a
custom type.

-}

import Dict exposing (Dict)
import Set exposing (Set)


type Graph e a
    = Empty
    | Vertex a
    | Overlay (Graph e a) (Graph e a)
    | Connect e (Graph e a) (Graph e a)


empty : Graph e a
empty =
    Empty


vertex : a -> Graph e a
vertex a =
    Vertex a


union : Graph e a -> Graph e a -> Graph e a
union g1 g2 =
    Overlay g1 g2


product : e -> Graph e a -> Graph e a -> Graph e a
product e g1 g2 =
    Connect e g1 g2



{- FIXME: This function might not produce a valid algebraic graph!
   edge : e -> a -> a -> Graph e a
   edge e a b =
       product e (singleton a) (singleton a)
-}


vertices : Graph e comparable -> Set comparable
vertices graph =
    case graph of
        Empty ->
            Set.empty

        Vertex a ->
            Set.singleton a

        Overlay a b ->
            Set.union (vertices a) (vertices b)

        Connect _ a b ->
            Set.union (vertices a) (vertices b)


edges : Graph e comparable -> Dict ( comparable, comparable ) e
edges graph =
    case graph of
        Empty ->
            Dict.empty

        Vertex a ->
            Dict.empty

        Overlay a b ->
            Dict.union (edges a) (edges b)

        {-
           TODO:

           Determin whether or not returning looped edges on a `Connect` branch
           should be the default behavior.

           This may be good for Category Theory or State Machine descriptions.
        -}
        Connect e a b ->
            let
                vertA =
                    vertices a |> Set.toList

                vertB =
                    vertices b |> Set.toList

                cross =
                    vertA
                        |> List.concatMap
                            (\x ->
                                List.map (\y -> ( x, y ))
                                    vertB
                            )
                        |> List.map (\x -> ( x, e ))
            in
            cross
                |> Dict.fromList
                |> Dict.union (edges a)
                |> Dict.union (edges b)


isEmpty : Graph e a -> Bool
isEmpty graph =
    case graph of
        Empty ->
            True

        Vertex a ->
            False

        Overlay a b ->
            isEmpty a && isEmpty b

        Connect _ a b ->
            isEmpty a && isEmpty b


hasVertex : a -> Graph e a -> Bool
hasVertex elem graph =
    case graph of
        Empty ->
            False

        Vertex a ->
            elem == a

        Overlay a b ->
            hasVertex elem a || hasVertex elem b

        Connect _ a b ->
            hasVertex elem a || hasVertex elem b


hasEdge : comparable -> Graph comparable a -> Bool
hasEdge edg graph =
    case graph of
        Empty ->
            False

        Vertex _ ->
            False

        Overlay a b ->
            hasEdge edg a || hasEdge edg b

        Connect e a b ->
            if edg == e then
                True

            else
                hasEdge edg a || hasEdge edg b


mapVertices : (a -> b) -> Graph e a -> Graph e b
mapVertices fun graph =
    case graph of
        Empty ->
            Empty

        Vertex a ->
            Vertex (fun a)

        Overlay a b ->
            Overlay (mapVertices fun a) (mapVertices fun b)

        Connect e a b ->
            Connect e (mapVertices fun a) (mapVertices fun b)


mapEdges : (e -> f) -> Graph e a -> Graph f a
mapEdges fun graph =
    case graph of
        Empty ->
            Empty

        Vertex a ->
            Vertex a

        Overlay a b ->
            Overlay (mapEdges fun a) (mapEdges fun b)

        Connect e a b ->
            Connect (fun e) (mapEdges fun a) (mapEdges fun b)


{-| TODO:


## Consider a similar function for edges

If you have a function of type: `(e -> Graph f a) -> Graph e a -> Graph f a`
then the only output of the `(e -> Graph f a)` part can be `Empty` because
there is no source of an `a` typed value.

It may be possible to have the following type:

      concatMap : (e -> a -> Graph f b) -> Graph e a -> Graph f b

The problem with this is that

-}
concatMap : (a -> Graph e b) -> Graph e a -> Graph e b
concatMap fun graph =
    case graph of
        Empty ->
            Empty

        Vertex a ->
            fun a

        Overlay a b ->
            Overlay (concatMap fun a) (concatMap fun b)

        Connect e a b ->
            Connect e (concatMap fun a) (concatMap fun b)


foldVertices : (a -> b -> b) -> b -> Graph e a -> b
foldVertices fun acc graph =
    case graph of
        Empty ->
            acc

        Vertex a ->
            fun a acc

        Overlay a b ->
            foldVertices fun (foldVertices fun acc a) b

        Connect _ a b ->
            foldVertices fun (foldVertices fun acc a) b


foldEdges : (e -> f -> f) -> f -> Graph e a -> f
foldEdges fun acc graph =
    case graph of
        Empty ->
            acc

        Vertex a ->
            acc

        Overlay a b ->
            foldEdges fun (foldEdges fun acc a) b

        Connect e a b ->
            foldEdges fun (foldEdges fun (fun e acc) a) b



{-
   compact : Graph e comparable -> Graph e comparable
   compact graph =
       let
           -- Edges --
           e =
               graph
                   |> edges
                   |> fromEdges

           -- Vertices --
           v =
               graph
                   |> vertices
                   |> fromVertices
       in
       union v e


   removeIdentityMorphisms : Graph e comparable -> Graph e comparable
   removeIdentityMorphisms graph =
       graph
           |> edges
           |> Dict.filter (\( a, b ) _ -> a /= b)
           |> fromEdges


   fromEdges : Dict ( comparable, comparable ) e -> Graph e comparable
   fromEdges dict =
       let
           foldFun ( a, b ) val acc =
               acc
       in
       if Dict.isEmpty dict then
           Empty

       else
           dict
               |> Dict.foldl foldFun Empty


   fromVertices : Set comparable -> Graph e comparable
   fromVertices vs =
       if Set.isEmpty then
           Empty

       else
           vs
               |> Set.foldl

-}
