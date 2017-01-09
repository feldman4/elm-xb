module GJK exposing (..)

import Vector as V exposing (Vector)
import List.Extra exposing (uniqueBy)
import Html exposing (text)


-- debugging imports

import Island.Things
import Utilities exposing (map3T)
import Frame


main : Html.Html msg
main =
    let
        cube =
            Island.Things.cube |> List.map (map3T V.fromVec3)

        aFrame =
            Frame.identity

        bFrame =
            Frame.identity |> Frame.extrinsicNudge (Vector -2.1 -2.1 -2.1)

        a =
            cube |> List.map (map3T (Frame.transformOutOf aFrame))

        b =
            cube |> List.map (map3T (Frame.transformOutOf bFrame))

        z =
            gjk a b
    in
        text (toString z)


type Simplex
    = Point Vector
    | Line ( Vector, Vector )
    | Triangle ( Vector, Vector, Vector )
    | Tetrahedron Tetrahedron


type alias Tetrahedron =
    ( Vector, Vector, Vector, Vector )


type alias Mesh =
    List ( Vector, Vector, Vector )


type alias Direction =
    Vector


origin : Vector
origin =
    Vector 0 0 0


{-| -}
support : Mesh -> Vector -> Vector
support shape direction =
    let
        unwrap ( a, b, c ) =
            [ a, b, c ]

        points =
            shape |> List.concatMap unwrap |> uniqueBy toString

        winner =
            points
                |> List.sortBy (\x -> -1 * (V.dot x direction))
                |> List.head
                |> Maybe.withDefault origin
    in
        winner


support2 : Mesh -> Mesh -> Vector -> Vector
support2 meshA meshB direction =
    support meshB (V.scale -1 direction)
        |> V.sub (support meshA direction)


{-| reversed sign of a to origin
-}
test : Vector -> Vector -> Bool
test a v =
    V.dot a v < 0


cross3 : Vector -> Vector -> Vector -> Vector
cross3 a b c =
    V.cross b c |> V.cross a


gjk : Mesh -> Mesh -> Maybe Tetrahedron
gjk meshA meshB =
    let
        dInitial =
            V.xAxis

        -- z =
        --     support meshA dInitial
        --         |> Debug.log "A support"
        --
        -- z2 =
        --     support meshB (V.scale -1 dInitial)
        --         |> Debug.log "B support"
        s =
            support2 meshA meshB dInitial

        d =
            V.scale -1 s
    in
        case gjkInner meshA meshB (Point s) d of
            Just (Tetrahedron t) ->
                Just t

            _ ->
                Nothing


gjkInner : Mesh -> Mesh -> Simplex -> Vector -> Maybe Simplex
gjkInner meshA meshB soFar d =
    let
        a =
            support2 meshA meshB d

        -- z =
        --     Debug.log "test" { a = a, d = d }
        z2 : Maybe Int
        z2 =
            case soFar of
                Tetrahedron _ ->
                    Just (Debug.crash "tetrahedron" 0)

                _ ->
                    Nothing
    in
        -- didn't pass origin, no intersection
        if V.dot a d < 0 then
            Nothing
        else
            let
                soFar2 =
                    extendSimplex soFar a

                ( soFar3, d2 ) =
                    doSimplex soFar2 d
            in
                case soFar3 of
                    -- only returned if we're done
                    Tetrahedron _ ->
                        Just soFar3

                    Triangle _ ->
                        gjkInner meshA meshB soFar3 d2

                    Line _ ->
                        gjkInner meshA meshB soFar3 d2

                    Point _ ->
                        gjkInner meshA meshB soFar3 d2


doSimplex : Simplex -> Vector -> ( Simplex, Vector )
doSimplex soFar d =
    case soFar of
        Point a ->
            ( Point a, origin )

        Line ( a, b ) ->
            if test (V.sub b a) a then
                ( Line ( a, b ), cross3 (V.sub b a) (V.scale -1 a) (V.sub b a) )
            else
                ( Point a, V.scale -1 a )

        Triangle ( a, b, c ) ->
            (doTriangle a b c)

        Tetrahedron ( a, b, c, d ) ->
            let
                abc =
                    V.cross (V.sub b a) (V.sub c a)

                adb =
                    V.cross (V.sub d a) (V.sub b a)

                acd =
                    V.cross (V.sub c a) (V.sub d a)

                bdc =
                    V.cross (V.sub d b) (V.sub c b)
            in
                if test a abc then
                    doTriangle a b c
                else if test a adb then
                    doTriangle a d b
                else if test a acd then
                    doTriangle a c d
                else if test b bdc then
                    doTriangle b d c
                else
                    ( soFar, d )


doTriangle : Vector -> Vector -> Vector -> ( Simplex, Vector )
doTriangle a b c =
    let
        ab =
            V.sub b a

        ac =
            V.sub c a

        abc =
            V.cross ab ac

        ao =
            V.scale -1 a

        testA =
            test a

        star =
            if testA ab then
                ( Line ( a, b ), cross3 ab ao ab )
            else
                ( Point a, ao )
    in
        if testA (V.cross abc ac) then
            if testA ac then
                ( Line ( a, c ), cross3 ac ao ac )
            else
                star
        else if testA (V.cross ab abc) then
            star
        else if testA abc then
            ( Triangle ( a, b, c ), abc )
        else
            ( Triangle ( a, c, b ), V.scale -1 abc )


extendSimplex : Simplex -> Vector -> Simplex
extendSimplex s x =
    case s of
        Point a ->
            Line ( x, a )

        Line ( a, b ) ->
            Triangle ( x, a, b )

        Triangle ( a, b, c ) ->
            Tetrahedron ( x, a, b, c )

        Tetrahedron ( a, b, c, d ) ->
            Debug.crash "fuck you" (Tetrahedron ( x, b, c, d ))
