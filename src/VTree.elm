module VTree exposing (..)

import Vector as V exposing (Vector)
import Math.Vector3 as V3 exposing (vec3, Vec3)
import Utilities exposing (..)


type VTree a
    = Node ( Vector, VTree a, VTree a )
    | Leaf a
    | Empty


{-| Tolerance for plane tests and the like.
-}
eps : Float
eps =
    0.001


buildTree : (List a -> Vector) -> (Vector -> a -> Bool) -> List a -> VTree a
buildTree separate test parts =
    case parts of
        [] ->
            Empty

        x :: [] ->
            Leaf x

        x :: xs ->
            let
                v =
                    separate parts

                ( left, right ) =
                    parts |> List.partition (test v)

                b =
                    buildTree separate test
            in
                Node ( v, b left, b right )


{-| For cases when the same item may appear in both branches, e.g., triangles
separated by a plane.
-}
buildTree2 : (List a -> ( Vector, List a, List a )) -> List a -> VTree a
buildTree2 separate parts =
    case parts of
        [] ->
            Empty

        x :: [] ->
            Leaf x

        x :: xs ->
            let
                ( v, left, right ) =
                    separate parts

                b =
                    buildTree2 separate
            in
                Node ( v, b left, b right )


queryTree : (Vector -> Bool) -> VTree a -> Maybe a
queryTree test tree =
    case tree of
        Empty ->
            Nothing

        Leaf a ->
            Just a

        Node ( v, left, right ) ->
            if test v then
                queryTree test left
            else
                queryTree test right


type alias XY a =
    { a | x : Float, y : Float }


type alias XYZ =
    XY { z : Float }


type alias TriMesh a =
    List ( a, a, a )


meshToXY : TriMesh Vec3 -> TriMesh (XY { z : Float })
meshToXY mesh =
    mesh |> List.map (map3T V3.toRecord)


{-| Get a vector for inclusion testing from the RQ edge of 2D triangle PRQ.
Returns Nothing if the triangle has zero area.
CCW.
-}
edgeVector : XY a -> XY a -> XY a -> Maybe Vector
edgeVector p r q =
    let
        a =
            Vector (q.x - r.x) (q.y - r.y) 0

        b =
            Vector (p.x - r.x) (p.y - r.y) 0

        up =
            V.cross a b

        w =
            if up.z <= eps then
                Nothing
            else
                up |> V.cross b |> V.normalize

        bias =
            Maybe.map (\x -> V.dot x (Vector r.x r.y 0)) w
    in
        Maybe.map2 (\w b -> Vector w.x w.y b) w bias


{-| x,y coordinates define plane normal. z coordinate is offset.
-}
test2DTriangle : Vector -> ( XY a, XY a, XY a ) -> Bool
test2DTriangle v tri =
    let
        test x =
            (V.dot (Vector x.x x.y -1) v) >= eps
    in
        -- tri |> shrinkTriangle 0.01 |> map3L test |> List.any identity
        tri |> map3L identity |> List.any test


leftRightSplit : (Vector -> ( XY a, XY a, XY a ) -> Bool) -> Vector -> TriMesh (XY a) -> ( TriMesh (XY a), TriMesh (XY a) )
leftRightSplit test v triangles =
    let
        left =
            triangles
                |> List.filter (test v)

        right =
            triangles
                |> List.filter (test (V.scale -1 v))
    in
        ( left, right )


{-| only takes upward facing triangles
-}
splitTriangles : TriMesh (XY a) -> ( Vector, TriMesh (XY a), TriMesh (XY a) )
splitTriangles inputTriangles =
    let
        triangles =
            inputTriangles

        -- |> List.filter (\( p, q, r ) -> edgeVector p q r |> isJust)
        points =
            triangles |> List.concatMap (map3L identity)

        v =
            split2D points

        ( left, right ) =
            leftRightSplit test2DTriangle v triangles

        trouble =
            max (List.length left) (List.length right) >= List.length triangles
    in
        if trouble then
            let
                -- want the split that decreases the max # of triangles in a branch
                score v_ =
                    triangles
                        |> leftRightSplit test2DTriangle v_
                        |> (\( a, b ) -> max (List.length a) (List.length b))

                v2 =
                    triangles
                        |> List.map (map3R edgeVector)
                        |> List.concatMap (\( a, b, c ) -> [ a, b, c ])
                        |> List.filterMap identity
                        |> List.sortBy score
                        |> List.head
                        |> Maybe.withDefault (Vector 1 0 0)

                ( left2, right2 ) =
                    leftRightSplit test2DTriangle v2 triangles

                -- z =
                --     Debug.log "trouble" ( List.length left, List.length right, List.length left2, List.length right2 )
            in
                ( v2, left2, right2 )
        else
            ( v, left, right )


split2D : List (XY a) -> Vector
split2D parts =
    let
        points =
            parts |> List.map (\{ x, y } -> Vector x y 0)

        sum =
            List.foldl V.add (Vector 0 0 0)

        centroid =
            sum points
                |> V.scale (1 / (List.length points |> toFloat))

        cov =
            points
                |> List.map (\x -> V.sub x centroid)
                |> List.map V.toTuple
                |> List.map (\( x, y, z ) -> Vector (x * x) (x * y) (y * y))
                |> sum

        ( a, b, c, d ) =
            ( cov.x, cov.y, cov.y, cov.z )

        trace =
            a + d

        det =
            a * d - b * c

        l1 =
            (trace / 2) + sqrt ((trace ^ 2 / 4) - det)

        v =
            if b == 0 then
                if a > d then
                    Vector 1 0 0
                else
                    Vector 0 1 0
            else
                Vector (l1 - d) c 0
                    |> V.normalize
                    |> Maybe.withDefault V.xAxis
    in
        Vector v.x v.y (V.dot v centroid)


{-| s is small
-}
shrinkTriangle : Float -> ( XY a, XY a, XY a ) -> ( XY a, XY a, XY a )
shrinkTriangle s ( p, q, r ) =
    let
        c =
            { x = (p.x + q.x + r.x) / 3
            , y = (p.y + q.y + r.y) / 3
            }

        shrink x =
            { x | x = x.x + (c.x - x.x) * s, y = x.y + (c.y - x.y) * s }
    in
        ( shrink p, shrink q, shrink r )


test2D : XY a -> Vector -> Bool
test2D { x, y } v =
    V.dot (Vector x y -1) v >= -eps


inTriangle : XY a -> ( XY b, XY b, XY b ) -> Bool
inTriangle pt triangle =
    let
        ( e1, e2, e3 ) =
            map3R edgeVector triangle

        f =
            test2D pt
    in
        case ( e1, e2, e3 ) of
            ( Just e1_, Just e2_, Just e3_ ) ->
                f e1_ && f e2_ && f e3_

            _ ->
                False
