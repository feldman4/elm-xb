module Island.Geometry exposing (..)

import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Island.Types exposing (..)
import List.Extra
import Dict
import Dict.Extra exposing (groupBy)
import WebGL
import Collision
import Vector as V exposing (Vector)
import Quaternion as Q
import Frame exposing (Frame)


{-| Tolerance for plane tests and the like.
-}
eps : Float
eps =
    0.001


map3T : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map3T f ( a, b, c ) =
    ( f a, f b, f c )


map3L : (a -> b) -> ( a, a, a ) -> List b
map3L f ( a, b, c ) =
    [ f a, f b, f c ]


map3R : (a -> a -> a -> b) -> ( a, a, a ) -> ( b, b, b )
map3R f ( p, q, r ) =
    ( f p q r, f q r p, f r p q )


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


frameToWFrame : Frame -> WFrame
frameToWFrame frame =
    { position = frame.position
    , omega = Q.toVector frame.orientation
    , omegaInst = Vector 0 0 0
    }


wFrameToFrame : WFrame -> Frame
wFrameToFrame wFrame =
    { position = wFrame.position
    , orientation = Q.fromVector (V.add wFrame.omega wFrame.omegaInst)
    }


type VTree a
    = Node ( Vector, VTree a, VTree a )
    | Leaf a
    | Empty


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


meshToXY : RawMesh -> TriMesh (XY { z : Float })
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


test2DTriangle : Vector -> ( XY a, XY a, XY a ) -> Bool
test2DTriangle v tri =
    let
        test x =
            (V.dot (Vector x.x x.y -1) v) >= eps
    in
        -- tri |> shrinkTriangle 0.01 |> map3L test |> List.any identity
        tri |> map3L test |> List.any identity


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
                |> List.filter (\( p, q, r ) -> edgeVector p q r |> isJust)

        points =
            triangles |> List.concatMap (map3L identity)

        v =
            split2D points

        ( left, right ) =
            leftRightSplit test2DTriangle v triangles

        trouble =
            max (List.length left) (List.length right) >= List.length triangles

        z =
            Debug.log "# triangles:" (List.length triangles)
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

                z =
                    Debug.log "trouble" ( List.length left2, List.length right2, v2, triangles )
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


type alias M2 =
    ( Float, Float, Float, Float )


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


invert2x2 : M2 -> Maybe M2
invert2x2 m =
    let
        ( a, b, c, d ) =
            m

        det =
            a * d - b * c
    in
        if det < 1.0e-3 then
            Nothing
        else
            Just ( d / det, -b / det, -c / det, a / det )


{-| Reflect vector through normal.
-}
reflect : Vector -> Vector -> Vector
reflect n x =
    let
        n_ =
            V.normalize n
                |> Maybe.withDefault V.zAxis
    in
        n_
            |> V.scale (1 * (V.dot n_ x))
            |> V.sub x



--
-- toBarycentric : (Vec3, Vec3, Vec3) -> Vec3 -> Vec3
-- weighted : Float -> Float -> Float


scale3D : Vec3 -> RawMesh -> RawMesh
scale3D scale mesh =
    let
        ( x, y, z ) =
            V3.toTuple scale

        f v =
            vec3 (x * V3.getX v) (y * V3.getY v) (z * V3.getZ v)
    in
        mesh |> List.map (map3T f)


eachV : (Float -> Float) -> Vector -> Vector
eachV f v =
    Vector (f v.x) (f v.y) (f v.z)


type Dir
    = LeftTurn
    | RightTurn
    | Straight


makeZHull : RawMesh -> List ( Float, Float )
makeZHull mesh =
    let
        toXY v =
            ( v.x, v.y )

        uniqueXY mesh =
            mesh
                |> List.concatMap (map3L (V.fromVec3 >> toXY))
                |> List.Extra.unique
    in
        mesh |> uniqueXY |> hull


{-| From notnew/elm-hull. Not sure old-style .head adapted correctly.
-}
hull : List ( Float, Float ) -> List ( Float, Float )
hull pts =
    let
        diff ( x, y ) ( x_, y_ ) =
            ( x - x_, y - y_ )

        direction ( x, y ) ( x_, y_ ) =
            let
                cross =
                    x * y_ - y * x_

                -- z component of cross product
            in
                if cross < 0 then
                    LeftTurn
                else if cross > 0 then
                    RightTurn
                else
                    Straight

        getDirection a b c =
            direction (diff b a) (diff c b)

        sorted =
            List.sortBy Tuple.first pts

        top =
            go LeftTurn (List.tail sorted |> Maybe.withDefault []) (List.take 1 sorted)

        bottom =
            go RightTurn (List.tail sorted |> Maybe.withDefault []) (List.take 1 sorted)

        go dir pts stack =
            case ( pts, stack ) of
                ( [], s ) ->
                    s

                ( p :: ps, s :: [] ) ->
                    go dir ps (p :: stack)

                ( p :: ps, b :: a :: ss ) ->
                    if getDirection a b p == dir then
                        go dir ps (p :: stack)
                    else
                        go dir pts (a :: ss)

                ( _, [] ) ->
                    []
    in
        top ++ List.reverse bottom |> List.Extra.unique


eachV2 : (Float -> Float -> Float) -> Vector -> Vector -> Vector
eachV2 f a b =
    Vector (f a.x b.x) (f a.y b.y) (f a.z b.z)


makeBounds : RawMesh -> Collision.Bounds
makeBounds rawMesh =
    let
        toFace ( a, b, c ) =
            Collision.face (V.fromVec3 a) (V.fromVec3 b) (V.fromVec3 c)
    in
        rawMesh |> List.map toFace |> Collision.create


eachMeshPoint : (Vec3 -> Vec3) -> RawMesh -> RawMesh
eachMeshPoint f mesh =
    mesh |> List.map (map3T f)


addPQ : a -> PQ { mesh : a }
addPQ mesh =
    { mesh = mesh, position = vec3 0 0 0, quaternion = vec4 0 0 0 0 }


indexMesh : RawMesh -> Mesh
indexMesh mesh =
    let
        dict =
            mesh
                |> List.concatMap (\( a, b, c ) -> [ a, b, c ])
                |> List.Extra.uniqueBy toString
                |> List.indexedMap (\a b -> ( toString b, a ))
                |> Dict.fromList

        -- turn (Vec3,)*3 into (Vertex,)*3
        getter v =
            { index = Dict.get (toString v) dict |> Maybe.withDefault -1
            , position = v
            , normal = vec3 0 0 0
            , neighbors = []
            }

        setter ( a, b, c ) =
            let
                normal =
                    toNormal ( a.position, b.position, c.position )
            in
                ( { a | normal = normal, neighbors = a.neighbors ++ [ ( c.index, a.index, b.index ) ] }
                , { b | normal = normal, neighbors = a.neighbors ++ [ ( a.index, b.index, c.index ) ] }
                , { c | normal = normal, neighbors = a.neighbors ++ [ ( b.index, c.index, a.index ) ] }
                )
    in
        mesh
            |> List.map (map3T getter)
            |> List.map setter



-- populateNeighbors : Mesh -> Mesh
-- populateNeighbors mesh =
--   let
--     meshEmpty = mesh |> List.map (map3T (\v -> {v | neighbors = []}))
--     dict =


{-| Surface normal, normalized. Points are in counter-clockwise orientation when facing
the normal.
-}
toNormal : ( Vec3, Vec3, Vec3 ) -> Vec3
toNormal ( a, b, c ) =
    V3.cross (V3.sub c b) (V3.sub a b) |> V3.normalize


{-| Calculate average of all normals at each vertex. This kind of
assumes that we use triangles to represent smooth surfaces, not sharp corners.
-}
useCornerNormals : Mesh -> Mesh
useCornerNormals mesh =
    let
        f ( a, b, c ) =
            [ { index = a.index, normal = toNormal ( b.position, a.position, c.position ) }
            , { index = b.index, normal = toNormal ( c.position, b.position, a.position ) }
            , { index = c.index, normal = toNormal ( a.position, c.position, b.position ) }
            ]

        normSum vectors =
            let
                n =
                    List.length vectors |> toFloat
            in
                vectors |> List.foldl V3.add (vec3 0 0 0) |> V3.scale (1 / n)

        normDict =
            mesh
                |> List.concatMap f
                |> groupBy .index
                |> Dict.map (\_ a -> List.map .normal a |> normSum)

        setter a =
            { a | normal = Dict.get a.index normDict |> Maybe.withDefault a.normal }
    in
        mesh
            |> List.map (map3T setter)


invertNormals : RawMesh -> RawMesh
invertNormals mesh =
    mesh |> List.map (\( a, b, c ) -> ( a, c, b ))


invertIndexedNormals : Mesh -> Mesh
invertIndexedNormals mesh =
    mesh |> List.map (map3T (\a -> { a | normal = V3.scale -1 a.normal }))


offset : RawMesh -> Vec3 -> RawMesh
offset mesh x =
    let
        f ( a, b, c ) =
            ( V3.add a x, V3.add b x, V3.add c x )
    in
        mesh |> List.map f


centroid : RawMesh -> Vec3
centroid mesh =
    let
        f ( a, b, c ) =
            V3.add (V3.add a b) c

        n =
            (List.length mesh) * 3 |> toFloat
    in
        mesh
            |> List.map f
            |> List.foldl V3.add (vec3 0 0 0)
            |> V3.scale (1 / n)


{-| Use it with List.concatMap
-}
quadToTri : Quad -> RawMesh
quadToTri quad =
    let
        ( a, b, c, d ) =
            quad
    in
        [ ( a, b, c ), ( a, c, d ) ]


{-| Make a rectangular grid with integer spacing.
-}
makeGrid : Int -> Int -> List Quad
makeGrid height width =
    let
        k =
            0

        quad ( i, j ) =
            ( vec3 i j k
            , vec3 (i + 1) j k
            , vec3 (i + 1) (j + 1) k
            , vec3 i (j + 1) k
            )

        rows =
            List.range 0 width |> List.map toFloat

        cols =
            List.range 0 height |> List.map toFloat
    in
        rows
            |> List.concatMap (\i -> List.map (\j -> ( i, j )) cols)
            |> List.map quad


meshToTriangle : Mesh -> WebGL.Drawable Attribute
meshToTriangle mesh =
    let
        toAttribute vertex =
            { position = vertex.position
            , coords = vertex.position
            , normal = vertex.normal
            }
    in
        mesh |> List.map (map3T toAttribute) |> WebGL.Triangle


edgedMeshToTriangle : Mesh -> WebGL.Drawable EdgedVertex
edgedMeshToTriangle mesh =
    let
        dict =
            mesh
                |> List.concatMap (map3L identity)
                |> groupBy .index

        -- represents an empty
        emptyVector =
            vec3 0.11 0.11 0.11

        getPosition i =
            Dict.get i dict |> Maybe.andThen List.head |> Maybe.map .position |> Maybe.withDefault emptyVector

        firstLast ( a, b, c ) =
            [ a, c ]

        flattenedNeighbors vertex =
            Dict.get vertex.index dict
                |> Maybe.withDefault []
                |> List.concatMap (\v -> v.neighbors |> List.concatMap firstLast)
                |> List.take 6
                |> List.map getPosition

        toAttribute vertex =
            let
                neighbors =
                    flattenedNeighbors vertex

                get i =
                    neighbors |> List.drop (i - 1) |> List.head |> Maybe.withDefault emptyVector
            in
                { position = vertex.position
                , coords = vec3 0 0 0
                , normal = vertex.normal
                , n1 = get 1
                , n2 = get 2
                , n3 = get 3
                , n4 = get 4
                , n5 = get 5
                , n6 = get 6
                }
    in
        mesh
            |> List.map (map3T toAttribute)
            |> WebGL.Triangle
