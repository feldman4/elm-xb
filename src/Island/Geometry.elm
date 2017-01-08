module Island.Geometry exposing (..)

import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Island.Types exposing (..)
import Utilities exposing (..)
import List.Extra
import Dict
import Dict.Extra exposing (groupBy)
import WebGL
import Collision
import Vector as V exposing (Vector)
import Quaternion as Q
import Frame exposing (Frame)


{-| Velocity expressed with rotation vector omega. Instantaneous component is
set to zero at the end of animate cycle, after displacement and collision
resolution. Damping of regular velocity occurs in Control Effect.
-}
frameToVFrame : Frame -> VFrame
frameToVFrame frame =
    { position = frame.position
    , positionInst = Vector 0 0 0
    , omega = Q.toVector frame.orientation
    , omegaInst = Vector 0 0 0
    }


vFrameToFrame : VFrame -> Frame
vFrameToFrame wFrame =
    { position = wFrame.position
    , orientation = Q.fromVector (V.add wFrame.omega wFrame.omegaInst)
    }


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


{-| Needed so long as RawMesh uses Math.Vector3 instead of Vector.
-}
scale3D : Vector -> RawMesh -> RawMesh
scale3D scale mesh =
    let
        f v =
            vec3 (scale.x * V3.getX v) (scale.y * V3.getY v) (scale.z * V3.getZ v)
    in
        mesh |> List.map (map3T f)


eachV : (Float -> Float) -> Vector -> Vector
eachV f v =
    Vector (f v.x) (f v.y) (f v.z)


eachV2 : (Float -> Float -> Float) -> Vector -> Vector -> Vector
eachV2 f a b =
    Vector (f a.x b.x) (f a.y b.y) (f a.z b.z)


{-| Used in convex hull calculation.
-}
type Dir
    = LeftTurn
    | RightTurn
    | Straight


{-| 2D convex hull using XY coordinates.
-}
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


makeBounds : RawMesh -> Collision.Bounds
makeBounds rawMesh =
    let
        toFace ( a, b, c ) =
            Collision.face (V.fromVec3 a) (V.fromVec3 b) (V.fromVec3 c)
    in
        rawMesh |> List.map toFace |> Collision.create


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


{-| Convert each Quad into mesh with 2 triangles. Use it with List.concatMap
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
