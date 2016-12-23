module Island.Geometry exposing (..)

import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Island.Types exposing (..)
import List.Extra
import Dict
import Dict.Extra exposing (groupBy)
import WebGL


map3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map3 f ( a, b, c ) =
    ( f a, f b, f c )


map3L : (a -> b) -> ( a, a, a ) -> List b
map3L f ( a, b, c ) =
    [ f a, f b, f c ]


eachMeshPoint : (Vec3 -> Vec3) -> RawMesh -> RawMesh
eachMeshPoint f mesh =
    mesh |> List.map (map3 f)


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
                ( { a | normal = normal, neighbors = a.neighbors ++ [ ( b.index, a.index, c.index ) ] }
                , { b | normal = normal, neighbors = a.neighbors ++ [ ( c.index, b.index, a.index ) ] }
                , { c | normal = normal, neighbors = a.neighbors ++ [ ( a.index, c.index, b.index ) ] }
                )
    in
        mesh
            |> List.map (map3 getter)
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
            |> List.map (map3 setter)


invertNormals : RawMesh -> RawMesh
invertNormals mesh =
    mesh |> List.map (\( a, b, c ) -> ( a, c, b ))


invertIndexedNormals : Mesh -> Mesh
invertIndexedNormals mesh =
    mesh |> List.map (map3 (\a -> { a | normal = V3.scale -1 a.normal }))


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
        mesh |> List.map (map3 toAttribute) |> WebGL.Triangle


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
        mesh |> List.map (map3 toAttribute) |> WebGL.Triangle
