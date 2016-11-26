module PTree exposing (worldTree, buildTree, Quad, Face, exampleTreeOrigin, exampleTreeBase)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import WebGL exposing (..)


worldTree : Quad -> Vec3 -> Mat4 -> List Renderable
worldTree quad origin perspective =
    let
        depth =
            5

        uniforms =
            { perspective = perspective }

        quads =
            (buildTree depth quad origin)

        drawableTree =
            Triangle <| List.concat <| List.indexedMap quadFace quads
    in
        [ render treeVertexShader treeFragmentShader drawableTree uniforms ]


toList : ( a, a ) -> List a
toList ( a, b ) =
    [ a, b ]


buildTree : Int -> Quad -> Vec3 -> List Quad
buildTree depth quad origin =
    let
        ( tLeft, tRight ) =
            treeMat quad origin

        fLeft =
            eachQuad (transform tLeft)

        fRight =
            eachQuad (transform tRight)

        transformer x =
            branchTransform fLeft fRight x |> toList
    in
        recurseBranch depth transformer [ Basics.identity ] |> List.map (\x -> x quad)


recurseBranch : Int -> (a -> List a) -> List a -> List a
recurseBranch n transformer acc =
    if n == 0 then
        acc
    else
        let
            accNew =
                (List.concatMap transformer acc)
        in
            acc ++ (recurseBranch (n - 1) transformer accNew)


branchTransform : (a -> a) -> (a -> a) -> (a -> a) -> ( a -> a, a -> a )
branchTransform fLeft fRight soFar =
    ( soFar << fLeft
    , soFar << fRight
    )



-- Define the mesh for a crate


type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }


type alias Face =
    List ( Vertex, Vertex, Vertex )


type alias Quad =
    ( Vec3, Vec3, Vec3, Vec3 )


eachQuad : (Vec3 -> Vec3) -> Quad -> Quad
eachQuad f q =
    let
        ( a, b, c, d ) =
            q
    in
        ( f a, f b, f c, f d )



-- example input quad


exampleTreeBase : Quad
exampleTreeBase =
    ( vec3 0.1 1.9 0
    , vec3 -0.7 0.7 0
    , vec3 -1 -1 0
    , vec3 1.5 -1 0
    )


exampleTreeOrigin : Vec3
exampleTreeOrigin =
    vec3 0 2 0


angleBetween : Vec3 -> Vec3 -> Float
angleBetween a b =
    dot (normalize a) (normalize b)
        |> Basics.acos



-- Calculates affine transformation for left and right branches


treeMat : Quad -> Vec3 -> ( Mat4, Mat4 )
treeMat baseQuad treeOrigin =
    let
        ( a, b, c, d ) =
            baseQuad

        leftScale =
            length (sub treeOrigin a) / length (sub c d)

        rightScale =
            length (sub treeOrigin b) / length (sub c d)

        leftAngle =
            angleBetween (sub treeOrigin a) (sub c d)

        rightAngle =
            angleBetween (sub treeOrigin b) (sub d c)
    in
        ( M4.identity
            |> translate a
            |> M4.scale3 leftScale leftScale leftScale
            |> rotate (-1 * leftAngle) k
            |> translate (V3.negate d)
        , M4.identity
            |> translate b
            |> M4.scale3 rightScale rightScale rightScale
            |> rotate (rightAngle) k
            |> translate (V3.negate c)
        )



-- Turns Quad of 3-vectors into mesh of Vertex triangles


quadFace : Int -> Quad -> Face
quadFace depth quad =
    let
        ( a, b, c, d ) =
            quad

        start =
            vec3 0.8 0.1 0.1

        end =
            vec3 0.9 0.8 0.8

        depth_ =
            toFloat (depth + 1)

        shade =
            V3.scale ((depth_ - 1) / depth_) end
                |> V3.add (V3.scale (1 / depth_) start)

        a_ =
            Vertex a shade

        b_ =
            Vertex b shade

        c_ =
            Vertex c shade

        d_ =
            Vertex d shade
    in
        [ ( a_, b_, c_ )
        , ( a_, c_, d_ )
        ]



-- Shaders
-- vertexShader magically knows attributes (Vertex) must have one field for each
-- attribute in the GLSL shader code
-- variables


treeVertexShader : Shader Vertex { u | perspective : Mat4 } { vcoord : Vec3 }
treeVertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec3 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord;
}

|]


treeFragmentShader : Shader {} { u | perspective : Mat4 } { vcoord : Vec3 }
treeFragmentShader =
    [glsl|

precision mediump float;
varying vec3 vcoord;

void main () {
  gl_FragColor = vec4(vcoord, 1.);
}

|]
