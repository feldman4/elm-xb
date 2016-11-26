module PTree exposing (..)

import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import WebGL exposing (..)
import Random.Pcg as Random
import Time exposing (Time)
import Spring exposing (Spring, animate, epsilon)


worldTree : Quad -> Vec3 -> Mat4 -> List Renderable
worldTree quad origin perspective =
    let
        depth =
            5

        uniforms =
            { perspective = perspective }

        quads =
            buildTree depth quad origin

        drawableTree =
            Triangle <| List.concat <| List.indexedMap quadFace quads
    in
        [ render treeVertexShader treeFragmentShader drawableTree uniforms ]


worldForest : List (TreeBase Quad) -> Mat4 -> List Renderable
worldForest treeBases perspective =
    let
        depth =
            5

        uniforms =
            { perspective = perspective }

        buildATree x =
            buildTree depth x.quad x.origin

        allQuads =
            List.map buildATree treeBases
                |> List.concatMap (List.indexedMap quadFace)
                |> List.concat

        drawableForest =
            Triangle <| allQuads

        blendFunc =
            BlendFunc ( SrcAlpha, DstAlpha )

        depthFunc =
            DepthFunc Always

        blendEquation =
            BlendEquation Add

        functionCalls =
            [ Enable Blend, Enable DepthTest, depthFunc, blendFunc, blendEquation ]
    in
        [ renderWithConfig functionCalls treeVertexShader treeFragmentShader drawableForest uniforms ]



-- ANIMATION


animateForest : Time.Time -> List (TreeBase SpringyQuad) -> List (TreeBase SpringyQuad)
animateForest dt forest =
    let
        animateBase base =
            { base | quad = eachQuad (animateVec3 dt) base.quad }
    in
        List.map animateBase forest


animateVec3 : Time.Time -> Spring Vec3 -> Spring Vec3
animateVec3 fpms spring =
    let
        frameRate =
            fpms / 1000

        fspring =
            V3.sub spring.position spring.destination
                |> V3.scale -spring.stiffness

        fdamper =
            V3.scale -spring.damping spring.velocity

        a =
            V3.add fspring fdamper

        newV =
            V3.scale frameRate a
                |> V3.add spring.velocity

        newX =
            V3.scale frameRate newV
                |> V3.add spring.position
    in
        if
            V3.length (V3.sub newV spring.velocity)
                < Spring.epsilon
                && V3.length (V3.sub newX spring.position)
                < Spring.epsilon
        then
            { spring
                | position = spring.destination
                , velocity = vec3 0 0 0
            }
        else
            { spring
                | position = newX
                , velocity = newV
            }



-- ANIMATION


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


type alias SpringyQuad =
    ( Spring.Spring Vec3, Spring.Spring Vec3, Spring.Spring Vec3, Spring.Spring Vec3 )


type alias Spring a =
    { stiffness : Float
    , damping : Float
    , position : a
    , velocity : a
    , destination : a
    }


quadToSQuad : Quad -> SpringyQuad
quadToSQuad ( a, b, c, d ) =
    let
        defaultSpring x =
            { stiffness = 1
            , damping = 0.1
            , position = x
            , velocity = vec3 0 0 0
            , destination = x
            }
    in
        ( defaultSpring a, defaultSpring b, defaultSpring c, defaultSpring d )


sQuadToQuad : SpringyQuad -> Quad
sQuadToQuad ( a, b, c, d ) =
    ( a.position, b.position, c.position, d.position )


despring : TreeBase SpringyQuad -> TreeBase Quad
despring base =
    { quad = sQuadToQuad base.quad
    , origin = base.origin
    }


type alias TreeBase q =
    { quad : q
    , origin : Vec3
    }


eachQuad : (a -> a) -> ( a, a, a, a ) -> ( a, a, a, a )
eachQuad f q =
    let
        ( a, b, c, d ) =
            q
    in
        ( f a, f b, f c, f d )


mapQuad : (a -> Vec3 -> Vec3) -> ( a, a, a, a ) -> Quad -> Quad
mapQuad f ( a, b, c, d ) q =
    let
        ( a_, b_, c_, d_ ) =
            q
    in
        ( f a a_, f b b_, f c c_, f d d_ )



-- example input quad


exampleTreeBase : Quad
exampleTreeBase =
    ( vec3 0.5 0.8 0
    , vec3 -0.5 1.0 0
    , vec3 -1 -1 0
    , vec3 1.5 -1 0
    )


exampleTreeOrigin : Vec3
exampleTreeOrigin =
    vec3 0 2 0


exampleForest : List (TreeBase Quad)
exampleForest =
    let
        numTrees =
            20

        seed0 =
            Random.initialSeed 22783283

        ( treeOffsets, seed1 ) =
            Random.map3 vec3 (Random.float -10 10) (Random.float -2 4) (Random.float -10 10)
                |> Random.list numTrees
                |> flip Random.step seed0

        smallFloat =
            Random.float -0.8 0.8

        tuple4 a b c d =
            ( a, b, c, d )

        ( quadOffsets, seed2 ) =
            Random.map3 vec3 smallFloat smallFloat (Random.constant 0)
                |> (\a -> Random.map4 tuple4 a a a a)
                |> Random.list numTrees
                |> flip Random.step seed1

        a =
            Debug.log "offsets" quadOffsets

        applyTreeOffset x =
            { quad = eachQuad (V3.add x) exampleTreeBase
            , origin = V3.add exampleTreeOrigin x
            }

        applyQuadOffsets x y =
            { y | quad = mapQuad V3.add x y.quad }
    in
        List.map applyTreeOffset treeOffsets
            |> List.map2 applyQuadOffsets quadOffsets


exampleSpringyForest : List { origin : Vec3, quad : SpringyQuad }
exampleSpringyForest =
    let
        kick x =
            { x | velocity = vec3 0.5 0.5 0 }

        addSpring base =
            { base | quad = quadToSQuad base.quad |> eachQuad kick }
    in
        List.map addSpring exampleForest


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

        zOffset =
            vec3 0 0 0.01
    in
        ( M4.identity
            |> translate a
            |> M4.scale3 leftScale leftScale leftScale
            |> rotate (-1 * leftAngle) k
            |> translate (V3.negate d)
            |> translate zOffset
        , M4.identity
            |> translate b
            |> M4.scale3 rightScale rightScale rightScale
            |> rotate (rightAngle) k
            |> translate (V3.negate c)
            |> translate zOffset
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
  gl_FragColor = vec4(vcoord, 0.8);
}

|]
