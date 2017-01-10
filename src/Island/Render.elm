module Island.Render exposing (..)

import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector4 as V4 exposing (vec4, Vec4)
import WebGL exposing (..)
import Island.Types exposing (..)
import Island.Shaders exposing (..)
import Frame exposing (Frame)
import Vector as V exposing (Vector)
import Island.Things exposing (..)
import EveryDict


config : List FunctionCall
config =
    let
        blendFunc =
            BlendFunc ( DstColor, DstColor )

        depthFunc =
            DepthFunc Always

        blendEquation =
            BlendEquation Subtract
    in
        [ Enable Blend, Enable DepthTest ]



--, depthFunc, blendEquation, blendFunc ]
--, ,  blendFunc, blendEquation ]


{-| Render an BasicObject with a single light source, choosing shaders based on Material.
-}
renderObject : Model -> Mat4 -> RenderableObject -> Renderable
renderObject { window, camera, textures } perspectiveMatrix object =
    let
        drawable =
            (getCached object.drawable).drawable

        transform =
            M4.mul (object.frame |> Frame.toMat4) (M4.makeScale (V.toVec3 object.scale))

        normalMatrix =
            object.frame
                |> Frame.setPosition (Vector 0 0 0)
                |> Frame.toMat4

        render =
            WebGL.renderWithConfig config

        defaultUniforms =
            { perspective = perspectiveMatrix
            , transform = transform
            , normalMatrix = normalMatrix
            , light = lightSource
            , viewer = camera.position |> V.toVec3
            , color = vec4 0.8 0.8 0.8 1
            }

        defaultRender =
            defaultUniforms
                |> render colorVertexShader colorFragmentShader drawable
    in
        case object.material of
            Color color ->
                let
                    uniforms =
                        { perspective = perspectiveMatrix
                        , transform = transform
                        , normalMatrix = normalMatrix
                        , light = lightSource
                        , viewer = camera.position |> V.toVec3
                        , color = color
                        , r = 3000
                        }
                in
                    render colorVertexShader colorFragmentShader drawable uniforms

            MaterialTexture name ->
                case EveryDict.get name textures of
                    Just texture ->
                        let
                            uniforms =
                                { perspective = perspectiveMatrix
                                , transform = transform
                                , normalMatrix = normalMatrix
                                , light = lightSource
                                , viewer = camera.position |> V.toVec3
                                , texture = texture
                                }
                        in
                            render textureVertexShader textureFragmentShader drawable uniforms

                    Nothing ->
                        -- object is missing texture
                        defaultRender

            OceanTexture ( name0, name1 ) ->
                let
                    displacement =
                        EveryDict.get name0 textures

                    normal =
                        EveryDict.get name1 textures
                in
                    case ( displacement, normal ) of
                        ( Just dTexture, Just nTexture ) ->
                            let
                                uniforms =
                                    { perspective = perspectiveMatrix
                                    , transform = transform
                                    , normalMatrix = normalMatrix
                                    , light = lightSource
                                    , viewer = camera.position |> V.toVec3
                                    , color = vec4 0.9 0.9 1.0 0.3
                                    , displacement = dTexture
                                    , normals = nTexture
                                    , r = 30
                                    , l = 15
                                    }
                            in
                                render oceanVertexShader colorFragmentShader drawable uniforms

                        _ ->
                            -- missing one or more ocean textures
                            defaultRender


{-| Only render if we have Just drawable
-}
renderMaybe : Object -> Maybe RenderableObject
renderMaybe object =
    case object.drawable of
        Just drawable ->
            Just
                { drawable = drawable
                , material = object.material
                , frame = object.frame
                , scale = object.scale
                }

        _ ->
            Nothing


{-| Load textures in order, using dummy render call. After this executes
textures should be bound sequentially to gl.TEXTURE
-}
renderDummyTextures : ( Texture, Texture ) -> Renderable
renderDummyTextures ( tex0, tex1 ) =
    let
        uniforms =
            { tex0 = tex0, tex1 = tex1 }

        drawable =
            WebGL.Triangle []
    in
        WebGL.render dummyVertexShader dummyFragmentShader drawable uniforms


{-| Makes the dummy render call. This function must be modified if textures
exposed to JS WebGL are modified.
-}
textureEntity : Model -> List Renderable
textureEntity model =
    let
        dummyFilter ( name, texture ) =
            case name of
                NormalMap ->
                    [ texture ]

                DisplacementMap ->
                    [ texture ]

                _ ->
                    []

        dummyTextures =
            EveryDict.toList model.textures
                |> List.concatMap dummyFilter
    in
        case dummyTextures of
            tex0 :: tex1 :: [] ->
                [ renderDummyTextures ( tex0, tex1 ) ]

            tex0 :: [] ->
                [ renderDummyTextures ( tex0, tex0 ) ]

            [] ->
                []

            _ ->
                let
                    a =
                        Debug.crash "More than 2 dummy textures."
                in
                    []


perspective : ( Int, Int ) -> Frame -> Mat4
perspective ( w, h ) frame =
    let
        origin =
            frame.position |> V.toVec3

        gaze =
            Frame.transformOutOf frame (Vector 1 0 0) |> V.toVec3

        up =
            (Vector 0 0 1) |> V.toVec3
    in
        M4.mul (M4.makePerspective 90 (toFloat w / toFloat h) 0.01 100)
            (M4.makeLookAt origin gaze up)


{-| Create orthographic projection matrix from camera Frame.
-}
orthoPerspective : Float -> Float -> Frame -> Mat4
orthoPerspective width height frame =
    let
        origin =
            frame.position |> V.toVec3

        gaze =
            Frame.transformOutOf frame (Vector 1 0 0) |> V.toVec3

        up =
            (Vector 0 0 1) |> V.toVec3

        ( left, right, bottom, top, znear, zfar ) =
            ( -width, width, -height, height, 0.1, 100 )

        pMat =
            M4.makeOrtho left right bottom top znear zfar

        camMat =
            M4.makeLookAt origin gaze up
    in
        M4.mul pMat camMat
