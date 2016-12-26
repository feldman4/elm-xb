module Island.Render exposing (..)

import Math.Matrix4 as M4
import Math.Vector4 as V4 exposing (vec4, Vec4)
import WebGL exposing (Renderable, Shader, Texture, Error)
import Minimum
import Island.Types exposing (..)
import Island.Shaders exposing (..)
import Frame
import Island.Things exposing (..)
import EveryDict


{-| Render an BasicObject with a single light source, choosing shaders based on Material.
-}
renderObject : Minimum.Model { u | textures : EveryDict.EveryDict NamedTexture Texture } -> RenderableObject -> Renderable
renderObject { window, person, textures } object =
    let
        perspectiveMatrix =
            Minimum.perspective ( window.size.width, window.size.height ) person

        drawable =
            getThing object.drawable

        transform =
            M4.mul (object.frame |> Frame.toMat4) (M4.makeScale object.scale)

        defaultRender =
            { perspective = perspectiveMatrix
            , transform = transform
            , light = lightSource
            , viewer = person.position
            , color = vec4 0.8 0.8 0.8 1
            }
                |> WebGL.render colorVertexShader colorFragmentShader drawable
    in
        case object.material of
            Color color ->
                let
                    uniforms =
                        { perspective = perspectiveMatrix
                        , transform = transform
                        , light = lightSource
                        , viewer = person.position
                        , color = color
                        }
                in
                    WebGL.render colorVertexShader colorFragmentShader drawable uniforms

            MaterialTexture name ->
                case EveryDict.get name textures of
                    Just texture ->
                        let
                            uniforms =
                                { perspective = perspectiveMatrix
                                , transform = transform
                                , light = lightSource
                                , viewer = person.position
                                , texture = texture
                                }
                        in
                            WebGL.render textureVertexShader textureFragmentShader drawable uniforms

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
                                    , light = lightSource
                                    , viewer = person.position
                                    , color = vec4 0.9 0.9 1.0 1.0
                                    , displacement = dTexture
                                    , normals = nTexture
                                    }
                            in
                                WebGL.render oceanVertexShader colorFragmentShader drawable uniforms

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
