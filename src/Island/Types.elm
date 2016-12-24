module Island.Types exposing (..)

import Math.Vector3 as V3 exposing (vec3, Vec3)
import Math.Vector4 as V4 exposing (vec4, Vec4)
import Math.Matrix4 as M4 exposing (Mat4)
import WebGL exposing (Texture)
import Minimum
import Frame
import EveryDict


type Thing
    = LightCube
    | Boat
    | Face
    | SeaSphere
    | Ocean


type NamedTexture
    = Crate
    | Thwomp
    | DisplacementMap
    | NormalMap


type alias RawMesh =
    List ( Vec3, Vec3, Vec3 )


type alias Quad =
    ( Vec3, Vec3, Vec3, Vec3 )


{-| Basic component of a mesh. Neighbors should be ordered CCW when facing into
the normal.
-}
type alias Vertex =
    { index : Int
    , position : Vec3
    , normal : Vec3
    , neighbors : List ( Int, Int, Int )
    }


type alias Mesh =
    List ( Vertex, Vertex, Vertex )


type Material
    = Color Vec4
    | MaterialTexture NamedTexture
    | OceanTexture ( NamedTexture, NamedTexture )



-- type Object = Basic BasicObject | Motion MotionObject


type alias Object =
    GenericObject {}


type alias Particle =
    { position : Vec3, mass : Float }


type alias Motion a =
    { a | lastFrame : Frame.Frame }


type alias Rigid a =
    { a | nodes : List Particle }


type alias GenericObject a =
    { a
        | drawable : Thing
        , material : Material
        , frame :
            Frame.Frame
        , scale :
            Vec3
            -- doesn't account for scaling, can just multiply Mat4
        , effects : Effects a
    }


type Effects a
    = Effects (List (GenericObject a -> GenericObject a))


{-| For rendering
-}
type alias Attribute =
    { position : Vec3
    , coords : Vec3
    , normal : Vec3
    }


type alias PQ a =
    { a | position : Vec3, quaternion : Vec4 }


type alias MeshPQ =
    PQ { mesh : RawMesh }


type alias IndexedMeshPQ =
    PQ { mesh : Mesh }


type alias Modeled =
    { objects : List Object
    , sea : IndexedMeshPQ
    , gridSea : IndexedMeshPQ
    , textures : EveryDict.EveryDict NamedTexture Texture
    }


type alias Model =
    Minimum.Model Modeled



-- Shaders


type alias Edged a =
    { a | n1 : Vec3, n2 : Vec3, n3 : Vec3, n4 : Vec3, n5 : Vec3, n6 : Vec3 }


type alias EdgedVertex =
    Edged Attribute


type alias Varyings =
    { phongI : Vec3
    , phongL : Vec3
    , phongN : Vec3
    , phongV : Vec3
    , sources :
        Vec3
        -- , fDistance : Float
    }


type alias Uniforms u =
    { u
        | perspective : Mat4
        , transform : Mat4
        , light : Vec3
        , viewer : Vec3
    }


type alias UniformColor u =
    { u | color : Vec4 }


type alias TextureUniforms u =
    { u | tex0 : Texture, tex1 : Texture }
