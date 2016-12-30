module Island.Types exposing (..)

import Math.Vector3 as V3 exposing (vec3, Vec3)
import Math.Vector4 as V4 exposing (vec4, Vec4)
import Math.Matrix4 as M4 exposing (Mat4)
import WebGL exposing (Texture, Error)
import Minimum
import Frame exposing (Frame)
import Vector exposing (Vector)
import EveryDict
import Collision


type NamedInteraction
    = Select
    | Deselect
    | Follow Follow
    | RequestOcean
    | ResolveCollisions


type NamedEffect
    = Control
    | MainControl Float
    | View
    | Floating FloatingInfo
    | Gravity Float
    | Collide



-- g


type Action
    = MinAction Minimum.Action
    | TextureError Error
    | TextureLoaded ( NamedTexture, Texture )
    | WaterIndicator ( String, ( Float, Float, Float, Float ) )


{-|
- coordinates : Nothing indicates out of bounds
-}
type alias FloatingInfo =
    { coordinates : Maybe ( Float, Float )
    , moved : Bool
    , requested : Bool
    , height : Float
    }


defaultFloating : FloatingInfo
defaultFloating =
    { coordinates = Nothing, height = 0, moved = False, requested = False }



-- RENDERING


type Thing
    = LightCube
    | Boat
    | Face
    | SeaSphere
    | Ocean
    | Island


type NamedTexture
    = Crate
    | Thwomp
    | DisplacementMap
    | NormalMap


type alias RawMesh =
    List ( Vec3, Vec3, Vec3 )


type alias BoundedDrawable a =
    { drawable : WebGL.Drawable a
    , bounds : Collision.Bounds
    }


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



-- OBJECTS, EFFECTS, INTERACTIONS


type alias Particle =
    { position : Vec3, mass : Float }


type alias Motion a =
    { a | lastFrame : Frame.Frame }


type alias Rigid a =
    { a | nodes : List Particle }


{-|
- .velocity is for objects that can be moved (matched by physics)
-}
type alias Object =
    { drawable : Maybe Thing
    , material : Material
    , frame : Frame.Frame
    , scale : Vec3
    , effects : List NamedEffect
    , velocity : Maybe Vector
    }


type alias RenderableObject =
    { drawable : Thing
    , material : Material
    , frame : Frame.Frame
    , scale : Vec3
    }



-- | Vision
--


{-| Orbital: to get perspective,
-}
type Follow
    = FPS
    | Orbital Float


type alias Interaction =
    Model -> ( Model, Maybe NamedInteraction, Cmd Action )


type alias Effect =
    Object -> ( Object, Maybe NamedEffect )



-- NEXT


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
    , textures : EveryDict.EveryDict NamedTexture Texture
    , interactions : List NamedInteraction
    , camera : Frame
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
        , normalMatrix : Mat4
        , light : Vec3
        , viewer : Vec3
    }


type alias UniformColor u =
    { u | color : Vec4 }


type alias TextureUniforms u =
    { u | tex0 : Texture, tex1 : Texture }
