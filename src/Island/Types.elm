module Island.Types exposing (..)

import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Minimum


type alias Mesh =
    List Tri


type alias Tri =
    ( Vec3, Vec3, Vec3 )


type alias Quad =
    ( Vec3, Vec3, Vec3, Vec3 )


type alias Light =
    Vec3


{-| For rendering
-}
type alias Vertex =
    { position : Vec3
    , color : Vec3
    , normal : Vec3
    }


type alias PQ a =
    { a | position : Vec3, quaternion : Vec4 }


type alias MeshPQ =
    PQ { mesh : Mesh }


type alias Modeled =
    { boat : MeshPQ, island : MeshPQ, sea : MeshPQ }


type alias Model =
    Minimum.Model Modeled
