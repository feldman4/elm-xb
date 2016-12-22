module Island.Types exposing (..)

import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import WebGL exposing (Texture)
import Minimum


type alias Mesh =
    List ( Vec3, Vec3, Vec3 )


type alias IndexedPoint =
    { index : Int, point : Vec3, normal : Vec3, neighbors : List ( Int, Int, Int ) }


type alias IndexedMesh =
    List ( IndexedPoint, IndexedPoint, IndexedPoint )


type alias Quad =
    ( Vec3, Vec3, Vec3, Vec3 )


type alias Light =
    Vec3


{-| For rendering
-}
type alias Vertex =
    { position : Vec3
    , coords : Vec3
    , color : Vec3
    , normal : Vec3
    }


type alias PQ a =
    { a | position : Vec3, quaternion : Vec4 }


type alias MeshPQ =
    PQ { mesh : Mesh }


type alias IndexedMeshPQ =
    PQ { mesh : IndexedMesh }


type alias Modeled =
    { boat : IndexedMeshPQ
    , island : MeshPQ
    , sea : IndexedMeshPQ
    , gridSea : IndexedMeshPQ
    , textures : { tex0 : Maybe Texture, tex1 : Maybe Texture }
    }


type alias Model =
    Minimum.Model Modeled
