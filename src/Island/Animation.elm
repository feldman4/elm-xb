module Island.Animation exposing (..)

import Math.Vector3 as V3 exposing (vec3, Vec3)


-- import Math.Vector4 as V4 exposing (vec4, Vec4)

import Island.Types exposing (..)
import Frame exposing (Frame)
import Vector exposing (Vector)


-- import Quaternion

import Time exposing (Time)


type Substance
    = Water
    | Air


{-| Motion of a partially submerged buoyant rigid body, also known
as a boat. The water frame is centered at the boat position and
oriented so the z axis coincides with the surface normal.
-}
waterMotion : Frame.Frame -> Time.Time -> MotionObject -> MotionObject
waterMotion waterFrame dt object =
    let
        nodeToForce node =
            let
                position =
                    node.position |> Vector.fromVec3

                velocity =
                    getVelocity object.lastFrame object.frame dt position

                ( beta, rho ) =
                    getSubstance waterFrame node
                        |> (\s -> ( viscosity s, density s ))

                forceG =
                    vec3 0 0 (node.mass * -9.8)

                forceB =
                    vec3 0 0 (-1 * rho)

                -- drag
                forceD =
                    velocity |> Vector.scale -beta

                torque =
                    Vector.cross position
            in
                3
    in
        object


getVelocity : Frame -> Frame -> Time -> Vector -> Vector
getVelocity oldFrame newFrame dt position =
    let
        oldPosition =
            Frame.transformInto oldFrame position

        newPosition =
            Frame.transformInto newFrame position
    in
        Vector.sub newPosition oldPosition
            |> Vector.scale (1.0 / dt)


getSubstance : Frame -> Node -> Substance
getSubstance frame { position } =
    let
        substance z =
            if z > 0 then
                Air
            else
                Water
    in
        Frame.transformInto frame (Vector.fromVec3 position)
            |> Vector.getZ
            |> substance


viscosity : Substance -> Float
viscosity substance =
    case substance of
        Water ->
            1

        Air ->
            0


density : Substance -> Float
density substance =
    case substance of
        Water ->
            2

        Air ->
            0
