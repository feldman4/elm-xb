port module Island.Effects exposing (..)

import Island.Types exposing (..)
import Island.Geometry exposing (scale3D, vFrameToFrame, reflect, eachV2)
import Island.Things exposing (getCached, toBody, boat, island, cube)
import Island.Ports exposing (requestOceanPort)
import Utilities exposing (..)
import VTree exposing (queryTree, test2D, inTriangle)
import Minimum as M exposing (angleBetween, foldla, toButton)
import Frame exposing (Frame)
import Math.Vector4 as V4 exposing (vec4)
import Vector as V exposing (Vector)
import Quaternion as Q exposing (Quaternion)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Time exposing (Time)
import Collision
import Collision.Tree
import Set
import Dict


{-| Folds over all Objects, pattern matching on every NamedEffect and calling
relevant functions to update Object. Order of NamedEffects within an Object
matters!
-}
effectManager : NamedEffect -> Model -> Time -> Object -> ( Object, Maybe NamedEffect )
effectManager namedEffect model dt object =
    case namedEffect of
        Motion ->
            ( motion dt object, Just Motion )

        Control ->
            -- pick up and hold
            ( { object | frame = object.frame |> Frame.setPosition (Frame.transformOutOf model.camera (Vector 2 0 0)) }
            , Just Control
            )

        MainControl speed ->
            case
                object.velocity
            of
                Just vFrame ->
                    let
                        newVFrame =
                            control model (dt * speed * 0.001) object.frame vFrame
                    in
                        -- speed is in units per second, dt in milliseconds
                        -- shouldn't have to set frame (turning)
                        ( { object | velocity = Just newVFrame }
                        , Just (MainControl speed)
                        )

                Nothing ->
                    ( object, Just (MainControl speed) )

        View ->
            ( object, Just View )

        Floating floatingInfo ->
            floatBoat floatingInfo object

        Gravity g ->
            -- dt in milliseconds
            ( gravity (g * 0.001) dt object, Just (Gravity g) )

        Collide x ->
            ( object, Just (Collide x) )

        Delete ->
            ( object, Just Delete )

        Pause effects ->
            ( object, Just (Pause effects) )


{-| Pattern matches a single (Action (Msg), NamedInteraction) pair. Returns an
Interaction function.
-}
interactionManager : Action -> NamedInteraction -> Maybe Interaction
interactionManager action name =
    case ( name, action ) of
        ( Select, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.B) ->
                    if bool then
                        Just (select |> noAction)
                    else
                        Nothing

                _ ->
                    Nothing

        ( Select, _ ) ->
            Nothing

        ( Deselect, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.B) ->
                    if bool then
                        Just (deselect |> noAction)
                    else
                        Nothing

                _ ->
                    Nothing

        ( Deselect, _ ) ->
            Nothing

        ( Follow FPS, MinAction (M.Animate dt) ) ->
            Just (followFPS |> noAction)

        ( Follow (Orbital theta), MinAction (M.Animate dt) ) ->
            Just (followOrbital theta dt |> noAction)

        ( Follow _, _ ) ->
            Nothing

        ( RequestOcean, MinAction (M.Animate dt) ) ->
            Just requestOcean

        ( RequestOcean, _ ) ->
            Nothing

        ( ResolveCollisions, MinAction (M.Animate dt) ) ->
            Just (resolveCollisions dt |> noAction)

        ( ResolveCollisions, _ ) ->
            Nothing

        ( Deletion, MinAction (M.Animate dt) ) ->
            Just (delete |> noAction)

        ( Deletion, _ ) ->
            Nothing

        ( PauseExcept exceptGroup, MinAction (M.Animate dt) ) ->
            Just (pauseExcept exceptGroup |> noAction)

        ( PauseExcept exceptGroup, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.X) ->
                    if bool then
                        Just ((\m -> ( pauseSwitch m, Just name )) |> noAction)
                    else
                        Nothing

                _ ->
                    Nothing

        ( PauseExcept _, _ ) ->
            Nothing


clampVelocity : Float -> Float -> Vector -> Vector
clampVelocity xyr zr velocity =
    let
        ( x, y, z ) =
            V.toTuple velocity

        v_xy =
            Vector x y 0

        m =
            V.length v_xy |> clamp -xyr xyr

        z_ =
            clamp -zr zr z

        v =
            v_xy |> V.normalize |> Maybe.withDefault (Vector 0 0 0) |> V.scale m |> V.add (Vector 0 0 z_)
    in
        v


motion : Float -> Object -> Object
motion dt object =
    case object.velocity of
        Just vFrame ->
            let
                newFrame =
                    object.frame
                        |> Frame.extrinsicNudge (V.scale (dt / 30) vFrame.position)
                        |> Frame.intrinsicRotate (Q.fromVector (V.scale (dt / 30) vFrame.omegaInst))
                        |> Frame.extrinsicRotate (Q.fromVector (V.scale (dt / 30) vFrame.omega))
            in
                { object | frame = newFrame }

        Nothing ->
            object


undoMotion : Float -> Object -> Object
undoMotion dt object =
    case object.velocity of
        Just vFrame ->
            let
                newFrame =
                    object.frame
                        |> Frame.extrinsicRotate (Q.conjugate (Q.fromVector (V.scale (dt / 30) vFrame.omega)))
                        |> Frame.intrinsicRotate (Q.conjugate (Q.fromVector (V.scale (dt / 30) vFrame.omegaInst)))
                        |> Frame.extrinsicNudge (V.scale (dt / 30) (V.scale -1 vFrame.position))
            in
                { object | frame = newFrame }

        Nothing ->
            object


{-| Switch an object's pause state. Works on everything in range.
-}
pauseSwitch : Model -> Model
pauseSwitch model =
    let
        ( maxAngle, maxDistance ) =
            ( 0.4, 10 )

        canGrab =
            grab maxAngle maxDistance model

        isPause effect =
            case effect of
                Pause _ ->
                    True

                _ ->
                    False

        isMainControl effect =
            case effect of
                MainControl _ ->
                    True

                _ ->
                    False

        switch obj =
            case List.filter isPause obj.effects |> List.head of
                Just (Pause effects) ->
                    { obj | effects = effects ++ (List.filter (not << isPause) obj.effects) }

                _ ->
                    { obj | effects = [ Pause [] ] ++ obj.effects }

        apply obj =
            if canGrab obj && not (List.any isMainControl obj.effects) then
                switch obj
            else
                obj
    in
        { model | objects = List.map apply model.objects }


{-| Move each object's Effects into a Pause Effect, unless they are in the
exception group.
-}
pauseExcept : EffectGroup -> Model -> ( Model, Maybe NamedInteraction )
pauseExcept exceptGroup model =
    let
        isPause effect =
            case effect of
                Pause _ ->
                    True

                _ ->
                    False

        isMainControl effect =
            case effect of
                MainControl _ ->
                    True

                _ ->
                    False

        shouldPause effect =
            not (inEffectGroup exceptGroup effect)

        pause obj =
            case List.filter isPause obj.effects |> List.head of
                Just (Pause effects) ->
                    let
                        ( a, b ) =
                            List.filter (not << isPause) obj.effects
                                |> List.partition shouldPause
                    in
                        [ Pause (effects ++ a) ] ++ b

                _ ->
                    -- this pauses all objects
                    -- [ Pause obj.effects ]
                    obj.effects

        newObjects =
            model.objects
                |> List.map (\obj -> { obj | effects = pause obj })
    in
        ( { model | objects = newObjects }, Just (PauseExcept exceptGroup) )


inEffectGroup : EffectGroup -> NamedEffect -> Bool
inEffectGroup group effect =
    case group of
        AllEffects ->
            True

        NoEffects ->
            False

        CollideOnly ->
            case effect of
                Collide _ ->
                    True

                _ ->
                    False


delete : Model -> ( Model, Maybe NamedInteraction )
delete model =
    let
        newObjects =
            model.objects
                |> List.filter (\x -> List.member Delete x.effects)
    in
        ( { model | objects = newObjects }, Just Deletion )


{-| Reverse motion, set velocity to zero. No contact position/normal.
-}
undoCollision : Float -> Object -> Object
undoCollision dt object =
    case object.velocity of
        Nothing ->
            object

        Just v ->
            let
                z =
                    Debug.log "collision undone" object.drawable

                newObject =
                    object |> undoMotion dt

                reversed =
                    Vector v.position.x v.position.y 0.05

                zeroVelocity =
                    { position = reversed
                    , positionInst = Vector 0 0 0
                    , omega = Vector 0 0 0
                    , omegaInst = Vector 0 0 0
                    }
            in
                { newObject | velocity = Just zeroVelocity }


{-| Function should be true at low and false at high.
-}
binarySearch : (Float -> Bool) -> Float -> Float -> Float -> Float
binarySearch f tol low high =
    let
        mid =
            (low + high) / 2
    in
        if high - low < tol then
            if f high then
                high
            else
                mid
        else if f mid then
            binarySearch f tol mid high
        else
            binarySearch f tol low mid


undoCollisionNormal : Float -> Vector -> Object -> Object -> Object
undoCollisionNormal dt normal surface object =
    case object.velocity of
        Nothing ->
            object

        Just v ->
            let
                tol =
                    0.01

                bodyB =
                    toBody surface

                f x =
                    undoMotion x object
                        |> toBody
                        |> Collision.collide bodyB

                t =
                    binarySearch f (tol * dt) 0 dt

                z2 =
                    Debug.log "tau" (t / dt)

                stopZ =
                    Vector v.position.x v.position.y 0

                reflectZ =
                    reflect normal v.position

                newV =
                    { position = reflectZ
                    , positionInst = Vector 0 0 0
                    , omega = Vector 0 0 0
                    , omegaInst = Vector 0 0 0
                    }

                newObject =
                    undoMotion (t + tol) object
                        |> (\x -> { x | velocity = Just newV })
                        |> motion dt
            in
                newObject


resolveCollisions : Float -> Model -> ( Model, Maybe NamedInteraction )
resolveCollisions dt model =
    let
        collide a rest =
            List.foldl (runCollider dt) a rest

        newObjects =
            model.objects
                -- |> List.filter (\obj -> getCollide obj |> isJust)
                |>
                    List.indexedMap (\n obj -> collide obj (allBut n model.objects))
    in
        ( { model | objects = newObjects }, Just ResolveCollisions )


{-| Deal with collision between objects A and B as follows:

- only do a collide if A has a velocity, A and B have Collide effect, and velocities are not equal with zero angular component
- Collide * vs. GJK, GJK vs. *
  - implies convex
  - resolve collision by setting A's velocity equal to B's velocity
- Collide OBB/N vs. OBB
  - resolve collision by setting A's velocity equal to B's velocity
- Collide OBB/N vs. OBBN
  - resolve collision by contact normal method
- Collide * vs. HeightMap
  - displace A vertically by position in B

-}
runCollider : Float -> Object -> Object -> Object
runCollider dt b a =
    let
        collideOBB obj1 obj2 =
            Collision.collide (toBody obj1) (toBody obj2)

        doIt f g =
            if f a b then
                g a b
            else
                { a | material = Color (vec4 0 1 0 1) }
    in
        case ( getCollide a, getCollide b ) of
            ( Just OBB, Just OBB ) ->
                doIt collideOBB (resolveOBB dt)

            ( Just OBBN, Just OBB ) ->
                doIt collideOBB (resolveOBB dt)

            ( Just OBB, Just OBBN ) ->
                doIt collideOBB (resolveOBBN dt)

            ( Just OBBN, Just OBBN ) ->
                doIt collideOBB (resolveOBBN dt)

            ( Just _, Just HeightMap ) ->
                resolveHeightMap dt a b

            ( Just _, Just GJK ) ->
                a

            ( Just GJK, Just _ ) ->
                a

            _ ->
                a


resolveOBB : Float -> Object -> Object -> Object
resolveOBB dt a b =
    { a | material = Color (vec4 1 0 0 1) } |> undoCollision dt


resolveOBBN : Float -> Object -> Object -> Object
resolveOBBN dt a b =
    let
        nodesB =
            Collision.collisionMap (toBody b) (toBody a)

        leavesB =
            Collision.Tree.leaves (toBody b).bounds
                |> Dict.fromList

        hitLeavesB =
            nodesB
                |> Set.toList
                |> List.filterMap (\k -> Dict.get k leavesB)

        faceNormal leaf =
            V.cross (V.sub leaf.p leaf.r) (V.sub leaf.q leaf.r)
                |> V.normalize
                |> Maybe.withDefault V.zAxis

        hitNormalB =
            hitLeavesB
                |> List.map faceNormal
                |> List.foldl V.add (Vector 0 0 0)
                |> V.normalize
                |> Maybe.withDefault V.zAxis
    in
        { a | material = Color (vec4 1 0 0 1) } |> undoCollisionNormal dt hitNormalB b


resolveHeightMap : Float -> Object -> Object -> Object
resolveHeightMap dt a b =
    let
        queryPoint =
            a.frame.position
                |> Frame.transformInto b.frame
                |> eachV2 (flip (/)) b.scale

        -- toZHull t =
        --     (getCached t).zHull
        --
        -- inPerimeter =
        --     Maybe.map toZHull b.drawable
        --         |> Maybe.withDefault []
        --         |> (flip pointInHull) aPoint
        toZTree t =
            (getCached t).zTree

        interpolateHeight u ( p, q, r ) =
            let
                ( a, b, c, d ) =
                    ( p.x - r.x, q.x - r.x, p.y - r.y, q.y - r.y )

                det =
                    a * d - b * c

                l1 =
                    (d * (u.x - r.x) - b * (u.y - r.y)) / det

                l2 =
                    (a * (u.y - r.y) - c * (u.x - r.x)) / det

                l3 =
                    1 - l1 - l2
            in
                if det < 1.0e-5 then
                    Nothing
                else
                    Just { u | z = max (u.z) (l1 * p.z + l2 * q.z + l3 * r.z) }

        maybeInTriangle point triangle =
            if inTriangle point triangle then
                Just triangle
            else
                Nothing

        height : Maybe Float
        height =
            Maybe.map toZTree b.drawable
                |> Maybe.andThen (queryTree (test2D queryPoint))
                |> Maybe.andThen (maybeInTriangle queryPoint)
                |> Maybe.andThen (interpolateHeight queryPoint)
                |> Maybe.map (eachV2 (*) b.scale)
                |> Maybe.map (Frame.transformOutOf b.frame)
                |> Maybe.map .z
    in
        case height of
            Just z ->
                let
                    p =
                        a.frame.position
                in
                    { a | frame = Frame.setPosition { p | z = z } a.frame }

            Nothing ->
                a


pointInHull : List ( Float, Float ) -> ( Float, Float ) -> Bool
pointInHull perimeter point =
    let
        ( x2, y2 ) =
            point

        cross2 ( x1, y1 ) =
            (x1 * y2 - x2 * y1) > 0
    in
        List.all cross2 perimeter


getCollide : Object -> Maybe CollideType
getCollide obj =
    let
        f effect =
            case effect of
                Collide x ->
                    Just x

                _ ->
                    Nothing
    in
        obj.effects |> List.filterMap f |> List.head


{-| Applies gravity to object if it has .velocity.
Displacement handled in Motion Effect.
-}
gravity : Float -> Float -> Object -> Object
gravity g dt object =
    case object.velocity of
        Just vFrame ->
            let
                p =
                    V.add vFrame.position (V.scale (-dt * g) V.zAxis)
            in
                { object
                    | velocity = Just { vFrame | position = p }
                }

        Nothing ->
            object


{-| Effect handler. Clamps object height to surface height in Floating effect.
The .moved property is marked as True, telling the RequestOcean Interaction to
request a new height. If the object is not over an ocean, does nothing.
-}
floatBoat : FloatingInfo -> Object -> ( Object, Maybe NamedEffect )
floatBoat floatingInfo object =
    let
        ( x, y, z ) =
            V.toTuple object.frame.position

        position =
            max floatingInfo.height z
                |> Vector x y

        -- this feels inconsistent, sometimes can coast off waves, sometimes not
        velocity =
            case object.velocity of
                Nothing ->
                    Nothing

                Just vFrame ->
                    let
                        ( x_, y_, z_ ) =
                            V.toTuple vFrame.position
                    in
                        if floatingInfo.height >= z then
                            Just { vFrame | position = Vector x_ y_ 0 }
                        else
                            Just vFrame

        newObject =
            { object
                | frame =
                    { position = position
                    , orientation = object.frame.orientation
                    }
                , velocity = velocity
            }
    in
        case floatingInfo.coordinates of
            Nothing ->
                -- do nothing
                ( object, Just (Floating { floatingInfo | moved = True }) )

            Just coordinates ->
                ( newObject, Just (Floating { floatingInfo | moved = True }) )


{-| gets the first ocean
-}
getOcean : Model -> Maybe Object
getOcean model =
    model.objects
        |> List.filter (\x -> x.drawable == Just Ocean)
        |> List.head


{-| Interaction handler for updating Floating effects.
-}
requestOcean : Model -> ( Model, Maybe NamedInteraction, Cmd msg )
requestOcean model =
    let
        inRange ( x, y, z ) =
            if 0 < x && x < 1 && 0 < y && y < 1 then
                Just ( x, y )
            else
                Nothing

        ocean =
            getOcean model

        -- does object have a Floating in need of update?
        needsRequest object =
            object.effects
                |> List.any
                    (\e ->
                        case e of
                            Floating { moved, requested } ->
                                moved

                            -- && not requested
                            _ ->
                                False
                    )

        -- find the object in ocean XY coordinates, scaled from -1 to 1
        findInOcean oc object =
            object.frame.position
                |> Frame.transformInto oc.frame
                |> eachV2 (flip (/)) oc.scale
                |> V.toTuple
                |> inRange

        -- Floating is marked with request coordinates so it can be identified later
        markRequest oc object =
            let
                updateEffect coordinates effect =
                    case effect of
                        Floating info ->
                            Floating { info | coordinates = coordinates, requested = True }

                        _ ->
                            effect
            in
                { object | effects = object.effects |> List.map (updateEffect (findInOcean oc object)) }

        -- batch requests
        requests =
            case ocean of
                Just oc ->
                    model.objects
                        |> List.filter needsRequest
                        |> List.filterMap (findInOcean oc)
                        |> List.map (\x -> requestOceanPort ( x, toString x ))
                        |> Cmd.batch

                Nothing ->
                    Cmd.none

        -- second pass to update Floating effects, wasteful
        newObjects =
            case ocean of
                Just oc ->
                    model.objects
                        |> List.map
                            (\x ->
                                if needsRequest x then
                                    markRequest oc x
                                else
                                    x
                            )

                Nothing ->
                    model.objects
    in
        ( { model | objects = newObjects }, Just RequestOcean, requests )


{-| Called when port returns heights sampled from dispacement map. Updates
Floating effect with height after converting into ocean coordinates.
-}
updateFloating : Model -> String -> Float -> Object -> Object
updateFloating model coordinates height object =
    let
        -- gets the first ocean
        ocean =
            model.objects
                |> List.filter (\x -> x.drawable == Just Ocean)
                |> List.head

        update offset effect =
            case effect of
                Floating info ->
                    let
                        name =
                            case info.coordinates of
                                Just c ->
                                    toString c

                                Nothing ->
                                    ""
                    in
                        if info.requested && name == coordinates then
                            Floating { info | requested = False, moved = False, height = offset height }
                        else
                            Floating info

                _ ->
                    effect
    in
        case ocean of
            Just oc ->
                let
                    -- ocean vertical scale applied in shader, not to mesh
                    offset h =
                        h * oc.scale.z + oc.frame.position.z
                in
                    { object | effects = object.effects |> List.map (update offset) }

            Nothing ->
                object


{-| Provide a test for objects within angular and distance tolerance.
Not intended for orbital camera, maybe should add distance.
Would be nice to have bounding box select, etc.
-}
grab : Float -> Float -> Model -> (Object -> Bool)
grab maxAngle maxDistance model =
    let
        test object =
            let
                position =
                    Frame.transformInto model.camera object.frame.position

                angle =
                    angleBetween (position |> V.toVec3) V3.i
            in
                (angle < maxAngle) && (V.length position < maxDistance)
    in
        test


{-| Add Control Effect to objects within angular and distance tolerance.
Only add if the object supports it (has a velocity).
-}
select : Model -> ( Model, Maybe NamedInteraction )
select model =
    let
        ( maxAngle, maxDistance ) =
            ( 0.4, 10 )

        canGrab =
            grab maxAngle maxDistance model

        isMainControl x =
            case x of
                MainControl y ->
                    True

                _ ->
                    False

        control object =
            case object.velocity of
                Just x ->
                    if (canGrab object) && not (List.any isMainControl object.effects) then
                        { object | effects = object.effects ++ [ Control ] }
                    else
                        object

                Nothing ->
                    object

        results : List Object
        results =
            model.objects |> List.map control

        -- checks if any changed
        didSomething =
            results |> List.any (\x -> List.member Control x.effects)

        new =
            if didSomething then
                Just Deselect
            else
                Just Select
    in
        ( { model | objects = results }, new )


deselect : Model -> ( Model, Maybe NamedInteraction )
deselect model =
    let
        noControl effect =
            case effect of
                Control ->
                    Nothing

                _ ->
                    Just effect

        newObjects =
            model.objects |> List.map (\x -> { x | effects = List.filterMap noControl x.effects })
    in
        ( { model | objects = newObjects }, Just Select )


followFPS : Model -> ( Model, Maybe NamedInteraction )
followFPS model =
    let
        following =
            model.objects |> List.filter (\x -> List.member View x.effects)

        -- follow the first object with a View effect
        camera =
            following |> List.head |> Maybe.map .frame |> Maybe.withDefault Frame.identity
    in
        ( { model | camera = camera }, Just (Follow FPS) )


{-| Tracks object orientation, rotate with LT, RT.
-}
followOrbital : Float -> Float -> Model -> ( Model, Maybe NamedInteraction )
followOrbital theta dt model =
    let
        newTheta =
            0.0033 * dt * (model.gamepad.lt.value - model.gamepad.rt.value) + theta

        following =
            model.objects |> List.filter (\x -> List.member View x.effects)

        target =
            following
                |> List.head
                |> Maybe.map .frame
                |> Maybe.withDefault (Frame.identity)

        offset =
            Vector -3 0 1.5
                |> Q.rotate target.orientation
                |> Q.rotate (Q.zRotation newTheta)

        orientation =
            Q.rotationFor V.xAxis (V.scale -1 offset)

        camera =
            { position = offset
            , orientation = orientation
            }
                |> Frame.extrinsicNudge target.position
    in
        ( { model | camera = camera }, Just (Follow (Orbital newTheta)) )


{-| If only it could be this simple
model.interactions
    |> List.filterMap (interactionManager action)
    |> foldla model
-}
applyInteractions : Action -> Model -> ( Model, Cmd Action )
applyInteractions action model =
    let
        applyInteraction int ( m, intSoFar, actionsSoFar ) =
            let
                ( newM, newInt, newAct ) =
                    case interactionManager action int of
                        Just f ->
                            f m

                        Nothing ->
                            ( m, Just int, Cmd.none )
            in
                ( newM, appendMaybe newInt intSoFar, actionsSoFar ++ [ newAct ] )

        ( finalModel, finalInts, finalActions ) =
            List.foldl applyInteraction ( model, [], [] ) model.interactions
    in
        ( { finalModel | interactions = finalInts }, Cmd.batch finalActions )


control : Model -> Float -> Frame -> VFrame -> VFrame
control model delta frame vFrame =
    let
        -- change in velocity
        dV =
            move (M.directions model.keys model.gamepad) delta frame

        -- |> Debug.log "m"
        -- directly modify frame :(
        -- not sure how to express turning as a change in velocity.orientation
        newFrame =
            turn (M.gamepadLook model.gamepad) (delta * 50) frame

        -- reverse frame orientation before calculating difference
        dW2 =
            Frame.inverse frame
                |> Frame.mul newFrame
                |> .orientation
                |> Q.toVector

        -- when adding omega over time, something accumulates
        -- leads to clockwise roll. comes out when omega is far from
        -- the external z axis
        -- maybe this is physical? game notion of rotation is to set an
        -- instantaneous omega. if we add in previous omegas we will not get
        -- the instantaneous result unless the directions coincide.
        --
        -- should be able to add some of omegaInst to omega; transformOutOf
        -- omegaInst doesn't work
        newVFrame =
            { position =
                V.add vFrame.position dV
                    |> clampVelocity 10 50
                    |> V.scale decay
            , positionInst = Vector 0 0 0
            , omega = vFrame.omega
            , omegaInst = dW2
            }

        clampNormal x v =
            let
                m =
                    V.length v |> clamp 0 x
            in
                v
                    |> V.normalize
                    |> Maybe.withDefault (Vector 0 0 0)
                    |> V.scale m

        decay =
            0.5 ^ (delta / 0.06)
    in
        newVFrame


{-| Uses typical XY directions to return change in velocity frame. Ignore z.
-}
move : { x : Float, y : Float, z : Float } -> Float -> Frame -> Vector
move { x, y, z } delta frame =
    let
        gaze =
            { position = Vector 0 0 0, orientation = frame.orientation }
                |> (flip Frame.transformOutOf) V.xAxis

        forward =
            Vector (V.getX gaze) (V.getY gaze) 0
                |> V.normalize
                |> Maybe.withDefault (Vector 0 0 0)

        sidelong =
            { position = Vector 0 0 0, orientation = frame.orientation }
                |> (flip Frame.transformOutOf) V.yAxis

        sideways =
            Vector (V.getX sidelong) (V.getY sidelong) 0
                |> V.normalize
                |> Maybe.withDefault (Vector 0 0 0)
    in
        V.scale x forward
            |> V.add (V.scale y sideways)
            |> V.scale delta


turn : { dx : Float, dy : Float } -> Float -> Frame -> Frame
turn { dx, dy } delta frame =
    frame
        |> Frame.extrinsicRotate (Q.zRotation (delta * dx))
        |> Frame.intrinsicRotate (Q.yRotation (-delta * dy))


noAction : (Model -> ( Model, Maybe NamedInteraction )) -> Interaction
noAction f x =
    let
        ( a, b ) =
            f x
    in
        ( a, b, Cmd.none )


{-| Effects change the model and return a List Effect, allowing for persistence.
-}
applyEffects : Model -> Time -> Object -> Object
applyEffects model dt object =
    let
        applyEffect effect ( obj, soFar ) =
            let
                ( newObj, newEffect ) =
                    effectManager effect model dt obj

                newEffects =
                    appendMaybe newEffect soFar
            in
                ( newObj, newEffects )

        ( finalObject, finalEffects ) =
            List.foldl applyEffect ( object, [] ) object.effects
    in
        { finalObject | effects = finalEffects }



-- {-| Type mystery:
-- type X = A Int | B
-- a = A 3
-- b = A
-- c = B
-- same : X -> X -> Bool
-- same = ???
-- -}
-- filterInteraction : Action -> Interaction -> Maybe Interaction
-- filterInteraction action interaction =
--     case ( action, interaction.valid ) of
--         ( MinAction x, MinAction y ) ->
--             Just interaction
--
--         _ ->
--             Nothing
