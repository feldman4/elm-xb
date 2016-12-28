port module Island.Effects exposing (..)

import Island.Types exposing (..)
import Minimum as M exposing (angleBetween, foldla, toButton)
import Frame exposing (Frame)
import Vector as V exposing (Vector)
import Quaternion as Q exposing (Quaternion)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Time exposing (Time)


effectManager : NamedEffect -> Model -> Time -> Object -> ( Object, Maybe NamedEffect )
effectManager namedEffect model dt object =
    case namedEffect of
        Control ->
            -- pick up and hold
            ( { object | frame = object.frame |> Frame.setPosition (Frame.transformOutOf model.camera (Vector 2 0 0)) }
            , Just Control
            )

        MainControl speed ->
            ( { object | frame = control model (dt * speed) object.frame }
            , Just (MainControl speed)
            )

        View ->
            ( object, Just View )

        Floating floatingInfo ->
            floatBoat floatingInfo object

        Gravity g ->
            ( gravity g dt object, Just (Gravity g) )


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

        ( Deselect, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.B) ->
                    if bool then
                        Just (deselect |> noAction)
                    else
                        Nothing

                _ ->
                    Nothing

        ( Follow FPS, MinAction (M.Animate dt) ) ->
            Just (followFPS |> noAction)

        ( Follow (Orbital theta), MinAction (M.Animate dt) ) ->
            Just (followOrbital theta dt |> noAction)

        ( RequestOcean, MinAction (M.Animate dt) ) ->
            Just requestOcean

        _ ->
            Nothing


{-| Applies gravity and displacement to object if it has .veloity.
-}
gravity : Float -> Float -> Object -> Object
gravity g dt object =
    case object.velocity of
        Just v ->
            let
                velocity =
                    V.zAxis |> V.scale (-dt * g) |> V.add v
            in
                { object
                    | frame =
                        { position = V.add object.frame.position velocity
                        , orientation = object.frame.orientation
                        }
                    , velocity = Just velocity
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
        newInfo =
            { floatingInfo | moved = True }

        ( x, y, z ) =
            V.toTuple object.frame.position

        position =
            max floatingInfo.height z
                |> Vector x y

        velocity =
            case object.velocity of
                Nothing ->
                    Nothing

                Just v ->
                    let
                        ( x, y, z ) =
                            V.toTuple v
                    in
                        if floatingInfo.height > z then
                            Just (Vector x y 0)
                        else
                            Just v

        newObject =
            { object
                | frame =
                    { position = position
                    , orientation = object.frame.orientation
                    }
                , velocity = velocity
            }

        outOfRange ( x, y ) =
            (abs x) > 1 || (abs y) > 1
    in
        if outOfRange floatingInfo.coordinates then
            -- do nothing
            ( object, Just (Floating floatingInfo) )
        else
            ( newObject, Just (Floating newInfo) )


{-| Interaction handler for updating Floating effects.
-}
requestOcean : Model -> ( Model, Maybe NamedInteraction, Cmd msg )
requestOcean model =
    let
        inRange ( x, y, z ) =
            if (abs x) <= 1 && (abs y) <= 1 then
                Just ( x, y )
            else
                Nothing

        -- gets the first ocean
        ocean =
            model.objects
                |> List.filter (\x -> x.drawable == Just Ocean)
                |> List.head

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
                |> dotApply (flip (/)) (oc.scale |> V.fromVec3)
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
                case findInOcean oc object of
                    Just coordinates ->
                        { object | effects = object.effects |> List.map (updateEffect coordinates) }

                    Nothing ->
                        object

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
                    model.objects |> List.map (markRequest oc)

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
                    if info.requested && (toString info.coordinates) == coordinates then
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
                        h * (V3.getZ oc.scale) + (V.getZ oc.frame.position)
                in
                    { object | effects = object.effects |> List.map (update offset) }

            Nothing ->
                object


{-| (0,1) coordinates in texture, request name
-}
port requestOceanPort : ( ( Float, Float ), String ) -> Cmd msg


{-| Add Control Effect to objects within angular and distance tolerance.
Only add if the object supports it (has a velocity).
-}
select : Model -> ( Model, Maybe NamedInteraction )
select model =
    let
        ( maxAngle, maxDistance ) =
            ( 0.4, 10 )

        control object =
            let
                position =
                    Frame.transformInto model.camera object.frame.position

                angle =
                    angleBetween (position |> V.toVec3) V3.i

                valid =
                    (angle < maxAngle) && (V.length position < maxDistance)

                isMainControl x =
                    case x of
                        MainControl y ->
                            True

                        _ ->
                            False
            in
                case object.velocity of
                    Just x ->
                        if valid && not (List.any isMainControl object.effects) then
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


control : Model -> Float -> Frame -> Frame
control model delta frame =
    frame
        |> move (M.directions model.keys model.gamepad) delta
        |> turn (M.gamepadLook model.gamepad) delta


{-| Uses typical XY directions to update frame. Ignore z.
-}
move : { x : Float, y : Float, z : Float } -> Float -> Frame -> Frame
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
        frame
            |> Frame.extrinsicNudge (V.scale x forward |> V.scale delta)
            |> Frame.extrinsicNudge (V.scale y sideways |> V.scale delta)


turn : { dx : Float, dy : Float } -> Float -> Frame -> Frame
turn { dx, dy } delta frame =
    frame
        |> Frame.extrinsicRotate (Q.zRotation (delta * dx))
        |> Frame.intrinsicRotate (Q.yRotation (-delta * dy))


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


appendMaybe : Maybe a -> List a -> List a
appendMaybe maybeX xs =
    case maybeX of
        Just x ->
            xs ++ [ x ]

        Nothing ->
            xs


dotApply : (Float -> Float -> Float) -> Vector -> Vector -> Vector
dotApply f a b =
    let
        ( a1, a2, a3 ) =
            V.toTuple a

        ( b1, b2, b3 ) =
            V.toTuple b
    in
        Vector (f a1 b1) (f a2 b2) (f a3 b3)


noAction : (Model -> ( Model, Maybe NamedInteraction )) -> Interaction
noAction f x =
    let
        ( a, b ) =
            f x
    in
        ( a, b, Cmd.none )



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
