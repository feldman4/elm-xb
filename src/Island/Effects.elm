module Island.Effects exposing (..)

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
            ( { object | frame = object.frame |> Frame.setPosition (Frame.transformOutOf model.camera (Vector 2 0 0)) }
            , Just Control
            )

        MainControl ->
            ( { object | frame = control model dt object.frame }
            , Just MainControl
            )

        View ->
            ( object, Just View )


interactionManager : Action -> NamedInteraction -> Maybe Interaction
interactionManager action name =
    case ( name, action ) of
        ( Select, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.B) ->
                    if bool then
                        Just select
                    else
                        Nothing

                _ ->
                    Nothing

        ( Deselect, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.B) ->
                    if bool then
                        Debug.log "deselected" (Just deselect)
                    else
                        Nothing

                _ ->
                    Nothing

        ( Follow FPS, MinAction (M.Animate dt) ) ->
            Just followFPS

        ( Follow (Orbital theta), MinAction (M.Animate dt) ) ->
            Just (followOrbital theta dt)

        _ ->
            Nothing


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

                a =
                    Debug.log "position" ( object.drawable, position )

                angle =
                    angleBetween (position |> V.toVec3) V3.i

                valid =
                    (angle < maxAngle) && (V.length position < maxDistance)
            in
                case object.velocity of
                    Just x ->
                        if valid && not (List.member MainControl object.effects) then
                            Debug.log "selected" { object | effects = object.effects ++ [ Control ] }
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
                Debug.log "didsomething" Just Deselect
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


{-| -}
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
                |> Maybe.map (\x -> x.frame.position)
                |> Maybe.withDefault (Vector 0 0 0)

        offset =
            Vector -3 0 1.5 |> Q.rotate (Q.zRotation newTheta)

        orientation =
            Q.rotationFor V.xAxis (V.scale -1 offset)

        camera =
            { position = offset
            , orientation = orientation
            }
                |> Frame.extrinsicNudge target
    in
        ( { model | camera = camera }, Just (Follow (Orbital newTheta)) )


{-| If only it could be this simple
model.interactions
    |> List.filterMap (interactionManager action)
    |> foldla model
-}
applyInteractions : Action -> Model -> Model
applyInteractions action model =
    let
        applyInteraction int ( m, soFar ) =
            let
                ( newM, newInt ) =
                    case interactionManager action int of
                        Just f ->
                            f m

                        Nothing ->
                            ( m, Just int )

                soFar2 =
                    appendMaybe newInt soFar
            in
                ( newM, soFar2 )

        ( finalModel, finalInts ) =
            List.foldl applyInteraction ( model, [] ) model.interactions
    in
        { finalModel | interactions = finalInts }


control : Model -> Time -> Frame -> Frame
control model dt frame =
    frame
        |> move (M.directions model.keys model.gamepad)
        |> turn (M.gamepadLook model.gamepad)


{-| Uses typical XY directions to update frame. Ignore z.
-}
move : { x : Float, y : Float, z : Float } -> Frame -> Frame
move { x, y, z } frame =
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
            |> Frame.extrinsicNudge (V.scale x forward |> V.scale 0.1)
            |> Frame.extrinsicNudge (V.scale y sideways |> V.scale 0.1)


turn : { dx : Float, dy : Float } -> Frame -> Frame
turn { dx, dy } frame =
    frame
        |> Frame.extrinsicRotate (Q.zRotation (1.5 * dx))
        |> Frame.intrinsicRotate (Q.yRotation (-1.5 * dy))


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
