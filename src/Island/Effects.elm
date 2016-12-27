module Island.Effects exposing (..)

import Island.Types exposing (..)
import Minimum as M exposing (angleBetween, foldla, toButton)
import Frame exposing (Frame)
import Vector as V exposing (Vector)
import Quaternion exposing (Quaternion)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Time exposing (Time)


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
-- {-| Determines if an Action will trigger an Interaction. Could implement a
-- filtering function that depends on the Model instead. -}
-- minActionInteractions =
--     EveryDict.fromList <|
--         [(Select, )]


{-| Add Control Effect to objects within angular and distance tolerance.
Only add if the object supports it (has a velocity).
-}
select : Model -> ( Model, Maybe NamedInteraction )
select model =
    let
        ( maxAngle, maxDistance ) =
            ( 0.2, 3 )

        control object =
            let
                x =
                    V3.sub (V.toVec3 object.frame.position) model.person.position

                valid =
                    ((angleBetween model.person.gaze x) < maxAngle) && (V3.length x < maxDistance)
            in
                case object.velocity of
                    Just x ->
                        ( { object | effects = object.effects ++ [ Control ] }, True )

                    Nothing ->
                        ( object, False )

        results =
            model.objects |> List.map control

        -- bad, fix
        didSomething =
            results |> List.tail |> Maybe.withDefault [] |> List.any Tuple.second

        new =
            if didSomething then
                Debug.log "didsomething" Just Deselect
            else
                Just Select
    in
        ( { model | objects = results |> List.map Tuple.first }, new )


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


follow : Model -> ( Model, Maybe NamedInteraction )
follow model =
    let
        following =
            model.objects |> List.filter (\x -> List.member View x.effects)

        camera =
            following |> List.head |> Maybe.map .frame |> Maybe.withDefault Frame.identity
    in
        ( { model | camera = camera }, Just Follow )


interactionManager : Action -> NamedInteraction -> Maybe Interaction
interactionManager action name =
    case ( name, action ) of
        ( Select, MinAction (M.ButtonChange ( bool, button )) ) ->
            case button |> toButton of
                Just (M.B) ->
                    if bool then
                        Debug.log "selected" (Just select)
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

        ( Follow, MinAction (M.Animate dt) ) ->
            Just follow

        _ ->
            Nothing


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


effectManager : NamedEffect -> Model -> Time -> Object -> ( Object, Maybe NamedEffect )
effectManager namedEffect model dt object =
    case namedEffect of
        Control ->
            ( { object | frame = control model dt object.frame }
            , Just MainControl
            )

        MainControl ->
            ( { object | frame = control model dt object.frame }
            , Just MainControl
            )

        View ->
            ( object, Just View )


{-| Hack to reuse update rule from M.
Two axis control doesn't fully determine quaternion, so x entry is zero.
-}
control : Model -> Time -> Frame -> Frame
control model dt frame =
    frame
        |> move (M.directions model.keys model.gamepad)
        |> turn (M.gamepadLook model.gamepad)


{-| Uses typical XY directions to update frame. Ignore
-}
move : { x : Float, y : Float, z : Float } -> Frame -> Frame
move { x, y, z } frame =
    let
        gaze =
            Frame.transformInto frame V.xAxis

        forward =
            Vector (V.getX gaze) (V.getY gaze) 0
                |> V.normalize
                |> Maybe.withDefault (Vector 0 0 0)
    in
        frame
            |> Frame.extrinsicNudge (V.scale x forward)
            |> Frame.intrinsicNudge (V.scale y V.yAxis)


turn : { dx : Float, dy : Float } -> Frame -> Frame
turn { dx, dy } frame =
    frame
        |> Frame.extrinsicRotate (Quaternion.zRotation dy)
        |> Frame.intrinsicRotate (Quaternion.yRotation dx)


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
