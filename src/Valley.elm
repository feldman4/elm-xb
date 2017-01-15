module Valley exposing (..)

import Frame exposing (Frame)
import Html exposing (br, div, p, text)
import Html.Attributes exposing (height, style, width)
import Island.Effects
import Island.Geometry exposing (frameToVFrame, scale3D)
import Island.Island
import Island.Render exposing (..)
import Island.Things exposing (getCached, initBoat, initLightCube)
import Island.Types exposing (Model, NamedInteraction, Object)
import Math.Matrix4 as M4 exposing (Mat4, makeOrtho)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)
import Minimum exposing (onClick)
import Quaternion as Q
import Utilities exposing (..)
import VTree exposing (inTriangle)
import Vector as V exposing (Vector)
import WebGL


type alias Face =
    Frame


type alias Ortho =
    Mat4


type alias Path =
    List Face


type Action
    = IslandAction Island.Types.Action



-- main : Html.Html msg
-- main =
--     let
--         -- testFindPath |> toString |> text
--         points =
--             testProjection
--                 |> List.map toString
--                 |> List.map (\x -> p [] [ text x ])
--                 |> div []
--     in
--         points


init : ( Model, Cmd Action )
init =
    let
        cube =
            initLightCube

        avatar =
            { cube
                | effects =
                    [ Island.Types.View
                    , Island.Types.MainControl 2
                    , Island.Types.Motion
                    ]
                , velocity = Just (Frame.identity |> frameToVFrame)
                , drawable = Nothing
                , frame =
                    Frame.identity
                        |> Frame.setPosition (Vector -2 0 2)
                        |> Frame.intrinsicRotate (Q.rotationFor V.xAxis (Vector 1 0 -1))
            }

        cubes =
            List.range 0 5
                |> List.map toFloat
                |> List.map
                    (\x ->
                        { cube
                            | frame = Frame.setPosition (Vector x x x) cube.frame
                            , scale = Vector 0.5 0.5 0.5
                        }
                    )

        tree =
            { cube | drawable = Just Island.Types.TreeLowPoly, frame = Frame.identity }

        ( model, act ) =
            Island.Island.init

        objects =
            [ avatar, tree ] ++ cubes

        interactions =
            [ Island.Types.Follow Island.Types.FPS ]
    in
        { model | objects = objects, interactions = interactions }
            ! [ Cmd.map IslandAction act ]


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = (\a m -> updater a m)
        }


updater : Action -> Model -> ( Model, Cmd Action )
updater a m =
    let
        ( m2, a2 ) =
            update a m

        ( m4, a4 ) =
            case a of
                IslandAction (Island.Types.MinAction (Minimum.OnClick click)) ->
                    -- just take care of raycasting, no Interaction for now
                    let
                        w =
                            ((click.offsetX |> toFloat) / (m2.window.width |> toFloat) * 2 - 1)
                                |> (*) 10

                        h =
                            (1 - 2 * (click.offsetY |> toFloat) / (m2.window.height |> toFloat))
                                |> (*) 10

                        greenObjects =
                            m2.objects |> List.map (\x -> { x | material = Island.Types.Color (vec4 0 1 0 1) })

                        colorHit obj objects =
                            if raycast ( w, h ) m2.camera obj then
                                let
                                    projMat =
                                        orthoPerspective 10 10 m2.camera

                                    -- actually want links between faces, not cubes
                                    testLink a =
                                        cubeToFaces a.frame
                                            |> List.any (\x -> cubeToFaces obj.frame |> List.any (isLinked projMat x))

                                    colorIf a =
                                        if testLink a || obj == a then
                                            { a | material = Island.Types.Color (vec4 1 0 0 1) }
                                        else
                                            a
                                in
                                    objects
                                        |> List.map colorIf
                            else
                                objects

                        hitObjects =
                            greenObjects |> List.foldl colorHit greenObjects
                    in
                        ( { m2 | objects = hitObjects }, Cmd.none )

                IslandAction act ->
                    let
                        ( m3, a3 ) =
                            Island.Effects.applyInteractions act m2
                    in
                        ( m3, Cmd.map IslandAction a3 )
    in
        ( m4, Cmd.batch [ a2, a4 ] )


{-| Works so long as there are no custom Effects. Otherwise you need a custom
applyEffects to pattern match and delegate appropriately. Will cause problems
because effect handlers expect an Object with {a | effects : List Effect}.
-}
update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        IslandAction act ->
            let
                ( newModel, newAct ) =
                    Island.Island.update act model
            in
                newModel ! [ Cmd.map IslandAction newAct ]



--
-- OnClick { offsetX, offsetY } ->
--     model ! []


subscriptions : Model -> Sub Action
subscriptions model =
    [ Sub.map IslandAction (Island.Island.simpleSubscriptions model)
    ]
        |> Sub.batch


{-| Represent ray from a to b as a Frame, with the z axis pointing in ray
direction.
-}
rayToFrame : Vector -> Vector -> Frame
rayToFrame a b =
    { position = a, orientation = Q.rotationFor V.zAxis (V.sub b a) }


{-| Do a collision test with a triangle by transforming the triangle
into the ray frame and checking if it contains the XY origin.
-}
rayThroughTriangle : Frame -> ( Vector, Vector, Vector ) -> Bool
rayThroughTriangle frame triangle =
    map3T (Frame.transformInto frame) triangle
        |> map3T (\v -> Vector v.x v.y v.z)
        |> inTriangle (Vector 0 0 0)


{-| Seems to work.
-}
raycast : ( Float, Float ) -> Frame -> Object -> Bool
raycast ( screenX, screenY ) camera object =
    case object.drawable of
        Nothing ->
            False

        Just thing ->
            let
                screenNear =
                    vec3 screenX screenY -1 |> V3.scale 10 |> M4.transform inv |> V.fromVec3

                screenFar =
                    vec3 screenX screenY 1 |> V3.scale 10 |> M4.transform inv |> V.fromVec3

                -- sort-of right inverse. M * M^-1 has nonzero entries in bottom row.
                -- orthographic projection doesn't seem very orthonormal, but this still works
                inv =
                    orthoPerspective 10 10 camera
                        |> M4.inverseOrthonormal

                z =
                    orthoPerspective 10 10 camera
                        |> (flip M4.mul) inv

                -- inverse doesn't include translation
                rayFrame =
                    rayToFrame screenNear screenFar
                        |> Frame.extrinsicNudge camera.position

                -- get object mesh
                cast thing =
                    (getCached thing).mesh
                        |> scale3D object.scale
                        |> List.map (map3T V.fromVec3)
                        |> List.map (map3T (Frame.transformOutOf object.frame))
                        |> List.any (rayThroughTriangle rayFrame)
            in
                cast thing


{-| Get 6 faces of cube, represented as frames. In the Face frame, the actual face
is a 1x1 rectangle centered on the origin in the XY plane.
-}
cubeToFaces : Frame -> List Face
cubeToFaces frame =
    let
        orientations =
            [ V.xAxis, V.yAxis, V.zAxis, (V.scale -1 V.yAxis), (V.scale -1 V.zAxis) ]
                |> List.map (Q.rotationFor V.xAxis)
                |> (::) (Q.yRotation pi)

        initialFace =
            Frame.identity
                |> Frame.extrinsicNudge (Vector 0 0 0.5)

        faces =
            orientations
                |> List.map (\q -> Frame.extrinsicRotate q initialFace)
                |> List.map (Frame.compose frame)
    in
        faces


{-| Two (simple) Faces are linked if they border each other in an orthographic
projection and point in the same direction. If the projection is not
parallel to rays from the origin bisecting the cube edges then only in-plane
neighbors will be linked.
-}
isLinked : Ortho -> Face -> Face -> Bool
isLinked ortho a b =
    let
        offsets =
            [ Vector 1 0 0, Vector -1 0 0, Vector 0 1 0, Vector 0 -1 0 ]

        -- positions of neighboring faces in transform
        neighbors x =
            offsets
                |> List.map (\v -> Frame.intrinsicNudge v x)
                |> List.map transform

        -- screen space Face center (origin)
        transform x =
            Frame.toMat4 x
                |> (M4.mul ortho)
                |> (\m -> M4.transform m (Vector 0 0 0 |> V.toVec3))

        threshold =
            0.03

        xyEqual u v =
            let
                xDiff =
                    (abs (V3.getX u - V3.getX v))

                yDiff =
                    (abs (V3.getY u - V3.getY v))
            in
                xDiff <= threshold && yDiff <= threshold

        touching =
            a |> neighbors |> List.any (xyEqual (transform b))
    in
        touching


findPath : Ortho -> List Face -> Face -> Face -> Maybe Path
findPath ortho faces a b =
    dijkstra (isLinked ortho) faces a b


testProjection : List ( Vec3, Bool )
testProjection =
    let
        camera =
            Frame.identity
                |> Frame.setPosition (Vector -3 3 -3)
                |> Frame.intrinsicRotate (Q.rotationFor V.xAxis (Vector -1 1 -1))
                |> (\f -> orthoPerspective 10 10 f)

        positions =
            [ Vector 0 0 0, Vector 0 1 0, Vector 1 1 0, Vector -2 3 -2 ]
                |> List.map (\v -> Frame.setPosition v Frame.identity)

        faces =
            positions
                |> List.map Frame.toMat4
                |> List.map (M4.mul camera)
                |> List.map (\m -> M4.transform m (Vector 0 0 0 |> V.toVec3))

        touchingOrigin x =
            isLinked camera Frame.identity x
    in
        positions
            |> List.map touchingOrigin
            |> List.map2 (,) faces


testFindPath : Maybe (List Int)
testFindPath =
    let
        test a b =
            abs (a - b) < 3

        points =
            List.range 0 10
    in
        dijkstra test points 1 7


view : Model -> Html.Html Action
view model =
    let
        window =
            model.window

        perspectiveMatrix =
            orthoPerspective 10 10 model.camera

        entities =
            (model.objects
                |> List.filterMap renderMaybe
                |> List.map (renderObject model perspectiveMatrix)
            )
                ++ (textureEntity model)

        message =
            [ "Enjoy your stay on the island. " ++ (toString model.clock.dt) ]

        messageText =
            List.map text message
                |> List.intersperse (br [] [])
    in
        div
            [ style
                [ ( "width", toString window.width ++ "px" )
                , ( "height", toString window.height ++ "px" )
                , ( "position", "relative" )
                ]
            ]
            [ WebGL.toHtml
                [ width window.width
                , height window.height
                , style [ ( "display", "block" ) ]
                , onClick (\x -> x |> Minimum.OnClick |> Island.Types.MinAction |> IslandAction)
                ]
                entities
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "font-family", "monospace" )
                    , ( "text-align", "center" )
                    , ( "left", "20px" )
                    , ( "right", "20px" )
                    , ( "top", "20px" )
                    ]
                ]
                messageText
            ]
