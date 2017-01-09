module Valley exposing (..)

import Frame exposing (Frame)
import Html exposing (br, div, p, text)
import Html.Attributes exposing (height, style, width)
import Island.Effects
import Island.Geometry exposing (scale3D)
import Island.Island
import Island.Render exposing (..)
import Island.Things exposing (getCached)
import Island.Types exposing (Model, Object, NamedInteraction)
import Minimum exposing (onClick)
import Math.Matrix4 as M4 exposing (Mat4, makeOrtho)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector4 as V4 exposing (Vec4, vec4)
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
        ( model, act ) =
            Island.Island.init

        objects =
            []

        interactions =
            []
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
                        ( w, h ) =
                            ( click.offsetX // m2.window.width |> toFloat
                            , click.offsetY // m2.window.height |> toFloat
                            )

                        colorHit obj =
                            if raycast ( w, h ) m2.camera obj then
                                { obj | material = Island.Types.Color (vec4 1 0 0 1) }
                            else
                                { obj | material = Island.Types.Color (vec4 0 1 0 1) }

                        hitObjects =
                            m2.objects
                                |> List.map colorHit
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


{-| Represent ray from a to b as a Frame, with the x axis pointing in ray
direction.
-}
rayToFrame : Vector -> Vector -> Frame
rayToFrame a b =
    { position = a, orientation = Q.rotationFor V.xAxis (V.sub b a) }


{-| Do a collision test with a triangle by transforming the triangle
into the ray frame and checking if it contains the XY origin.
-}
rayThroughTriangle : Frame -> ( Vector, Vector, Vector ) -> Bool
rayThroughTriangle frame triangle =
    map3T (Frame.transformInto frame) triangle
        |> inTriangle (Vector 0 0 0)


{-| Needs to be tested.
-}
raycast : ( Float, Float ) -> Frame -> Object -> Bool
raycast ( screenX, screenY ) camera object =
    let
        screenNear =
            vec3 screenX screenY -1 |> inv |> V.fromVec3

        screenFar =
            vec3 screenX screenY 1 |> inv |> V.fromVec3

        inv =
            orthoPerspective camera
                |> M4.inverseOrthonormal
                |> M4.transform

        rayFrame =
            rayToFrame screenNear screenFar

        -- get object mesh
        cast thing =
            (getCached thing).mesh
                |> scale3D object.scale
                |> List.map (map3T V.fromVec3)
                |> List.map (map3T (Frame.transformOutOf object.frame))
                |> List.any (rayThroughTriangle rayFrame)
    in
        case object.drawable of
            Nothing ->
                False

            Just thing ->
                cast thing


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

        transform x =
            Frame.toMat4 x
                |> (M4.mul ortho)
                |> (\m -> M4.transform m (Vector 0 0 0 |> V.toVec3))

        xyEqual u v =
            V3.getX u == V3.getX v && V3.getY u == V3.getY v

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
                |> orthoPerspective

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
            orthoPerspective model.camera

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
