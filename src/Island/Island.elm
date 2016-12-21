module Island.Island exposing (..)

import Html exposing (text, div, br)
import Html.Attributes exposing (style, height, width)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import WebGL exposing (Renderable, Shader)
import Minimum
import Island.Types exposing (..)
import Meshes exposing (icosphere, subdivide)
import Island.Geometry exposing (..)
import Island.Shaders exposing (..)
import Dict
import Dict.Extra exposing (groupBy)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Action )
init =
    let
        ( model, msg ) =
            Minimum.init

        action =
            Cmd.map MinAction msg
    in
        { boat = initBoat |> addPQ
        , island = initIsland |> addPQ
        , sea = [] |> addPQ
        , person = model.person
        , keys = model.keys
        , window = model.window
        , dragModel = model.dragModel
        }
            ! [ action ]


type Action
    = MinAction Minimum.Action


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        MinAction act ->
            let
                ( newModel, minAction ) =
                    Minimum.update act model
            in
                ( newModel
                , Cmd.batch
                    [ Cmd.map MinAction minAction
                    ]
                )


subscriptions : Model -> Sub Action
subscriptions model =
    [ Sub.map MinAction (Minimum.subscriptions model)
    ]
        |> Sub.batch


view : Model -> Html.Html Action
view model =
    let
        boat =
            model.boat

        island =
            model.island

        sea =
            model.sea

        window =
            model.window

        render drawable =
            renderDrawable drawable model colorVertexShader colorFragmentShader

        renderEdged drawable =
            renderDrawable drawable model edgeVertexShader colorFragmentShader

        entities =
            [ render (boat.mesh |> indexedMeshToTriangle)
            , render (island.mesh |> meshToTriangle)
            , renderEdged seaMeshEdged
              -- , renderDrawable (initCube |> indexedMeshToTriangle) model
            , render (initLightCube |> indexedMeshToTriangle)
            ]

        message =
            [ "Enjoy your stay on the island.", "dt: " ++ toString window.dt ]

        messageText =
            List.map text message
                |> List.intersperse (br [] [])
    in
        div
            [ style
                [ ( "width", toString window.size.width ++ "px" )
                , ( "height", toString window.size.height ++ "px" )
                , ( "position", "relative" )
                ]
            ]
            [ WebGL.toHtml
                [ width window.size.width
                , height window.size.height
                , style [ ( "display", "block" ) ]
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



-- FUNCTIONS


meshToTriangle : Mesh -> WebGL.Drawable Vertex
meshToTriangle mesh =
    let
        g ( a, b, c ) =
            let
                normal =
                    V3.cross (V3.sub b a) (V3.sub c a)

                att =
                    { position = a, normal = normal, color = vec3 0 0.2 1 }
            in
                ( att, { att | position = b }, { att | position = c } )
    in
        mesh |> List.map g |> WebGL.Triangle


indexedMeshToTriangle : IndexedMesh -> WebGL.Drawable Vertex
indexedMeshToTriangle mesh =
    let
        toAttribute vertex =
            { position = vertex.point
            , normal = vertex.normal
            , color = vec3 0 0.2 1
            }
    in
        mesh |> List.map (map3 toAttribute) |> WebGL.Triangle


edgedMeshToTriangle : IndexedMesh -> WebGL.Drawable EdgedVertex
edgedMeshToTriangle mesh =
    let
        dict =
            mesh
                |> List.concatMap (map3L identity)
                |> groupBy .index

        -- represents an empty
        emptyVector =
            vec3 0.11 0.11 0.11

        getPoint i =
            Dict.get i dict |> Maybe.andThen List.head |> Maybe.map .point |> Maybe.withDefault emptyVector

        firstLast ( a, b, c ) =
            [ a, c ]

        flattenedNeighbors vertex =
            Dict.get vertex.index dict
                |> Maybe.withDefault []
                |> List.concatMap (\v -> v.neighbors |> List.concatMap firstLast)
                |> List.take 6
                |> List.map getPoint

        toAttribute vertex =
            let
                neighbors =
                    flattenedNeighbors vertex

                get i =
                    neighbors |> List.drop (i - 1) |> List.head |> Maybe.withDefault emptyVector
            in
                { position = vertex.point
                , normal = vertex.normal
                , color = vec3 0 0.2 1
                , n1 = get 1
                , n2 = get 2
                , n3 = get 3
                , n4 = get 4
                , n5 = get 5
                , n6 = get 6
                }
    in
        mesh |> List.map (map3 toAttribute) |> WebGL.Triangle


lightSource : Vec3
lightSource =
    vec3 2.0 2.0 2


renderDrawable : WebGL.Drawable v -> Minimum.Model a -> Shader v (Uniforms {}) varyings -> Shader {} (Uniforms {}) varyings -> Renderable
renderDrawable drawable { window, person } vertexShader fragmentShader =
    let
        perspectiveMatrix =
            Minimum.perspective ( window.size.width, window.size.height ) person

        uniforms =
            { perspective = perspectiveMatrix
            , light = lightSource
            , viewer = person.position
            }
    in
        WebGL.render vertexShader fragmentShader drawable uniforms



-- INIT


initBoat : IndexedMesh
initBoat =
    boat |> indexMesh |> useCornerNormals |> invertIndexedNormals


initSea : Mesh
initSea =
    let
        sea =
            makeGrid 30 30 |> List.concatMap quadToTri

        x =
            centroid sea |> V3.scale -1
    in
        offset sea x


initSeaSphere : IndexedMesh
initSeaSphere =
    let
        sea =
            icosphere 4

        x =
            centroid sea |> V3.scale -1

        mesh =
            offset sea x
                |> eachMeshPoint (V3.scale 10)

        z =
            Debug.log "init" (List.head mesh)

        indexedMesh =
            mesh
                |> indexMesh
                |> useCornerNormals

        zz =
            Debug.log "init_i" (List.head indexedMesh)
    in
        indexedMesh


initCube : IndexedMesh
initCube =
    let
        x =
            centroid cube |> V3.scale -1

        mesh =
            offset cube x
                |> eachMeshPoint (V3.scale 3.1)
                |> eachMeshPoint (V3.add (vec3 0 0 0))
    in
        mesh |> indexMesh |> invertIndexedNormals


initLightCube : IndexedMesh
initLightCube =
    let
        x =
            centroid cube |> V3.scale -1

        mesh =
            offset cube x
                |> eachMeshPoint (V3.scale 0.3)
                |> eachMeshPoint (V3.add lightSource)
    in
        mesh |> indexMesh |> invertIndexedNormals


{-| Avoid repeating costly meshToTriangle.
-}
seaMesh : WebGL.Drawable Vertex
seaMesh =
    initSeaSphere |> indexedMeshToTriangle


seaMeshEdged : WebGL.Drawable EdgedVertex
seaMeshEdged =
    initSeaSphere |> edgedMeshToTriangle


initIsland : Mesh
initIsland =
    centroid island
        |> V3.scale -1
        |> offset island
