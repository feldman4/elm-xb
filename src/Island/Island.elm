module Island.Island exposing (..)

import Html exposing (text, div, br)
import Html.Attributes exposing (style, height, width)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import Math.Matrix4 as M4 exposing (Mat4)
import WebGL exposing (render, Renderable, Shader)
import Minimum
import Island.Types exposing (..)
import Meshes exposing (icosphere, subdivide)
import Island.Geometry exposing (..)
import Island.Shaders exposing (..)


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
view { window, person, boat, island, sea } =
    let
        perspectiveMatrix =
            Minimum.perspective ( window.size.width, window.size.height ) person

        entities =
            [ renderMesh boat perspectiveMatrix
            , renderMesh island perspectiveMatrix
              -- , renderMesh sea perspectiveMatrix
            , renderDrawable seaMesh perspectiveMatrix
            ]

        message =
            [ "Enjoy your stay", "on the island. dt: " ++ toString window.dt ]

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


renderMesh : MeshPQ -> Mat4 -> Renderable
renderMesh { mesh } perspective =
    let
        uniforms =
            { perspective = perspective
            , light = vec3 3 3 3
            }

        drawable =
            meshToTriangle mesh
    in
        WebGL.render colorVertexShader colorFragmentShader drawable uniforms


renderDrawable : WebGL.Drawable Vertex -> Mat4 -> Renderable
renderDrawable drawable perspective =
    let
        uniforms =
            { perspective = perspective
            , light = vec3 3 3 3
            }
    in
        WebGL.render colorVertexShader colorFragmentShader drawable uniforms



-- INIT


initBoat : Mesh
initBoat =
    boat


initSea : Mesh
initSea =
    let
        sea =
            makeGrid 30 30 |> List.concatMap quadToTri

        x =
            centroid sea |> V3.scale -1
    in
        offset sea x


initSeaSphere : Mesh
initSeaSphere =
    let
        sea =
            icosphere 2

        x =
            centroid sea |> V3.scale -1
    in
        offset sea x |> invertNormals |> eachMeshPoint (V3.scale 10)


{-| Avoid repeating costly meshToTriangle. Not so good if
-}
seaMesh : WebGL.Drawable Vertex
seaMesh =
    initSeaSphere |> meshToTriangle


initIsland : Mesh
initIsland =
    centroid island
        |> V3.scale -1
        |> offset island
