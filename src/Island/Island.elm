module Island.Island exposing (..)

import Html exposing (text, div, br)
import Html.Attributes exposing (style, height, width)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Vector3 as V3
import WebGL exposing (Renderable, Shader, Texture, Error)
import Minimum
import Island.Types exposing (..)
import Meshes exposing (icosphere, subdivide)
import Island.Geometry exposing (..)
import Island.Shaders exposing (..)
import Island.Render exposing (..)
import Task


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

        textureAction ( texId, url ) =
            WebGL.loadTexture url
                |> Task.attempt
                    (\result ->
                        case result of
                            Err err ->
                                TextureError err

                            Ok val ->
                                TextureLoaded (texId val)
                    )

        textureActions =
            List.map textureAction
                [ ( Tex0, "https://raw.githubusercontent.com/elm-community/webgl/master/examples/texture/woodCrate.jpg" )
                , ( Tex1, "https://raw.githubusercontent.com/elm-community/webgl/master/examples/texture/thwomp_face.jpg" )
                ]
    in
        { boat = initBoat |> addPQ
        , island = initIsland |> addPQ
        , sea = [] |> addPQ
        , gridSea = [] |> addPQ
        , person = model.person
        , keys = model.keys
        , window = model.window
        , dragModel = model.dragModel
        , textures = { tex0 = Nothing, tex1 = Nothing }
        }
            ! ([ action ] ++ textureActions)


type Action
    = MinAction Minimum.Action
    | TextureError Error
    | TextureLoaded TextureID


type TextureID
    = Tex0 Texture
    | Tex1 Texture


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

        TextureError err ->
            model ! []

        TextureLoaded texture ->
            let
                oldTextures =
                    model.textures

                newTextures =
                    case texture of
                        Tex0 tex ->
                            { oldTextures | tex0 = Just tex }

                        Tex1 tex ->
                            { oldTextures | tex1 = Just tex }
            in
                { model | textures = newTextures } ! []


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

        unpack m =
            Maybe.map2 (,) m.tex0 m.tex1

        textureEntity =
            case unpack model.textures of
                Just ( tex0, tex1 ) ->
                    [ renderTextured (face0 |> meshPQToTriangle) model tex0 textureVertexShader textureFragmentShader
                    , renderTextured (face1 |> meshPQToTriangle) model tex1 textureVertexShader textureFragmentShader
                    ]
                        ++ [ renderDummyTextures ( tex0, tex1 ) ]

                Nothing ->
                    []

        displacementEntity =
            case unpack model.textures of
                Just ( tex0, tex1 ) ->
                    [ renderTextured gridSeaMeshEdged model tex0 oceanVertexShader textureFragmentShader ]

                Nothing ->
                    []

        entities =
            [ -- [ render (boat.mesh |> indexedMeshToTriangle)
              -- , render (island.mesh |> meshToTriangle)
              -- , renderTextured model seaMeshEdged edgeVertexShader colorFragmentShader
              -- , renderDrawable (initCube |> indexedMeshToTriangle) model
              render (initLightCube |> indexedMeshToTriangle)
            ]
                ++ displacementEntity
                ++ textureEntity

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
-- INIT


initBoat : IndexedMesh
initBoat =
    boat |> indexMesh |> useCornerNormals |> invertIndexedNormals


initSea : Mesh
initSea =
    let
        sea =
            makeGrid 50 50 |> List.concatMap quadToTri

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

        indexedMesh =
            mesh
                |> indexMesh
                |> useCornerNormals
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


gridSeaMeshEdged : WebGL.Drawable EdgedVertex
gridSeaMeshEdged =
    initSea |> eachMeshPoint (V3.scale 0.02) |> indexMesh |> edgedMeshToTriangle


initIsland : Mesh
initIsland =
    centroid island
        |> V3.scale -1
        |> offset island
