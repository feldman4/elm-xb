port module Kwan exposing (..)

import Minimum
import Html exposing (text, div, br)
import Html.Attributes exposing (style, width, height)
import Math.Vector3 as V3 exposing (Vec3)
import Math.Matrix4 as M4 exposing (Mat4)
import WebGL exposing (render, Renderable, Shader)
import Html


type alias Point =
    { position : Vec3 }


type alias Person =
    { position : Vec3
    , velocity : Vec3
    , gaze : Vec3
    }


type alias Modeled =
    { points : List Vec3, word : String }


type alias Model =
    Minimum.Model Modeled


examplePoints : List Vec3
examplePoints =
    [ V3.vec3 1.0 2.0 3.0, V3.vec3 5.0 5.0 5.0, V3.vec3 -5.0 -5.0 -5.0 ]


main : Program Never Model Action
main =
    Html.program
        { init = initKwan
        , view = viewKwan
        , subscriptions = subscriptions
        , update = update
        }


initKwan : ( Model, Cmd Action )
initKwan =
    let
        ( model, msg ) =
            Minimum.init

        newModel =
            { points = []
            , word = "Hello"
            , person = model.person
            , keys = model.keys
            , window = model.window
            , dragModel = model.dragModel
            }

        actions =
            Cmd.batch
                [ Cmd.map MinAction msg
                , typeset newModel.word
                ]
    in
        ( newModel, actions )



-- UPDATE


type Action
    = MinAction Minimum.Action
    | ConvertPoints (List ( Float, Float ))


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

        ConvertPoints points ->
            { model | points = pointsToVec points } ! []



-- RENDER


pointsToVec : List ( Float, Float ) -> List Vec3
pointsToVec points =
    points |> List.map (\( a, b ) -> V3.vec3 3 (-a + 40) (-b + 40))


renderPoints : List Vec3 -> Mat4 -> List Renderable
renderPoints points perspective =
    let
        uniforms =
            { perspective = perspective }

        drawablePoints =
            points
                |> List.map (\p -> { position = p })
                |> WebGL.Points
    in
        [ WebGL.render pointVertexShader pointFragmentShader drawablePoints uniforms ]


pointVertexShader : Shader Point { u | perspective : Mat4 } {}
pointVertexShader =
    [glsl|

attribute vec3 position;
uniform mat4 perspective;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  gl_PointSize = 10.0;
}

|]


pointFragmentShader : Shader {} { u | perspective : Mat4 } {}
pointFragmentShader =
    [glsl|

precision mediump float;

void main () {
  gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
}

|]



-- PORTS


port typeset : String -> Cmd msg


port typesetResult : (List ( Float, Float ) -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Action
subscriptions model =
    [ Sub.map MinAction (Minimum.subscriptions model)
    , typesetResult ConvertPoints
    ]
        |> Sub.batch



-- VIEW


viewKwan : Model -> Html.Html Action
viewKwan { window, person, points } =
    let
        perspectiveMatrix =
            Minimum.perspective ( window.size.width, window.size.height ) person

        --
        -- entities =
        --     PTree.worldForest noSpringForest perspectiveMatrix
        entities =
            renderPoints points perspectiveMatrix

        message =
            [ "dt: " ++ toString window.dt ]

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
