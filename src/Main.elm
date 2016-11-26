-- Try adding the ability to crouch or to land on top of the crate.


module Main exposing (..)

import Keyboard
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import Task exposing (Task)
import Time exposing (..)
import WebGL exposing (..)
import Html exposing (Html, text, div)
import Html
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Window
import PTree exposing (..)
import Drag


-- MODEL


type alias Person =
    { position : Vec3
    , velocity : Vec3
    , gaze : Vec3
    , crouch : Bool
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    , z : Bool
    }


type alias Model =
    { texture : Maybe Texture
    , keys : Keys
    , size : Window.Size
    , person : Person
    , dragModel : Drag.Model
    }


type Action
    = TextureError Error
    | TextureLoaded Texture
    | KeyChange (Keys -> Keys)
    | Animate Time
    | Resize Window.Size
    | DragMsg Drag.Msg
    | Drag ( Int, Int )


eyeLevel : Float
eyeLevel =
    2


defaultPerson : Person
defaultPerson =
    { position = vec3 0 eyeLevel -10
    , velocity = vec3 0 0 0
    , gaze = k
    , crouch = False
    }


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TextureError err ->
            ( model, Cmd.none )

        TextureLoaded texture ->
            ( { model | texture = Just texture }, Cmd.none )

        KeyChange keyfunc ->
            ( { model | keys = keyfunc model.keys }, Cmd.none )

        Resize size ->
            ( { model | size = size }, Cmd.none )

        DragMsg msg ->
            let
                ( newDragModel, dragCmd ) =
                    Drag.update Drag msg model.dragModel
            in
                ( { model | dragModel = newDragModel }, dragCmd )

        Drag ( x, y ) ->
            let
                dx =
                    toFloat x / -200

                dy =
                    toFloat y / -200

                sideways =
                    V3.cross model.person.gaze j |> V3.normalize

                r =
                    makeRotate dx j |> M4.rotate dy sideways

                newGaze =
                    transform r model.person.gaze

                person =
                    model.person

                newPerson =
                    { person | gaze = newGaze }
            in
                { model | person = newPerson } ! []

        Animate dt ->
            ( { model
                | person =
                    model.person
                        |> walk (directions model.keys)
                        |> jump model.keys.space
                        |> crouch model.keys.z
                        |> gravity (dt / 500)
                        |> physics (dt / 500)
              }
            , Cmd.none
            )


init : ( Model, Cmd Action )
init =
    ( { texture = Nothing
      , person = defaultPerson
      , keys = Keys False False False False False False
      , size = Window.Size 0 0
      , dragModel = Drag.initialModel
      }
    , Cmd.batch
        [ loadTexture "texture/woodCrate.jpg"
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError err

                        Ok val ->
                            TextureLoaded val
                )
        , Task.perform Resize Window.size
        ]
    )


subscriptions : Model -> Sub Action
subscriptions model =
    [ AnimationFrame.diffs Animate
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Window.resizes Resize
    , Drag.subscriptions DragMsg model.dragModel
    ]
        |> Sub.batch


keyChange : Bool -> Keyboard.KeyCode -> Action
keyChange on keyCode =
    (case keyCode of
        32 ->
            \k -> { k | space = on }

        37 ->
            \k -> { k | left = on }

        39 ->
            \k -> { k | right = on }

        38 ->
            \k -> { k | up = on }

        40 ->
            \k -> { k | down = on }

        90 ->
            \k -> { k | z = on }

        _ ->
            Basics.identity
    )
        |> KeyChange


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


directions : Keys -> { x : Int, y : Int }
directions { left, right, up, down } =
    let
        direction a b =
            case ( a, b ) of
                ( True, False ) ->
                    -2

                ( False, True ) ->
                    2

                _ ->
                    0
    in
        { x = direction left right
        , y = direction down up
        }


walk : { x : Int, y : Int } -> Person -> Person
walk directions person =
    if getY person.position > eyeLevel then
        person
    else
        let
            vx =
                toFloat -directions.x

            vz =
                toFloat directions.y
        in
            { person
                | velocity = vec3 vx (getY person.velocity) vz
            }


jump : Bool -> Person -> Person
jump isJumping person =
    if not isJumping || getY person.position > eyeLevel then
        person
    else
        let
            ( vx, _, vz ) =
                toTuple person.velocity
        in
            { person
                | velocity = vec3 vx 2 vz
            }


physics : Float -> Person -> Person
physics dt person =
    let
        position =
            add person.position (V3.scale dt person.velocity)

        ( x, y, z ) =
            toTuple position
    in
        { person
            | position =
                if y < eyeLevel then
                    vec3 x eyeLevel z
                else
                    position
        }


gravity : Float -> Person -> Person
gravity dt person =
    if getY person.position <= eyeLevel then
        person
    else
        let
            v =
                toRecord person.velocity
        in
            { person
                | velocity = vec3 v.x (v.y - 2 * dt) v.z
            }


crouch : Bool -> Person -> Person
crouch on person =
    if on then
        { person | crouch = True }
    else
        { person | crouch = False }


world : Maybe Texture -> Mat4 -> List Renderable
world maybeTexture perspective =
    case maybeTexture of
        Nothing ->
            []

        Just tex ->
            let
                uniforms =
                    { crate = tex, perspective = perspective }
            in
                [ render vertexShader fragmentShader crate uniforms ]



-- VIEW


perspective : ( Int, Int ) -> Person -> Mat4
perspective ( w, h ) person =
    mul (makePerspective 90 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (add person.position person.gaze) j)


view : Model -> Html Action
view { size, person, texture } =
    let
        newPosition =
            case person.crouch of
                True ->
                    V3.setY (V3.getY person.position - 1) person.position

                False ->
                    person.position

        newPerson =
            { person | position = newPosition }

        perspectiveMatrix =
            perspective ( size.width, size.height ) newPerson

        -- entities =
        --     (world texture perspectiveMatrix)
        entities =
            worldTree exampleTreeBase exampleTreeOrigin perspectiveMatrix
    in
        div
            [ style
                [ ( "width", toString size.width ++ "px" )
                , ( "height", toString size.height ++ "px" )
                , ( "position", "relative" )
                ]
            ]
            [ WebGL.toHtml
                [ width size.width
                , height size.height
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
                [ text message ]
            ]


message : String
message =
    "Walk around with a first person perspective.\n"
        ++ "Arrows keys to move, space bar to jump.\n"



-- Define the mesh for a crate


type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }


type alias Face =
    List ( Vertex, Vertex, Vertex )


crate : Drawable Vertex
crate =
    Triangle
        (List.concatMap (\x -> rotateFace x crateFace)
            [ ( 0, 0, 0 )
            , ( 90, 0, 0 )
            , ( 180, 0, 0 )
            , ( 270, 0, 0 )
            , ( 0, 90, 0 )
            , ( 0, -90, 0 )
            ]
        )


rotateFace : ( Float, Float, Float ) -> Face -> Face
rotateFace ( angleYZ, angleXZ, angleXY ) face =
    let
        iRot =
            makeRotate (degrees angleYZ) i

        jRot =
            makeRotate (degrees angleXZ) j

        kRot =
            makeRotate (degrees angleXY) k

        t =
            mul (mul iRot jRot) kRot

        each f ( a, b, c ) =
            ( f a, f b, f c )
    in
        List.map (each (\v -> { v | position = transform t v.position })) face


crateFace : Face
crateFace =
    let
        topLeft =
            Vertex (vec3 -1 1 1) (vec3 0 1 0)

        topRight =
            Vertex (vec3 1 1 1) (vec3 1 1 0)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec3 0 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec3 1 0 0)
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]



-- Shaders
-- vertexShader magically knows attributes (Vertex) must have one field for each
-- attribute in the GLSL shader code
-- variables


vertexShader : Shader Vertex { u | perspective : Mat4 } { vcoord : Vec2, fPosition : Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;
varying vec3 fPosition;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
  fPosition = gl_Position.xyz;
}

|]


fragmentShader : Shader {} { u | crate : Texture } { vcoord : Vec2, fPosition : Vec3 }
fragmentShader =
    [glsl|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;
varying vec3 fPosition;

void main () {
  vec4 tex_Color = texture2D(crate, vcoord);
  vec4 fog_Color = vec4(0.9, 0.9, 1.0, 1.);
  float dist = min(length(fPosition) * 0.1, 1.0);
  gl_FragColor = mix(tex_Color, fog_Color, dist);
}

|]
