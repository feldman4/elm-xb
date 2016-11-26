-- adapted from elm-community/webgl crate example


module Main exposing (..)

import Keyboard
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
import PTree
import Drag


-- MODEL


type alias Model =
    { keys : Keys
    , size : Window.Size
    , person : Person
    , dragModel : Drag.Model
    , forest : List (PTree.TreeBase PTree.SpringyQuad)
    }


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


type Action
    = KeyChange (Keys -> Keys)
    | Animate Time
    | Resize Window.Size
    | DragMsg Drag.Msg
    | Drag ( Int, Int )


eyeLevel : Float
eyeLevel =
    2


gazeSpeed : Float
gazeSpeed =
    1 / 200


defaultPerson : Person
defaultPerson =
    { position = vec3 0 eyeLevel -10
    , velocity = vec3 0 0 0
    , gaze = k
    , crouch = False
    }



-- INIT AND UPDATE


init : ( Model, Cmd Action )
init =
    ( { person = defaultPerson
      , keys = Keys False False False False False False
      , size = Window.Size 0 0
      , dragModel = Drag.initialModel
      , forest = PTree.exampleSpringyForest
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
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
                direction =
                    { dx = toFloat x * -1 * gazeSpeed
                    , dy = toFloat y * -1 * gazeSpeed
                    }

                person =
                    model.person

                newPerson =
                    { person | gaze = turn direction person.gaze }
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
                , forest =
                    PTree.animateForest dt model.forest
              }
            , Cmd.none
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



{- uses y is up convention (V3.j) -}


turn : { dx : Float, dy : Float } -> Vec3 -> Vec3
turn { dx, dy } gaze =
    let
        sideways =
            V3.cross gaze j |> V3.normalize

        r =
            makeRotate dx V3.j |> M4.rotate dy sideways
    in
        transform r gaze |> V3.normalize


walk : { x : Int, y : Int } -> Person -> Person
walk directions person =
    if getY person.position > eyeLevel then
        person
    else
        let
            -- ridiculous axis alignment
            dKey =
                vec3 (toFloat directions.y) 0 (toFloat directions.x)

            flatGaze =
                vec3 (V3.getX person.gaze) 0 (V3.getZ person.gaze)

            theta =
                if (V3.dot flatGaze V3.k) > 0 then
                    PTree.angleBetween flatGaze V3.i
                else
                    -(PTree.angleBetween flatGaze V3.i)

            dWorld =
                M4.transform (M4.makeRotate -theta V3.j) dKey
        in
            { person
                | velocity = vec3 (getX dWorld) (getY person.velocity) (getZ dWorld)
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



-- VIEW


perspective : ( Int, Int ) -> Person -> Mat4
perspective ( w, h ) person =
    mul (makePerspective 90 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (add person.position person.gaze) j)


view : Model -> Html Action
view { size, person, forest } =
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

        noSpringForest =
            List.map PTree.despring forest

        entities =
            PTree.worldForest noSpringForest perspectiveMatrix
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
        ++ "Arrows keys to move, space bar to jump, Z to crouch.\n"
        ++ "Click and drag mouse to look."



-- mesh


type alias Vertex =
    { position : Vec3
    , coord : Vec3
    }


type alias Face =
    List ( Vertex, Vertex, Vertex )


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
