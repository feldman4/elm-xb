module Minimum exposing (..)

-- adapted from elm-community/webgl crate example

import Keyboard exposing (KeyCode)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import Task exposing (Task)
import Time
import AnimationFrame
import Window
import Drag


-- MODEL


type alias Model a =
    { a
        | keys : Keys
        , window : { size : Window.Size, dt : Float }
        , person : Person
        , clock : Time.Time
        , dragModel : Drag.Model
    }


type alias Person =
    { position : Vec3
    , velocity : Vec3
    , gaze : Vec3
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , shift : Bool
    }


type Action
    = KeyChange ( Bool, KeyCode )
    | Animate Time.Time
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
    { position = vec3 0 0 eyeLevel
    , velocity = vec3 0 0 0
    , gaze = i
    }



-- INIT
-- init : ( Model, Cmd Action )


init : ( Model {}, Cmd Action )
init =
    ( { person = defaultPerson
      , keys = Keys False False False False False
      , window = { size = Window.Size 0 0, dt = 0 }
      , dragModel = Drag.initialModel
      , clock = 0
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )



-- UPDATE


update : Action -> Model a -> ( Model a, Cmd Action )
update action model =
    case action of
        KeyChange msg ->
            { model | keys = updateKeys msg model.keys } ! []

        Resize size ->
            let
                window =
                    model.window
            in
                { model | window = { window | size = size } } ! []

        Animate dt ->
            let
                window =
                    model.window
            in
                { model
                    | person =
                        model.person
                            |> move (directions model.keys)
                            |> gravity (dt / 500)
                            |> physics (dt / 500)
                    , window = { window | dt = dt }
                    , clock = model.clock + dt
                }
                    ! []

        Drag ( x, y ) ->
            { model
                | person =
                    model.person
                        |> turn { dx = toFloat x * -gazeSpeed, dy = toFloat y * -gazeSpeed }
            }
                ! []

        DragMsg msg ->
            let
                ( newDragModel, dragCmd ) =
                    Drag.update Drag msg model.dragModel
            in
                ( { model | dragModel = newDragModel }, dragCmd )



-- OTHER


subscriptions : Model a -> Sub Action
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
    KeyChange ( on, keyCode )


updateKeys : ( Bool, Keyboard.KeyCode ) -> Keys -> Keys
updateKeys ( on, keyCode ) k =
    case keyCode of
        16 ->
            { k | shift = on }

        37 ->
            { k | left = on }

        39 ->
            { k | right = on }

        38 ->
            { k | up = on }

        40 ->
            { k | down = on }

        _ ->
            k


directions : Keys -> { x : Int, y : Int, z : Int }
directions { left, right, up, down, shift } =
    let
        direction a b =
            case ( a, b ) of
                ( True, False ) ->
                    -2

                ( False, True ) ->
                    2

                _ ->
                    0

        directionUp =
            if shift then
                1
            else
                0
    in
        { x = direction down up
        , y = direction right left
        , z = directionUp
        }


turn : { dx : Float, dy : Float } -> Person -> Person
turn { dx, dy } person =
    let
        gaze =
            person.gaze

        sideways =
            V3.cross gaze k |> V3.normalize

        r =
            makeRotate dx V3.k |> M4.rotate dy sideways
    in
        { person | gaze = transform r gaze |> V3.normalize }


move : { x : Int, y : Int, z : Int } -> Person -> Person
move directions person =
    if getZ person.position > eyeLevel then
        person
    else
        let
            -- ridiculous axis alignment
            dKey =
                vec3 (toFloat directions.x) (toFloat directions.y) 0

            flatGaze =
                vec3 (V3.getX person.gaze) (V3.getY person.gaze) 0

            theta =
                if (V3.dot flatGaze V3.j) > 0 then
                    angleBetween flatGaze V3.i
                else
                    -(angleBetween flatGaze V3.i)

            dWorld =
                M4.transform (M4.makeRotate theta V3.k) dKey
        in
            { person
                | velocity = vec3 (getX dWorld) (getY dWorld) (getZ person.velocity)
            }
                |> jump directions.z


jump : Int -> Person -> Person
jump z person =
    if (z == 0) || getZ person.position > eyeLevel then
        person
    else
        let
            ( vx, vy, _ ) =
                toTuple person.velocity
        in
            { person
                | velocity = vec3 vx vy 2
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
                if z < eyeLevel then
                    vec3 x y eyeLevel
                else
                    position
        }


gravity : Float -> Person -> Person
gravity dt person =
    if getZ person.position <= eyeLevel then
        person
    else
        let
            v =
                toRecord person.velocity
        in
            { person
                | velocity = vec3 v.x v.y (v.z - 2 * dt)
            }


angleBetween : Vec3 -> Vec3 -> Float
angleBetween a b =
    dot (normalize a) (normalize b)
        |> Basics.acos



-- VIEW


perspective : ( Int, Int ) -> Person -> Mat4
perspective ( w, h ) person =
    mul (makePerspective 90 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (add person.position person.gaze) k)
