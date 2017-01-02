port module Minimum exposing (..)

-- adapted from elm-community/webgl crate example

import Keyboard exposing (KeyCode)
import Math.Vector3 as V3 exposing (..)
import Math.Matrix4 as M4 exposing (..)
import Task exposing (Task)
import Time
import AnimationFrame
import Window
import Drag


-- MODEL


type alias Model a =
    { a
        | keys : Keys
        , gamepad : Gamepad
        , window : Window.Size
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
    | GamepadChange (Maybe GamepadRaw)
    | Animate Time.Time
    | Resize Window.Size
    | DragMsg Drag.Msg
    | Drag ( Int, Int )
    | ButtonChange ( Bool, String )


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


defaultGamepad : Gamepad
defaultGamepad =
    Gamepad 0 0 0 0 defaultButton defaultButton defaultButton defaultButton defaultButton defaultButton



-- INIT
-- init : ( Model, Cmd Action )


init : ( Model {}, Cmd Action )
init =
    ( { person = defaultPerson
      , keys = Keys False False False False False
      , gamepad = defaultGamepad
      , window = Window.Size 0 0
      , dragModel = Drag.initialModel
      , clock = 0
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )



-- UPDATE


mapActions : Model a -> List Action -> ( Model a, Cmd Action )
mapActions model actions =
    let
        fold a ( m, a1 ) =
            let
                ( m2, a2 ) =
                    update a m
            in
                ( m2, Cmd.batch [ a1, a2 ] )
    in
        List.foldl fold (model ! []) actions


update : Action -> Model a -> ( Model a, Cmd Action )
update action model =
    case action of
        KeyChange msg ->
            { model | keys = updateKeys msg model.keys } ! []

        GamepadChange change ->
            case change of
                Just gamepadRaw ->
                    { model | gamepad = gamepadRaw |> broil } ! []

                Nothing ->
                    model ! []

        ButtonChange ( state, name ) ->
            model ! []

        Resize size ->
            { model | window = size } ! []

        Animate dt ->
            { model
                | person =
                    model.person
                        |> move (directions model.keys model.gamepad)
                        |> turn (gamepadLook model.gamepad)
                        |> gravity (dt / 500)
                        |> physics (dt / 500)
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



-- GAMEPAD


port gamepad : (Maybe GamepadRaw -> msg) -> Sub msg


port buttonChange : (( Bool, String ) -> msg) -> Sub msg


toButton : String -> Maybe Button
toButton name =
    case name of
        "A" ->
            Just A

        "B" ->
            Just B

        "X" ->
            Just X

        "Y" ->
            Just Y

        "LT" ->
            Just LT

        "RT" ->
            Just RT

        _ ->
            Nothing


type Button
    = A
    | B
    | X
    | Y
    | LT
    | RT


type alias ButtonInfo =
    { pressed : Bool, value : Float }


type alias GamepadRaw =
    { axes : List Float, buttons : List ButtonInfo }


type alias Gamepad =
    { up1 : Float
    , right1 : Float
    , up2 : Float
    , right2 : Float
    , a : ButtonInfo
    , b : ButtonInfo
    , x : ButtonInfo
    , y : ButtonInfo
    , lt : ButtonInfo
    , rt : ButtonInfo
    }


buttonActions : List ( Gamepad -> ButtonInfo, Button )
buttonActions =
    [ ( .a, A ), ( .b, B ), ( .x, X ), ( .y, Y ), ( .lt, LT ), ( .rt, RT ) ]


defaultButton : ButtonInfo
defaultButton =
    { pressed = False, value = 0 }


broil : GamepadRaw -> Gamepad
broil gamepadRaw =
    let
        index n xs =
            xs |> List.drop n |> List.head

        getAxis n =
            gamepadRaw.axes |> index n |> Maybe.withDefault 0 |> outerClamp 0.2

        getButton n =
            gamepadRaw.buttons |> index n |> Maybe.withDefault defaultButton |> outerClampButton 0.2

        up1 =
            getAxis 1

        right1 =
            getAxis 0

        up2 =
            getAxis 2

        right2 =
            getAxis 3

        a =
            getButton 0

        b =
            getButton 1

        x =
            getButton 2

        y =
            getButton 3

        lt =
            getButton 6

        rt =
            getButton 7

        outerClamp r x =
            if (x < -r) || (x > r) then
                x
            else
                0

        outerClampButton r b =
            { b | value = outerClamp r b.value }
    in
        { up1 = up1, right1 = right1, up2 = up2, right2 = right2, a = a, b = b, x = x, y = y, lt = lt, rt = rt }


gamepadLook : Gamepad -> { dx : Float, dy : Float }
gamepadLook gamepad =
    { dx = gamepad.up2 * -gazeSpeed * 5
    , dy = gamepad.right2 * -gazeSpeed * 5
    }



-- OTHER


subscriptions : Model a -> Sub Action
subscriptions model =
    [ AnimationFrame.diffs Animate
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Window.resizes Resize
    , Drag.subscriptions DragMsg model.dragModel
    , gamepad GamepadChange
    , buttonChange ButtonChange
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


directions : Keys -> Gamepad -> { x : Float, y : Float, z : Float }
directions { left, right, up, down, shift } { right1, up1, a } =
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
            if (shift || a.pressed) then
                1
            else
                0
    in
        { x = (direction down up) - 2 * up1
        , y = (direction right left) - 2 * right1
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


move : { x : Float, y : Float, z : Float } -> Person -> Person
move directions person =
    if getZ person.position > eyeLevel then
        person
    else
        let
            -- ridiculous axis alignment
            dKey =
                vec3 (directions.x) (directions.y) 0

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


jump : Float -> Person -> Person
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


foldla : a -> List (a -> a) -> a
foldla acc fs =
    List.foldl (\f acc -> f acc) acc fs



-- VIEW


perspective : ( Int, Int ) -> Person -> Mat4
perspective ( w, h ) person =
    mul (makePerspective 90 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (add person.position person.gaze) k)
