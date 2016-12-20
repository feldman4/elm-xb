-- adapted from elm-community/webgl crate example


module Main exposing (..)

import Keyboard exposing (KeyCode)
import Math.Vector3 exposing (..)
import Math.Vector3 as V3
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import Task exposing (Task)
import Time exposing (..)
import WebGL exposing (..)
import Html exposing (Html, text, div, br)
import Html
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Window
import PTree
import Drag
import Typewriter.Typewriter as Speech
import CFG
import Random.Pcg as Random
import Interaction exposing (Interaction)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    { keys : Keys
    , size : Window.Size
    , person : Person
    , dragModel : Drag.Model
    , forest : List (PTree.TreeBase PTree.SpringyQuad)
    , speechModel : Speech.Model
    , seed : Random.Seed
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
    | Animate Time
    | Resize Window.Size
    | DragMsg Drag.Msg
    | Drag ( Int, Int )
    | SpeechMsg Speech.Msg
    | Interact Interaction.Interaction


eyeLevel : Float
eyeLevel =
    2


gazeSpeed : Float
gazeSpeed =
    1 / 200


defaultPerson : Person
defaultPerson =
    { position = vec3 0 eyeLevel -15
    , velocity = vec3 0 0 0
    , gaze = k
    }



-- INIT


init : ( Model, Cmd Action )
init =
    ( { person = defaultPerson
      , keys = Keys False False False False False
      , size = Window.Size 0 0
      , dragModel = Drag.initialModel
      , speechModel = Speech.init CFG.exampleCFG |> Tuple.first
      , forest = PTree.exampleSpringyForest
      , seed = Random.initialSeed 22783283
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )



-- UPDATE


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        KeyChange msg ->
            { model | keys = updateKeys msg model.keys } ! []

        Resize size ->
            { model | size = size } ! []

        Animate dt ->
            { model
                | person =
                    model.person
                        |> move (directions model.keys)
                        |> gravity (dt / 500)
                        |> physics (dt / 500)
                , forest =
                    PTree.animateForest dt model.forest
            }
                ! []

        Interact it ->
            (model ! [])
                |> Interaction.speechToTree it

        Drag ( x, y ) ->
            { model
                | person =
                    model.person
                        |> turn { dx = toFloat x * -gazeSpeed, dy = toFloat y * -gazeSpeed }
            }
                ! []

        SpeechMsg msg ->
            updateSpeech msg model

        DragMsg msg ->
            let
                ( newDragModel, dragCmd ) =
                    Drag.update Drag msg model.dragModel
            in
                ( { model | dragModel = newDragModel }, dragCmd )


updateSpeech : Speech.Msg -> Model -> ( Model, Cmd Action )
updateSpeech msg model =
    let
        ( newSpeechModel, speechCmd, speechEvent ) =
            Speech.update msg model.speechModel

        newModel =
            { model | speechModel = newSpeechModel }

        ( newModel2, interactCmd ) =
            update (Interact (Interaction.SpeechEvent speechEvent)) newModel
    in
        ( newModel2
        , Cmd.batch
            [ Cmd.map SpeechMsg speechCmd
            , interactCmd
            ]
        )



-- OTHER


subscriptions : Model -> Sub Action
subscriptions model =
    [ AnimationFrame.diffs Animate
    , Keyboard.downs (keyChange True)
    , Keyboard.ups (keyChange False)
    , Window.resizes Resize
    , Drag.subscriptions DragMsg model.dragModel
    , Sub.map SpeechMsg (Speech.subscriptions model.speechModel)
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
        { x = direction left right
        , y = direction down up
        , z = directionUp
        }


turn : { dx : Float, dy : Float } -> Person -> Person
turn { dx, dy } person =
    let
        gaze =
            person.gaze

        sideways =
            V3.cross gaze j |> V3.normalize

        r =
            makeRotate dx V3.j |> M4.rotate dy sideways
    in
        { person | gaze = transform r gaze |> V3.normalize }


move : { x : Int, y : Int, z : Int } -> Person -> Person
move directions person =
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
                |> jump directions.z


jump : Int -> Person -> Person
jump y person =
    if (y == 0) || getY person.position > eyeLevel then
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



-- VIEW


perspective : ( Int, Int ) -> Person -> Mat4
perspective ( w, h ) person =
    mul (makePerspective 90 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (add person.position person.gaze) j)


view : Model -> Html Action
view { size, person, forest, speechModel } =
    let
        perspectiveMatrix =
            perspective ( size.width, size.height ) person

        noSpringForest =
            List.map PTree.despring forest

        entities =
            PTree.worldForest noSpringForest perspectiveMatrix

        messageText =
            List.map text message
                |> List.intersperse (br [] [])
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
                messageText
            , viewSpeech speechModel
            ]


viewSpeech : Speech.Model -> Html a
viewSpeech model =
    let
        suggestions =
            model.writer.choices

        input =
            model.writer.input

        phrase =
            model.phrase

        topText =
            String.join " " (phrase ++ [ input ])

        bottomText =
            String.join ", " suggestions
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "font-family", "monospace" )
                , ( "text-align", "center" )
                , ( "left", "20px" )
                , ( "right", "20px" )
                , ( "bottom", "20px" )
                ]
            ]
            [ div [] [ text topText ], div [] [ text bottomText ] ]


message : List String
message =
    [ "Walk around with a first person perspective.\n"
        ++ "Arrows keys to move, shift to jump.\n"
        ++ "Click and drag mouse to look.\n"
    , "Type and hit space bar to complete insults.\n"
        ++ "Backspace deletes letters and words."
    ]



-- mesh
