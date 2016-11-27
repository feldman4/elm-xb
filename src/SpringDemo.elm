module SpringDemo exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program, Html)
import Html.Attributes exposing (id, class)
import Spring
import AnimationFrame
import Time exposing (Time)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { spring : Spring.Spring Float
    , time : Time.Time
    }


init : ( Model, Cmd Msg )
init =
    let
        initSpring =
            { stiffness = 1
            , damping = 0
            , position = 3
            , velocity = 0
            , destination = 1
            }
    in
        { spring = initSpring, time = 0 } ! []



-- UPDATE


type Msg
    = NoOp
    | Animate Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate time ->
            let
                newSpring =
                    Spring.animate time model.spring
            in
                { model | spring = newSpring } ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    let
        position =
            "Position:" ++ (toString model.spring.position)

        velocity =
            "Velocity:" ++ (toString model.spring.velocity)

        time =
            "Time:" ++ (toString model.time)
    in
        div [ id "field", class "field" ]
            [ text position
            , text velocity
            , text time
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ AnimationFrame.diffs Animate
    ]
        |> Sub.batch
