module Island.Island exposing (..)

import Html exposing (text, div, br)
import Html.Attributes exposing (style, height, width)
import WebGL exposing (Renderable, Shader, Texture, Error)
import Minimum
import Island.Types exposing (..)
import Island.Geometry exposing (..)
import Island.Render exposing (..)
import Island.Things exposing (..)
import Task
import EveryDict


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
        objects =
            [ initOcean, face0, face1, initBoat, initSeaSphere, initLightCube ]

        textureActions =
            [ Crate, Thwomp, NormalMap, DisplacementMap ]
                |> List.map textureAction

        ( model, msg ) =
            Minimum.init

        action =
            Cmd.map MinAction msg

        textureAction name =
            textureURL name
                |> WebGL.loadTexture
                |> Task.attempt
                    (\result ->
                        case result of
                            Err err ->
                                TextureError err

                            Ok val ->
                                TextureLoaded ( name, val )
                    )
    in
        { objects = objects
        , sea = [] |> addPQ
        , gridSea = [] |> addPQ
        , person = model.person
        , keys = model.keys
        , window = model.window
        , clock = model.clock
        , dragModel = model.dragModel
        , textures = EveryDict.empty
        }
            ! ([ action ] ++ textureActions)


type Action
    = MinAction Minimum.Action
    | TextureError Error
    | TextureLoaded ( NamedTexture, Texture )


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

        TextureLoaded ( name, texture ) ->
            { model | textures = EveryDict.insert name texture model.textures } ! []


subscriptions : Model -> Sub Action
subscriptions model =
    [ Sub.map MinAction (Minimum.subscriptions model)
    ]
        |> Sub.batch


view : Model -> Html.Html Action
view model =
    let
        window =
            model.window

        dummyFilter ( name, texture ) =
            case name of
                NormalMap ->
                    [ texture ]

                DisplacementMap ->
                    [ texture ]

                _ ->
                    []

        textureEntity =
            let
                dummyTextures =
                    EveryDict.toList model.textures
                        |> List.concatMap dummyFilter
            in
                case dummyTextures of
                    tex0 :: tex1 :: [] ->
                        [ renderDummyTextures ( tex0, tex1 ) ]

                    tex0 :: [] ->
                        [ renderDummyTextures ( tex0, tex0 ) ]

                    [] ->
                        []

                    _ ->
                        let
                            a =
                                Debug.crash "More than 2 dummy textures."
                        in
                            []

        entities =
            (model.objects
                |> List.map (renderObject model)
            )

        -- ++ textureEntity
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
