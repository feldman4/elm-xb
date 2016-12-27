port module Island.Island exposing (..)

import Html exposing (text, div, br)
import Html.Attributes exposing (style, height, width)
import WebGL exposing (Renderable, Shader, Texture, Error)
import Minimum
import Island.Types exposing (..)
import Island.Render exposing (..)
import Island.Things exposing (..)
import Island.Effects exposing (applyEffects, applyInteractions)
import Frame
import Task
import EveryDict


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = (\a m -> updater a m)
        }


updater : Action -> Model -> ( Model, Cmd Action )
updater a m =
    let
        ( m2, a2 ) =
            update a m

        m3 =
            applyInteractions a m2
    in
        ( m3, a2 )


init : ( Model, Cmd Action )
init =
    let
        objects =
            [ initAvatar, initOcean, face0, face1, initSeaSphere, initLightCube, initBoat ]

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
        , interactions = [ Select, Follow (Orbital 0) ]
        , person = model.person
        , keys = model.keys
        , gamepad = model.gamepad
        , window = model.window
        , clock = model.clock
        , dragModel = model.dragModel
        , textures = EveryDict.empty
        , camera = Frame.identity
        }
            ! ([ action ] ++ textureActions)


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        MinAction act ->
            let
                ( newModel, minAction ) =
                    Minimum.update act model

                newModel2 =
                    case act of
                        Minimum.Animate dt ->
                            -- unleash the beast
                            { newModel
                                | objects =
                                    model.objects
                                        |> List.map (applyEffects model dt)
                            }

                        _ ->
                            newModel
            in
                ( newModel2
                , Cmd.batch
                    [ Cmd.map MinAction minAction
                    ]
                )

        TextureError err ->
            model ! []

        TextureLoaded ( name, texture ) ->
            { model | textures = EveryDict.insert name texture model.textures } ! []

        WaterIndicator heights ->
            -- { model | objects = model.objects |> List.map (setWaterFrame heights) } ! []
            model ! []


port waterIndicator : (( ( Float, Float ), List Float ) -> msg) -> Sub msg


port askWaterIndicator : ( Int, Int ) -> Cmd msg


subscriptions : Model -> Sub Action
subscriptions model =
    [ Sub.map MinAction (Minimum.subscriptions model)
    , waterIndicator WaterIndicator
    ]
        |> Sub.batch


view : Model -> Html.Html Action
view model =
    let
        window =
            model.window

        entities =
            (model.objects
                |> List.filterMap renderMaybe
                |> List.map (renderObject model)
            )
                ++ (textureEntity model)

        message =
            [ "Enjoy your stay on the island." ]

        messageText =
            List.map text message
                |> List.intersperse (br [] [])
    in
        div
            [ style
                [ ( "width", toString window.width ++ "px" )
                , ( "height", toString window.height ++ "px" )
                , ( "position", "relative" )
                ]
            ]
            [ WebGL.toHtml
                [ width window.width
                , height window.height
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
