port module Island.Island exposing (..)

import Html exposing (text, div, br)
import Html.Attributes exposing (style, height, width)
import WebGL exposing (Renderable, Shader, Texture, Error)
import Minimum
import Island.Types exposing (..)
import Island.Render exposing (..)
import Island.Things exposing (..)
import Island.Effects exposing (applyEffects, applyInteractions, updateFloating)
import Frame
import EveryDict


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = (\a m -> updater a m)
        }


{-| Should interactions always be applied after Action? Can have two kinds if
necessary.
-}
updater : Action -> Model -> ( Model, Cmd Action )
updater a m =
    let
        ( m2, a2 ) =
            update a m

        ( m3, a3 ) =
            applyInteractions a m2
    in
        ( m3, Cmd.batch [ a2, a3 ] )


updaterPre : Action -> Model -> ( Model, Cmd Action )
updaterPre a m =
    let
        ( m2, a2 ) =
            applyInteractions a m

        ( m3, a3 ) =
            update a m2
    in
        ( m3, Cmd.batch [ a2, a3 ] )


init : ( Model, Cmd Action )
init =
    let
        interactions =
            [ Select
            , Follow (Orbital 0)
            , RequestOcean
              -- , DetectCollisionsGJK
            , PauseExcept NoEffects
            ]

        objects =
            [ initAvatar, initOcean, face0, face1, initLightCube, initBoat, initIsland ]

        textureActions =
            [ Crate, Thwomp, NormalMap, DisplacementMap ]
                |> List.map textureAction

        ( model, msg ) =
            Minimum.init

        action =
            Cmd.map MinAction msg
    in
        { objects = objects
        , interactions = interactions
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

        WaterIndicator ( coordinates, heights ) ->
            let
                ( a, b, c, d ) =
                    heights

                height =
                    b

                -- (a ^ 2 + b ^ 2 + c ^ 2 + d ^ 2) ^ (0.5)
            in
                { model
                    | objects =
                        model.objects
                            |> List.map (updateFloating model coordinates height)
                }
                    ! []


port waterIndicator : (( String, ( Float, Float, Float, Float ) ) -> msg) -> Sub msg


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
