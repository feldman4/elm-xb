module CFG_demo exposing (..)

import TypewriterPure as TP
import CFG
import Html
import Html exposing (div, text, ul)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { grammar : CFG.Grammar
    , writer : TP.Model
    , history : List (List String)
    , phrase : List String
    }


type Msg
    = UpdateWriter TP.Msg
    | AddWord String
    | AddSentence (List String)
    | DeleteWord


init : ( Model, Cmd Msg )
init =
    let
        grammar =
            CFG.exampleCFG
    in
        { grammar = CFG.exampleCFG
        , writer = initWriter grammar
        , history = [ [] ]
        , phrase = []
        }
            ! []


initWriter : CFG.Grammar -> TP.Model
initWriter grammar =
    TP.init (CFG.choices grammar CFG.S []) |> Tuple.first


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWriter msg ->
            let
                ( newWriter, newWriterMsg ) =
                    TP.update msg model.writer

                complete =
                    TP.choiceComplete newWriter

                newModel =
                    { model | writer = newWriter }
            in
                if complete then
                    let
                        nextWord =
                            TP.choice newWriter
                    in
                        case nextWord of
                            Just word ->
                                update (AddWord word) model

                            Nothing ->
                                -- sentence complete
                                update (AddSentence model.phrase) model
                else if
                    (newWriter.lastKey == (.backspace TP.keycode))
                        && (newWriter.input == "")
                then
                    update DeleteWord newModel
                else
                    { model | writer = newWriter } ! []

        DeleteWord ->
            let
                newPhrase =
                    model.phrase
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse

                newChoices =
                    newPhrase
                        |> List.map CFG.Terminal
                        |> CFG.choices model.grammar CFG.S

                writer =
                    model.writer

                newModel =
                    { model
                        | phrase = newPhrase
                        , writer = { writer | choices = newChoices, input = "" }
                    }
            in
                newModel ! []

        AddWord word ->
            let
                newModel =
                    { model | phrase = model.phrase ++ [ word ] }

                writer =
                    model.writer

                a =
                    Debug.log "choices" choices

                -- choices based on updated phrase
                choices =
                    newModel.phrase
                        |> List.map CFG.Terminal
                        |> CFG.choices model.grammar CFG.S
            in
                -- if there are no more choices, then add sentence
                if List.isEmpty choices then
                    update (AddSentence newModel.phrase) newModel
                else
                    { newModel
                        | writer =
                            { writer
                                | choices = choices
                                , input = ""
                            }
                    }
                        ! []

        AddSentence sentence ->
            { model
                | writer = initWriter model.grammar
                , history = model.history ++ [ sentence ]
                , phrase = []
            }
                ! []


view : Model -> Html.Html Msg
view model =
    let
        history =
            model.history
                |> List.map (String.join " ")

        historyList =
            List.map (\x -> ul [] [ text x ]) history

        phrase =
            div [] [ text (String.join " " model.phrase) ]

        writerView =
            Html.map UpdateWriter (TP.view model.writer)
    in
        div []
            (historyList ++ [ phrase ] ++ [ writerView ])


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map UpdateWriter (TP.subscriptions model.writer)
        ]
