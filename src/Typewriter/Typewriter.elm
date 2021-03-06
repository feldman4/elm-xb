module Typewriter.Typewriter exposing (..)

import Typewriter.Base as TP
import CFG
import Html
import Html exposing (div, text, ul)


main : Program Never Model Msg
main =
    let
        realUpdate a b =
            update a b |> (\( a, b, c ) -> ( a, b ))
    in
        Html.program
            { init = init CFG.exampleCFG
            , update = realUpdate
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


type Event
    = SentenceAdded


init : CFG.Grammar -> ( Model, Cmd Msg )
init grammar =
    { grammar = CFG.exampleCFG
    , writer = initWriter grammar
    , history = [ [] ]
    , phrase = []
    }
        ! []


initWriter : CFG.Grammar -> TP.Model
initWriter grammar =
    TP.init (CFG.choices grammar CFG.S []) |> Tuple.first


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Event )
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
                    ( { model | writer = newWriter }, Cmd.none, Nothing )

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
                ( newModel, Cmd.none, Nothing )

        AddWord word ->
            let
                newModel =
                    { model | phrase = model.phrase ++ [ word ] }

                writer =
                    model.writer

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
                    ( { newModel
                        | writer =
                            { writer
                                | choices = choices
                                , input = ""
                            }
                      }
                    , Cmd.none
                    , Nothing
                    )

        AddSentence sentence ->
            ( { model
                | writer = initWriter model.grammar
                , history = model.history ++ [ sentence ]
                , phrase = []
              }
            , Cmd.none
            , Just SentenceAdded
            )


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
